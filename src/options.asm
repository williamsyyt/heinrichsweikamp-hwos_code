;=============================================================================
;
;   File options.asm
;
;   Manage all options data.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-07-12 : [jDG] Creation.
;
; RATIONALS:
;
;   Provides a centralized file with 

#include    "hwos.inc"                  ; Mandatory header
#include    "strings.inc"
#include    "convert.inc"
#include	"ghostwriter.inc"
#include 	"eeprom_rs232.inc"
#include	"external_flash.inc"
#include    "wait.inc"

        extern  write_eeprom
        extern  read_eeprom
        extern  eeprom_serial_save,eeprom_opt_backup
        extern  option_table_begin,option_table_end

        global  option_reset_all        ; Reset all options to factory default.
        global  option_check_all        ; Check all option and reset option if out of min/max boundary
        global  option_reset            ; Reset FSR0 option to factory default.
        global  option_save_all         ; Save options to EEPROM.
        global  option_restore_all      ; Restore options from EEPROM.
        global  option_inc              ; Increment FSR0 option.
        global  option_draw             ; STRCAT FRS0 option.


;=============================================================================
        CBLOCK  tmp+0x12                ; Reserve space for wordprocessor & convert
            opt_type
            opt_default
            opt_inc                     ; Also default+1 (string), and enum low
            opt_min                     ; also enum high
            opt_max
            opt_unit:2                  ; Multi-lingual unit text.
            opt_eeprom
            opt_backup_tbl:3
            ; Reserved to tmp+0x1C...
        ENDC

gui     CODE                            ; Appends to other GUI segment
;=============================================================================
; Reset all options to factory defauts.
;
; INPUT:  none
; OUTPUT: none
; TRASH:  TBLPTR, TABLAT, WREG, FSR0, FSR1, FSR2

option_reset_all:
	    clrf	EEADRH
		read_int_eeprom	.2
		tstfsz	EEDATA                      ; Total dives=0?
		bra		option_reset_all2			; No, skip resetting logbook
		read_int_eeprom	.3
		tstfsz	EEDATA                      ; Total dives=0?
		bra		option_reset_all2			; No, skip resetting logbook

		clrf	EEDATA
		write_int_eeprom	.4
		write_int_eeprom	.5
		write_int_eeprom	.6
		write_int_eeprom	.2				; Also, delete total dive counter
		write_int_eeprom	.3				
		call	ext_flash_erase_logbook		; And complete logbook (!)

option_reset_all2:
		clrf	lo
		clrf	hi
		call	do_logoffset_common_write	; reset Logbook offset

        ; Point to option table begin
        movlw   LOW(option_table_begin)
        movwf   FSR0L
        movlw   HIGH(option_table_begin)
        movwf   FSR0H
option_reset_all_1:
        movf    FSR0L,W                 ; Reached end of table ?
        xorlw   LOW(option_table_end)   ; (8bit test -> 10 bytes x 128 options max)
        btfsc   STATUS,Z                ; YES: done.
        return

        rcall   option_reset            ; Reset one option.
        bra     option_reset_all_1      ; and loop.

;=============================================================================
; Check all option and reset option if out of min/max boundary
;
; INPUT:  none
; OUTPUT: none
; TRASH:  TBLPTR, TABLAT, WREG, FSR0, FSR1, FSR2
option_check_all:
        ; Point to option table begin
        movlw   LOW(option_table_begin)
        movwf   FSR0L
        movlw   HIGH(option_table_begin)
        movwf   FSR0H

option_check_all_1:
        movf    FSR0L,W                 ; Reached end of table ?
        xorlw   LOW(option_table_end)   ; (8bit test -> 10 bytes x 128 options max)
        btfsc   STATUS,Z                ; YES: done.
        return

        rcall   option_check            ; check one option.
        bra     option_check_all_1      ; and loop

;=============================================================================
; Read option handle
; INPUT:  FSR0 = option handle
; OUTPUT: FSR1 = address of variable.
; TRASH:  TBLPTR, TABLAT, WREG, FSR0, FSR1
option_read:
        movff   FSR0L,TBLPTRL
        movlw   HIGH(option_table_begin); Get 8 high bits.
        andlw   0xF0                    ; Keep only the 4 highest ones.
        iorwf   FSR0H,W                 ; Cat with the known 4 lower ones.
        movwf   TBLPTRH                 ; And we have the high byte.
        movlw   UPPER(option_table_begin)
        movwf   TBLPTRU

        ; Read type, default and register from table
        tblrd*+
        movff   TABLAT,opt_type
        tblrd*+
        movff   TABLAT,opt_default
        tblrd*+
        movff   TABLAT,opt_inc
        tblrd*+
        movff   TABLAT,opt_min
        tblrd*+
        movff   TABLAT,opt_max
        tblrd*+
        movff   TABLAT,opt_eeprom
        tblrd*+
        movff   TABLAT,opt_unit+0
        tblrd*+
        movff   TABLAT,opt_unit+1
        tblrd*+
        movff   TABLAT,FSR1L
        tblrd*+
        movff   TABLAT,FSR1H
        movff   TBLPTRL,FSR0L           ; Advance handle too, for reset_all
        movff   TBLPTRH,FSR0H
        return

;=============================================================================
; Check one option and reset if it's out of it's min/max boundaries
; INPUT:  FSR0 = option handle
; OUTPUT: none
; TRASH:  TBLPTR, TABLAT, WREG, FSR1, FSR2, lo
;
option_check:
        ; Read type, default and register from table
        rcall   option_read

        ; Switch on type
        movf    opt_type,W              ; Type == STRING ?
        xorlw   2
        bz      option_check_string     ; String: Do not reset strings

        movf    opt_type,W              ; Type == ENUM8 ?
        xorlw   1
        bz      option_check_enum8      ; ENUM8: Check if lower then max. value only

        tstfsz  opt_min                 ; opt_min=0?
        bra     option_check_both       ; no
        bra     option_check_enum8      ; Check max only

option_check_both:
        decf    opt_min,W
        cpfsgt  INDF1                   ; bigger then opt_min-1?
        bra     option_check_reset      ; No, reset option
option_check_enum8:                     ; ENUM8: Check max only
        infsnz  opt_max,W               ; Max = 255?
        return                          ; Yes, igonore max. test
        cpfslt  INDF1                   ; smaller then opt_max+1?
        bra     option_check_reset      ; No, reset option
        return                          ; in range, return

option_check_reset:
        movff   opt_default,INDF1       ; reset option to default
        return                          ; Done.

option_check_string:
        return

;=============================================================================
; Reset an option to its default value.
; INPUT:  FSR0 = option handle
; OUTPUT: none
; TRASH:  TBLPTR, TABLAT, WREG, FSR1, FSR2
;
option_reset:
        ; Read type, default and register from table
        rcall   option_read

        ; Switch on type
        movf    opt_type,W              ; Type == STRING ?
        xorlw   2
        bz      opt_reset_string        ; YES: special copy
        
        movff   opt_default,INDF1       ; NO: just a 8bit indirect copy
        return

opt_reset_string:
        movff   FSR1L,FSR2L             ; set string destination address.
        movff   FSR1H,FSR2H

        movff   opt_default+0,FSR1L     ; Get handle to multi-lingual text in FSR1
        movff   opt_default+1,FSR1H

        movff   TBLPTRL,opt_backup_tbl+0; TBLPTR trashed by text routine...
        movff   TBLPTRH,opt_backup_tbl+1
        movff   TBLPTRU,opt_backup_tbl+2

        call    strcat_text             ; Copy translated text to FSR2

        movff   opt_backup_tbl+0,TBLPTRL
        movff   opt_backup_tbl+1,TBLPTRH
        movff   opt_backup_tbl+2,TBLPTRU

        return

;=============================================================================
; Save all options to EEPROM
option_save_all:
        ;---- Save option serial into EEPROM to detect reset and new version
        movlw   LOW(eeprom_serial_save)
        movwf   EEADR
        movlw   HIGH(eeprom_serial_save)
        movwf   EEADRH
        movlw   LOW(eeprom_opt_serial)
        movwf   EEDATA
        call    write_eeprom
        incf    EEADR,F
        movlw   HIGH(eeprom_opt_serial)
        movwf   EEDATA
        call    write_eeprom
        
        ;---- Save all options
        movlw   LOW(option_table_begin)
        movwf   FSR0L
        movlw   HIGH(option_table_begin)
        movwf   FSR0H

option_save_all_1:
        movf    FSR0L,W                 ; Reached end of table ?
        xorlw   LOW(option_table_end)   ; (8bit test -> 10 bytes x 128 options max)
        btfsc   STATUS,Z                ; YES: done.
        return

        rcall   option_save             ; Save one option.
        bra     option_save_all_1       ; and loop.

        global  option_save
option_save:
        rcall   option_read
        incf    opt_eeprom,W            ; Should we save it ?
        btfsc   STATUS,Z                ; eeprom address is FFh ?
        return                          ; YES: nothing to do.

        movf    opt_eeprom,W            ; Compute backup address in EEPROM
        addlw   LOW(eeprom_opt_backup)  ; Add offset
        movwf   EEADR
        movlw   HIGH(eeprom_opt_backup)
        btfsc   STATUS,C                ; >256
        addlw   .1                      ; Yes: +1
        movwf   EEADRH

        movf    opt_type,W              ; Option type is string ?
        xorlw   2                       
        bz      option_save_string

        ; One byte to save to eeprom
        movff   INDF1,EEDATA
        btfss   EEADRH,1                ; EEADR:EEADRH < 512?
        call    write_eeprom            ; Yes, write
        return
        
option_save_string:
        movff   POSTINC1,EEDATA         ; Write one byte
        btfss   EEADRH,1                ; EEADR:EEADRH < 512?
        call    write_eeprom            ; Yes, write
        infsnz  EEADR,F
        incf    EEADRH,F
        
        decfsz  opt_max                 ; Decrement string length
        bra     option_save_string      ; And loop while not finished

        return
;=============================================================================

option_restore_all:
        ;---- Read option serial from EEPROM
        movlw   LOW(eeprom_serial_save)
        movwf   EEADR
        movlw   HIGH(eeprom_serial_save)
        movf    EEADRH
        call    read_eeprom
        movlw   LOW(eeprom_opt_serial)
        xorwf   EEDATA,W
        bnz     option_restore_bad      ; Auto reset if changed.
        incf    EEADR,F
        call    read_eeprom
        movlw   HIGH(eeprom_opt_serial)
        xorwf   EEDATA,W
        bz      option_restore_ok       ; Auto reset if changed.

option_restore_bad:
        call    option_reset_all        ; Reset RAM contains
        goto    option_save_all         ; Then save to EEPROM

        ;---- Proper restore
option_restore_ok:
        movlw   LOW(option_table_begin)
        movwf   FSR0L
        movlw   HIGH(option_table_begin)
        movwf   FSR0H

option_restore_all_1:
        movf    FSR0L,W                 ; Reached end of table ?
        xorlw   LOW(option_table_end)   ; (8bit test -> 10 bytes x 128 options max)
        btfsc   STATUS,Z                ; YES: done.
        return

        rcall   option_restore          ; Restore one option.
        bra     option_restore_all_1    ; and loop.
        return

option_restore:
        rcall   option_read
        incf    opt_eeprom,W            ; Should we save it ?
        btfsc   STATUS,Z                ; eeprom address is FFh ?
        return                          ; YES: nothing to do.

        movf    opt_eeprom,W            ; Compute backup address in EEPROM
        addlw   LOW(eeprom_opt_backup)  ; Add offset
        movwf   EEADR
        movlw   HIGH(eeprom_opt_backup)
        btfsc   STATUS,C                ; >256
        addlw   .1                      ; Yes: +1
        movwf   EEADRH

        movf    opt_type,W              ; Option type is string ?
        xorlw   2                       
        bz      option_restore_string

        ; Read one byte from eeprom
        call    read_eeprom
        movff   EEDATA, INDF1           ; And restore option register.
        return

option_restore_string:
        call    read_eeprom             ; Read one byte, and
        movff   EEDATA,POSTINC1         ; restore it
        infsnz  EEADR,F
        incf    EEADRH,F
        
        decfsz  opt_max                 ; Decrement string length
        bra     option_restore_string   ; And loop while not finished
        return

;=============================================================================
; Increment an option, based on type, and boundary.
; INPUT:  FSR0 = option handle
; OUTPUT: none
; TRASH:  TBLPTR, TABLAT, WREG, FSR0, FSR1
option_inc:
        ; Read type, default and register from table
        rcall   option_read
        
        ; Switch on type
        movf    opt_type,W
        bz      option_inc_uint8
        dcfsnz  WREG
        bra     option_inc_enum8
        dcfsnz  WREG
        bra     option_inc_string

option_inc_uint8:                       ; Defaults type too...
        movf    INDF1,W
        addwf   opt_inc,W
        cpfslt  opt_max
        bra     option_inc_1
        movf    opt_min,W
option_inc_1:
        movwf   INDF1
        return

option_inc_enum8:                       ; Always +1
        incf    INDF1,W
        cpfsgt  opt_max
        clrf    WREG
        movwf   INDF1
        return

option_inc_string:                      ; No edition yet...
        return

;=============================================================================
; Strcat option into FSR2 buffer.
option_draw:
        ; Read type, default and register from table
        rcall   option_read
        
        ; Switch on type
        movf    opt_type,W
        bz      option_draw_uint8
        dcfsnz  WREG
        bra     option_draw_enum8
        dcfsnz  WREG
        bra     option_draw_string
        return                      ; Unknown: return...

option_draw_uint8:
        movff   INDF1,lo            ; Draw value.
        bsf     leftbind
        output_8
        bcf     leftbind
        clrf    INDF2               ; Make sure to close string...

        movf    opt_unit+0,W        ; Is there a unit to append ?
        iorwf   opt_unit+1,W
        rcall   option_draw_unit

        movf    opt_default,W       ; Default value
        cpfseq  lo                  ; Current value
        bra     option_draw_uint8_2 ; Not default, add *
        return                      ; Default, Done.
option_draw_uint8_2:
        PUTC    "*"
        return                      ; Done.

option_draw_unit:
        movff   opt_unit+0,FSR1L
        movff   opt_unit+1,FSR1H
        goto    strcat_text
        


;---- Draw an enumerated value (set of translated strings)
option_draw_enum8:
        movf    INDF1,W             ; Get current value.
        cpfsgt  opt_max             ; Bound value
        clrf    WREG
        addwf   WREG                ; *= 2            
        addwf   opt_inc,W           ; Base text + 2 * value
        movwf   FSR1L
        movlw   0
        addwfc  opt_min,W           ; Propagate carry
        movwf   FSR1H               ; Into FSR1

        goto    strcat_text

option_draw_string:
        movff   POSTINC1,POSTINC2
        decfsz  opt_max
        bra     option_draw_string
        return

        END
