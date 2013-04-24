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

#include    "ostc3.inc"                  ; Mandatory header
#include    "strings.inc"
#include    "convert.inc"
#include	"ghostwriter.inc"
#include 	"eeprom_rs232.inc"
#include	"external_flash.inc"

        extern  write_eeprom
        extern  read_eeprom
        extern  eeprom_serial_save,eeprom_opt_backup

        global  option_reset_all        ; Reset all options to factory default.
        global  option_reset            ; Reset FSR0 option to factory default.
        global  option_save_all         ; Save options to EEPROM.
        global  option_restore_all      ; Restore options from EEPROM.
        global  option_inc              ; Increment FSR0 option.
        global  option_draw             ; STRCAT FRS0 option.

;=============================================================================
        CBLOCK  tmp+0x10                ; Reserve space for wordprocessor & convert
            opt_type
            opt_default
            opt_inc                     ; Also default+1 (string), and enum low
            opt_min                     ; also enum high
            opt_max
            opt_unit:2                  ; Multi-lingual unit text.
            opt_eeprom
            opt_backup_tbl:3
            ; Reserved to tmp+0x1F...
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
		movff	EEDATA,lo
		read_int_eeprom	.3
		movff	EEDATA,hi
		tstfsz	lo							; Total dives=0?
		bra		option_reset_all2			; No, skip resetting logbook
		tstfsz	hi							; Total dives=0?
		bra		option_reset_all2			; No, skip resetting logbook

		clrf    EEADRH                      ; Make sure to select eeprom bank 0
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
        movf    EEADRH
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

option_save:
        rcall   option_read
        incf    opt_eeprom,W            ; Should we save it ?
        btfsc   STATUS,Z                ; eeprom address is FFh ?
        return                          ; YES: nothing to do.

        movf    opt_eeprom,W            ; Compute backup address in EEPROM
        addlw   LOW(eeprom_opt_backup)  ; Add offset
        movwf   EEADR
        movlw   HIGH(eeprom_opt_backup)
        movwf   EEADRH

        movf    opt_type,W              ; Option type is string ?
        xorlw   2                       
        bz      option_save_string

        ; One byte to save to eeprom
        movff   INDF1,EEDATA
        goto    write_eeprom
        
option_save_string:
        movff   POSTINC1,EEDATA         ; Write one byte
        call    write_eeprom
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
        output_8
        clrf    INDF2               ; Make sure to close string...

		btfsc	settime_setdate		; In the Set Time or Set Date menu?
		return                      ; Yes, ignore the rest

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

;=============================================================================
; Options Tables

OPTION_UINT8    MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      1, min
        db      max, eeprom
        dw      unit
        dw      register      
        ENDM

OPTION_UINT8p2  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      2, min
        db      max, eeprom
        dw      unit
        dw      register      
        ENDM

OPTION_UINT8p3  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      3, min
        db      max, eeprom
        dw      unit
        dw      register      
        ENDM

OPTION_UINT8p10  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      .10, min
        db      max, eeprom
        dw      unit
        dw      register      
        ENDM

OPTION_ENUM8    MACRO   lbl, max, default, tValue, eeprom, register
        global  lbl
        extern  tValue
lbl:    db      1, default  ; Type1 = ENUM
        db      LOW(tValue), HIGH(tValue)
        db      max, eeprom
        dw      0                       ; No unit
        dw      register      
        ENDM

OPTION_BOOL     MACRO   lbl, default, eeprom, register
        OPTION_ENUM8    lbl, 2, default, tNo, eeprom, register
        ENDM

        
OPTION_STRING   MACRO   lbl, length, defText, eeprom, register
        global  lbl
lbl:    db      2, LOW(defText)         ; Type2 = STRING
        db      HIGH(defText), 0
        db      length, eeprom
        dw      0                       ; No unit
        dw      register
        ENDM

;=============================================================================
    ; Option table
    ; OPTION_UINT8  Label,   min,    max,    default, text-string,   EEPROM location (-1 for RAM only),   RAM location

; Manage Gas List.
        extern tPercent, tMeters, tMinutes, tGasDisabled, tbar

option_table_begin:
        extern  char_I_deco_gas_change, char_I_setpoint_change, char_I_setpoint_cbar, char_I_dil_change
;=============================================================================
; Manage Decoplaner & Dive parameters

        extern  char_I_dive_interval, char_I_bottom_time, char_I_bottom_depth
        OPTION_UINT8p10 odiveInterval,  .0, .240,   .0,     tMinutes, -1,     char_I_dive_interval
        OPTION_UINT8p2  obottomTime,    .1, .60,    .5,     tMinutes, -1,     char_I_bottom_time
        OPTION_UINT8p3  obottomDepth,   .12,.120,    .21,    tMeters,  -1,     char_I_bottom_depth

        extern  char_I_deco_model, char_I_saturation_multiplier, char_I_desaturation_multiplier
        OPTION_ENUM8    oDiveMode,      4,  0,  tDvOC,               .8,    opt_dive_mode
        OPTION_ENUM8    oDecoMode,      2,  1,  tZHL16,              .9,    char_I_deco_model
        OPTION_UINT8p10 oPPO2Max,       .120, ppo2_warning_high, .160,   0,      .10,    opt_ppO2_max
        OPTION_UINT8    oLastDeco,      .3,  .6, .3,  tMeters,      .11,    opt_last_stop
        OPTION_UINT8    oGF_low,       .10,  .100, .40,  tPercent,  .12,    opt_GF_low
        OPTION_UINT8    oGF_high,      .70,  .110, .85, tPercent,   .13,    opt_GF_high
        OPTION_UINT8p10 osatmult,      .100,  .140, .110,tPercent,  .14,    char_I_saturation_multiplier
        OPTION_UINT8p10 odesatmult,    .60,  .100,  .90,tPercent,   .15,    char_I_desaturation_multiplier
        OPTION_UINT8p10 oPPO2Min,       .16, ppo2_warning_low, .19,   0,         .16,    opt_ppO2_min
        OPTION_UINT8    oaGF_low,      .60,  .100, .60,  tPercent,  .17,    opt_aGF_low
        OPTION_UINT8    oaGF_high,     .80,  .120, .85, tPercent,   .18,    opt_aGF_high
        OPTION_BOOL     oEnable_aGF,    1,                          .19,    opt_enable_aGF              ; =1: aGF can be selected underwater

;=============================================================================
; Managing Settings
        extern          char_I_extra_time
        OPTION_UINT8    oExtraTime,     0,  .9,   5,tMinutes,   .22,    char_I_extra_time
        OPTION_ENUM8    oBrightness,    3,  0,  tEco,           .23,    opt_brightness
        OPTION_UINT8    oDiveSalinity,  0,  4, 0,  tPercent,    .24,    opt_salinity
        OPTION_ENUM8    oCCRMode,    2,  0,  tCCRModeFixedSP,   .25,    opt_ccr_mode
        extern  tDefName
        OPTION_ENUM8    oLanguage,      4,  0,  tEnglish,   .26,    opt_language
		OPTION_ENUM8    oDateFormat,    3,  1,  tDateformat,.27,    opt_dateformat
        OPTION_ENUM8    oUnits,         2,  0,  tMetric,    .28,    opt_units			; 0=Meters, 1=Feets

;=============================================================================
; Compass calibration data
        OPTION_UINT8    oCalx0,         0,.255,.0,      0,  .29,    compass_CX_f+0
        OPTION_UINT8    oCalx1,         0,.255,.0,      0,  .30,    compass_CX_f+1
        OPTION_UINT8    oCaly0,         0,.255,.0,      0,  .31,    compass_CY_f+0
        OPTION_UINT8    oCaly1,         0,.255,.0,      0,  .32,    compass_CY_f+1
        OPTION_UINT8    oCalz0,         0,.255,.0,      0,  .33,    compass_CZ_f+0
        OPTION_UINT8    oCalz1,         0,.255,.0,      0,  .34,    compass_CZ_f+1

        OPTION_ENUM8    oGas1,          3,  1,  tGasDisabled,         .35,     opt_gas_type+0; 0=Disabled, 1=First, 2=Travel, 3=Deco
        OPTION_ENUM8    oGas2,          3,  0,  tGasDisabled,         .36,     opt_gas_type+1
        OPTION_ENUM8    oGas3,          3,  0,  tGasDisabled,         .37,     opt_gas_type+2
        OPTION_ENUM8    oGas4,          3,  0,  tGasDisabled,         .38,     opt_gas_type+3
        OPTION_ENUM8    oGas5,          3,  0,  tGasDisabled,         .39,     opt_gas_type+4
        OPTION_UINT8    oGas1O2,        .21,.100,   .21,    tPercent, .40,     opt_gas_O2_ratio+0
        OPTION_UINT8    oGas1He,        .1, .100,   .0,     tPercent, .41,     opt_gas_He_ratio+0
        OPTION_UINT8    oGas2O2,        .21,.100,   .21,    tPercent, .42,     opt_gas_O2_ratio+1
        OPTION_UINT8    oGas2He,        .1, .100,   .0,     tPercent, .43,     opt_gas_He_ratio+1
        OPTION_UINT8    oGas3O2,        .21,.100,   .21,    tPercent, .44,     opt_gas_O2_ratio+2
        OPTION_UINT8    oGas3He,        .1, .100,   .0,     tPercent, .45,     opt_gas_He_ratio+2
        OPTION_UINT8    oGas4O2,        .21,.100,   .21,    tPercent, .46,     opt_gas_O2_ratio+3
        OPTION_UINT8    oGas4He,        .1, .100,   .0,     tPercent, .47,     opt_gas_He_ratio+3
        OPTION_UINT8    oGas5O2,        .21,.100,   .21,    tPercent, .48,     opt_gas_O2_ratio+4
        OPTION_UINT8    oGas5He,        .1, .100,   .0,     tPercent, .49,     opt_gas_He_ratio+4
        OPTION_UINT8    oGas1Depth,     .0, .100,   .0,     tMeters,  .50,     char_I_deco_gas_change+0
        OPTION_UINT8    oGas2Depth,     .0, .100,   .0,     tMeters,  .51,     char_I_deco_gas_change+1
        OPTION_UINT8    oGas3Depth,     .0, .100,   .0,     tMeters,  .52,     char_I_deco_gas_change+2
        OPTION_UINT8    oGas4Depth,     .0, .100,   .0,     tMeters,  .53,     char_I_deco_gas_change+3
        OPTION_UINT8    oGas5Depth,     .0, .100,   .0,     tMeters,  .54,     char_I_deco_gas_change+4
        OPTION_UINT8    oDil1O2,        .21,.100,   .21,    tPercent, .55,     opt_dil_O2_ratio+0
        OPTION_UINT8    oDil1He,        .1, .100,   .0,     tPercent, .56,     opt_dil_He_ratio+0
        OPTION_UINT8    oDil2O2,        .21,.100,   .21,    tPercent, .57,     opt_dil_O2_ratio+1
        OPTION_UINT8    oDil2He,        .1, .100,   .0,     tPercent, .58,     opt_dil_He_ratio+1
        OPTION_UINT8    oDil3O2,        .21,.100,   .21,    tPercent, .59,     opt_dil_O2_ratio+2
        OPTION_UINT8    oDil3He,        .1, .100,   .0,     tPercent, .60,     opt_dil_He_ratio+2
        OPTION_UINT8    oDil4O2,        .21,.100,   .21,    tPercent, .61,     opt_dil_O2_ratio+3
        OPTION_UINT8    oDil4He,        .1, .100,   .0,     tPercent, .62,     opt_dil_He_ratio+3
        OPTION_UINT8    oDil5O2,        .21,.100,   .21,    tPercent, .63,     opt_dil_O2_ratio+4
        OPTION_UINT8    oDil5He,        .1, .100,   .0,     tPercent, .64,     opt_dil_He_ratio+4
        OPTION_UINT8    oSetPoint1,     .20, .160,  .70,    tbar,     .65,     char_I_setpoint_cbar+0
        OPTION_UINT8    oSetPoint2,     .20, .160,  .90,    tbar,     .66,     char_I_setpoint_cbar+1
        OPTION_UINT8    oSetPoint3,     .20, .160,  .100,   tbar,     .67,     char_I_setpoint_cbar+2
        OPTION_UINT8    oSetPoint4,     .20, .160,  .120,   tbar,     .68,     char_I_setpoint_cbar+3
        OPTION_UINT8    oSetPoint5,     .20, .160,  .140,   tbar,     .69,     char_I_setpoint_cbar+4
        OPTION_UINT8    oSP1Depth,      .0, .100,   .0,     tMeters,  .70,     char_I_setpoint_change+0
        OPTION_UINT8    oSP2Depth,      .0, .100,   .0,     tMeters,  .71,     char_I_setpoint_change+1
        OPTION_UINT8    oSP3Depth,      .0, .100,   .0,     tMeters,  .72,     char_I_setpoint_change+2
        OPTION_UINT8    oSP4Depth,      .0, .100,   .0,     tMeters,  .73,     char_I_setpoint_change+3
        OPTION_UINT8    oSP5Depth,      .0, .100,   .0,     tMeters,  .74,     char_I_setpoint_change+4
        OPTION_ENUM8    oDil1,          2,  1,  tDilDisabled,         .75,     opt_dil_type+0   ; 0=Disabled, 1=First, 2=Normal
        OPTION_ENUM8    oDil2,          2,  0,  tDilDisabled,         .76,     opt_dil_type+1
        OPTION_ENUM8    oDil3,          2,  0,  tDilDisabled,         .77,     opt_dil_type+2
        OPTION_ENUM8    oDil4,          2,  0,  tDilDisabled,         .78,     opt_dil_type+3
        OPTION_ENUM8    oDil5,          2,  0,  tDilDisabled,         .79,     opt_dil_type+4
        OPTION_UINT8    oDil1Depth,     .0, .100,   .0,     tMeters,  .80,     char_I_dil_change+0
        OPTION_UINT8    oDil2Depth,     .0, .100,   .0,     tMeters,  .81,     char_I_dil_change+1
        OPTION_UINT8    oDil3Depth,     .0, .100,   .0,     tMeters,  .82,     char_I_dil_change+2
        OPTION_UINT8    oDil4Depth,     .0, .100,   .0,     tMeters,  .83,     char_I_dil_change+3
        OPTION_UINT8    oDil5Depth,     .0, .100,   .0,     tMeters,  .84,     char_I_dil_change+4
        OPTION_STRING   oName,          opt_name_length,    tDefName, .85,     opt_name
        ; opt_name from 85 to 145


; Set Time/Set Date
		OPTION_UINT8    oSetHours,		.0,	.23, .0,	0, -1,		hours
		OPTION_UINT8    oSetMinutes,	.0,	.59, .0,	0, -1,		mins
		OPTION_UINT8    oSetDay,		.1,	.31, .0,	0, -1,		day
		OPTION_UINT8    oSetMonth,		.1,	.12, .0,	0, -1,		month
		OPTION_UINT8    oSetYear,		.13,.20, .0,	0, -1,		year

option_table_end:
        END
