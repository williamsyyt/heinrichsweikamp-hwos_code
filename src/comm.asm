;=============================================================================
;
;   File comm.asm
;
;   RS232 via USB
;
;   Copyright (c) 2012, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-22 : [mH] Creation
;  2012-02-11 : [jDG] Added "c" set custom text, and "i" identify.

#include "ostc3.inc"
#include "eeprom_rs232.inc"
#include "tft.inc"
#include "wait.inc"
#include "strings.inc"
#include "convert.inc"
#include "external_flash.inc"
#include "tft_outputs.inc"
#include "surfmode.inc"
#include "rtc.inc"
#include "adc_lightsensor.inc"

	extern  testloop,new_battery_menu,restart,option_reset_all
    extern  char_I_deco_gas_change, char_I_dil_change, char_I_setpoint_cbar, char_I_setpoint_change
    extern  char_I_deco_model, char_I_extra_time, char_I_saturation_multiplier, char_I_desaturation_multiplier
    extern  option_check_all, gaslist_cleanup_list, get_first_gas_to_WREG, get_first_dil_to_WREG

#DEFINE timeout_comm_pre_mode   .120        ; Pre-loop
#DEFINE timeout_comm_mode       .120        ; Download mode
#DEFINE timeout_service_mode    .120        ; Service mode

#DEFINE	comm_title_row		.0
#DEFINE	comm_title_column	.50

#DEFINE	comm_string_row	.30
#DEFINE	comm_string_column	.40


#DEFINE	comm_status1_row		.70
#DEFINE	comm_status1_column	.10
#DEFINE	comm_status2_row		.100
#DEFINE	comm_status2_column	comm_status1_column
#DEFINE	comm_status3_row		.130
#DEFINE	comm_status3_column	comm_status1_column

#DEFINE	comm_warning_row		.160
#DEFINE	comm_warning_column     .65

comm code

	; test for comm
	global	comm_mode
comm_mode:
    call    TFT_ClearScreen
    WIN_COLOR   color_greenish
	WIN_SMALL	comm_title_column, comm_title_row
	STRCPY_TEXT_PRINT	tUsbTitle		; comm Mode
    call	TFT_standard_color
    WIN_TOP     .10
    WIN_LEFT    .1
    movlw   0xDE
    movwf   TBLPTRL
    movlw   0xEE
    movwf   TBLPTRH
    movlw   0x01
    movwf   TBLPTRU
    call    color_image                 ; Show usb logo
	WIN_SMALL	comm_status1_column, comm_status1_row
	STRCPY_TEXT_PRINT	tUsbStarting	; Starting...
    call	TFT_serial                  ; Show serial and firmware version
    bcf     enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
	bcf		switch_right
    bcf     comm_service_enabled
    bsf     menubit
    bcf     battery_removed_in_usb      ; =1: The battery has been removed in USB
    movlw   timeout_comm_pre_mode
	movwf	timeout_counter
	WIN_SMALL	comm_status1_column+.80, comm_status1_row
	STRCPY_TEXT_PRINT	tUsbStartDone	; Done...
	call	enable_rs232				; Also sets to speed_normal ...
comm_mode1:
	bcf		onesecupdate
	bcf		LEDr
	dcfsnz 	timeout_counter,F
	bra		comm_service_exit           ; Timeout -> Exit
comm_mode2:
	call	get_battery_voltage			; gets battery voltage
    movlw   .3
    cpfslt  batt_voltage+1              ; Batt Voltage less then 3*256mV?
    bra     comm_mode3                  ; No
    ; Set flag
    bsf     battery_removed_in_usb      ; =1: The battery has been removed in USB
    bra     comm_mode4

comm_mode3:
    ; Voltage ok. Do we have a new battery now?
    btfsc   battery_removed_in_usb      ; =1: The battery has been removed in USB
    goto	new_battery_menu            ; show "New battery dialog"

comm_mode4:
	call	rs232_get_byte

    btfss   vusb_in                     ; USB plugged in?
    bra     comm_service_exit_nousb_delay   ; Disconnected -> Exit
comm_mode4a:

	btfsc	switch_right				; Abort with right
	bra		comm_service_exit

	btfsc	onesecupdate
	bra		comm_mode1

	movlw	0xAA						; start byte=0xAA?
	cpfseq	RCREG1
	bra		comm_mode2a
	bra		comm_mode2b             ; Startbyte for service mode found
comm_mode2a:
	movlw	0xBB						; start byte=0xBB?
	cpfseq	RCREG1
	bra		comm_mode2				; Cycle
	bra		comm_download_mode		; Startbyte for download mode found

comm_mode2b:
	; Startbyte found
	call	rs232_wait_tx			; Wait for UART
	movlw	0x4B
	movwf	TXREG1					; Send Answer
	; Now, check comm command

	call	rs232_get_byte				; first byte
	call	rs232_wait_tx               ; Wait for UART
    movff   RCREG1,TXREG1                 ; Echo
	movlw	UPPER comm_service_key
	cpfseq	RCREG1
    bra     comm_mode1               ; Wrong -> Restart
	call	rs232_get_byte				; second byte
	call	rs232_wait_tx			; Wait for UART
    movff   RCREG1,TXREG1                 ; Echo
	movlw	HIGH (comm_service_key & 0xFFFF)
	cpfseq	RCREG1
    bra     comm_mode1               ; Wrong -> Restart
	call	rs232_get_byte				; third byte
	call	rs232_wait_tx			; Wait for UART
    movff   RCREG1,TXREG1                 ; Echo
	movlw	LOW comm_service_key
	cpfseq	RCREG1
    bra     comm_mode1               ; Wrong -> Restart

	; Enable comm service mode
	WIN_SMALL	comm_status2_column, comm_status2_row
	STRCPY_TEXT_PRINT	tUsbServiceMode	; Service mode enabled
    bsf     comm_service_enabled
	bra		comm_download_mode0		; Startbyte for download mode found


comm_service_exit_nousb_delay:
    WAITMS  d'200'
    btfss   vusb_in                     ; USB plugged in?
    bra     comm_service_exit_nousb     ; Disconnected -> Exit
    bra     comm_mode4a                 ; (Still) connected, return

comm_service_exit_nousb:                ; Disconnected -> Exit
	WIN_SMALL	comm_status3_column, comm_status3_row
	STRCPY_TEXT_PRINT	tUsbClosed      ; Port closed
    bra     comm_service_exit_common

comm_service_exit:
	WIN_SMALL	comm_status3_column, comm_status3_row
	STRCPY_TEXT_PRINT	tUsbExit        ; Exited

comm_service_exit_common:
	call	rs232_wait_tx				; Wait for UART
	movlw	0xFF                        ; Reply FF
	movwf	TXREG1						; Send Answer

	; Wait 1 second
	bcf		onesecupdate
	btfss	onesecupdate
	bra		$-2
	; Wait 1 second
	bcf		onesecupdate
	btfss	onesecupdate
	bra		$-2

	call	disable_rs232
	bcf		LEDr
    goto    restart

;-----------------------------------------------------------------------------

comm_service_ll_bootloader:
    bsf     LEDr
	WIN_SMALL	comm_status3_column, comm_status3_row
	STRCPY_TEXT_PRINT	tUsbLlBld               ; Low Level Bootloader started
    WIN_TOP  	comm_warning_row
	WIN_LEFT 	comm_warning_column
    TFT_WRITE_PROM_IMAGE dive_warning_block 	; Show Warning icon
    goto    0x1FF0C

;-----------------------------------------------------------------------------
; Sends external flash from 0x3E0000 to 0x3FD000 (118784bytes) via comm
;
comm_send_firmware:
    movlw   0x50                            ; send echo
    movwf   TXREG1
    call    rs232_wait_tx                   ; Wait for UART

    ; Read 5 bytes into buffer.
	lfsr	FSR2,buffer
	movlw	.5								; counter
	movwf	lo
	movlw   0x55                            ; 5'ft byte checksum.
	movwf   hi

comm_send_firmware_loop:
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_send_firmware_abort   ; No, abort!
	movf	RCREG1,W
	movwf   POSTINC2                        ; Store checksum byte.
	xorwf   hi,F                            ; Also xor into checksum
	rlncf   hi,F                            ; And rotate it.
	decfsz	lo,F
	bra		comm_send_firmware_loop
	
	; Check that 5ft byte checksum's checksum
	movf    hi,W
	bnz     comm_send_firmware_failed

    movlw   0x4C                            ; send OK
    movwf   TXREG1
    call    rs232_wait_tx                   ; Wait for UART

	; Passed: goto second stage verification.
	; NOTE: Bootloader is Bank0. With buffer at address 0x200.
	goto    0x1FDF0                         ; And pray...

comm_send_firmware_failed:
	WIN_SMALL	comm_string_column, comm_string_row
    call    TFT_warnings_color
	STRCPY_PRINT    "Checksum failed"

comm_send_firmware_abort:

    movlw   0xFF                            ; send ABORTED byte.
	movwf	TXREG1
	call	rs232_wait_tx                   ; Wait for UART
	bra		comm_download_mode0				; Done.

;-----------------------------------------------------------------------------
; Reset to Dive 1 in logbook

comm_reset_logbook_pointers:
	clrf    EEADRH                      ; Make sure to select eeprom bank 0
	clrf	EEDATA
	write_int_eeprom	.4
	write_int_eeprom	.5
	write_int_eeprom	.6
	write_int_eeprom	.2				; Also, delete total dive counter
	write_int_eeprom	.3
    write_int_eeprom    .15
    write_int_eeprom    .16             ; And the backup counter, too
	call	ext_flash_erase_logbook		; And complete logbook (!)
	bra		comm_download_mode0			; Done.

;-----------------------------------------------------------------------------
comm_reset_battery_gauge:           ; Resets battery gauge registers
    call    reset_battery_pointer   ; Resets battery pointer 0x07-0x0C and battery_gauge:5
    bra		comm_download_mode0		; Done.

;-----------------------------------------------------------------------------
; erases range in 4kB steps

comm_erase_range4kb:
    movlw   0x42                        ; send echo
    movwf   TXREG1
    call    rs232_wait_tx               ; Wait for UART

    bcf     INTCON,GIE	; All interrups off!

    rcall   comm_get_flash_address		; Get three bytes address or return
    btfsc   rs232_recieve_overflow      ; Got Data?
    bra     comm_download_mode0         ; No, Done.

    call    rs232_get_byte
    btfsc   rs232_recieve_overflow      ; Got byte?
    bra     comm_download_mode0         ; No, Done.
    movff   RCREG1,lo
; Got 4bytes: 3bytes address and 1 bytes (lo) amount of 4kB blocks

comm_erase_range4kb1:
    call    ext_flash_erase4kB          ; Erase block!

    movlw   0x10
    addwf   ext_flash_address+1,F
    movlw   .0
    addwfc  ext_flash_address+2,F       ; Increase address by .4096, or 0x1000
    decfsz  lo,F
    bra     comm_erase_range4kb1        ; Loop until lo=zero

    bra     comm_download_mode0         ; Done (Sends the 4C OK too).

;-----------------------------------------------------------------------------

comm_erase_4kb:				; Get 3 bytes start address
	bcf		INTCON,GIE	; All interrups off!	

	rcall	comm_get_flash_address		; Get three bytes address or return
	btfsc	rs232_recieve_overflow      ; Got Data?
	bra		comm_download_mode0         ; No, Done.

	call	ext_flash_erase4kB          ; Erase one block
	bra		comm_download_mode0 		; Done.

;-----------------------------------------------------------------------------

comm_write_range:				; Get 3 bytes start address
    movlw   0x30                        ; send echo
	movwf	TXREG1
	call	rs232_wait_tx               ; Wait for UART

	bcf		INTCON,GIE                  ; All interrups off!

	rcall	comm_get_flash_address		; Get three bytes address or return
	btfsc	rs232_recieve_overflow      ; Got Data?
	bra		comm_download_mode0  		; No, Done.

comm_write_range_loop:
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow      ; Got byte?
	bra		comm_download_mode0         ; No, Done (and send OK byte too).
	movf	RCREG1,W
	call	ext_flash_byte_write        ; write one byte
	call	incf_ext_flash_address_p1   ; increase address+1
	bra		comm_write_range_loop

;-----------------------------------------------------------------------------

comm_send_range:				; Get 3 bytes start address and 3 bytes amount
    movlw   0x20                        ; send echo
    movwf   TXREG1
	call	rs232_wait_tx               ; Wait for UART

	bcf		INTCON,GIE	; All interrups off!	

	rcall	comm_get_flash_address		; Get three bytes address or return
	btfsc	rs232_recieve_overflow			; Got Data?
	bra		comm_download_mode0				; No, Done.

	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_download_mode0				; No, Done.
	movff	RCREG1,up
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_download_mode0				; No, Done.
	movff	RCREG1,hi
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_download_mode0				; No, Done.
	movff	RCREG1,lo

    ; If lo==0, we must precondition hi because there is to many bytes send !
    movf    lo,W    
    bnz     $+4
    decf    hi,F

; 6bytes received, send data
comm_send_range2:						; needs ext_flash_address:3 start address and up:hi:lo amount
	call	ext_flash_read_block_start
	movwf	TXREG1

	bra		comm_send_range24		; counter 24bit
comm_send_range24_loop:
	call	ext_flash_read_block		; Read one byte
	movwf	TXREG1						; Start new transmit
comm_send_range24:
	call	rs232_wait_tx				; Wait for UART
	decfsz	lo,F
	bra		comm_send_range24_loop
	decf	hi,F
	movlw	0xFF
	cpfseq	hi
	bra		comm_send_range24_loop
	decf	up,F
	movlw	0xFF
	cpfseq	up
	bra		comm_send_range24_loop
	call	ext_flash_read_block_stop

	bra		comm_download_mode0			; Done.

;-----------------------------------------------------------------------------

comm_get_flash_address:
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return									; No, return
	movff	RCREG1,ext_flash_address+2
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return									; No, return
	movff	RCREG1,ext_flash_address+1
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return									; No, return
	movff	RCREG1,ext_flash_address+0
	return

;-----------------------------------------------------------------------------

comm_download_mode:
	; Enable comm download mode
	WIN_SMALL	comm_status2_column, comm_status2_row
	STRCPY_TEXT_PRINT	tUsbDownloadMode; Download mode enabled
	bsf		INTCON,GIE					; All interrups on
	call	rs232_wait_tx				; Wait for UART
	movlw	0xBB                        ; Command Echo
	movwf	TXREG1						; Send Answer
comm_download_mode0:
    bsf		INTCON,GIE					; All interrups on
	call	rs232_wait_tx				; Wait for UART
    movlw   0x4C                        ; 4C in service mode
    btfss   comm_service_enabled
	movlw	0x4D                        ; 4D in download mode
	movwf	TXREG1						; Send Answer
	movlw	timeout_service_mode
	movwf	timeout_counter 			; Timeout
	bcf		switch_right
comm_download_mode1:
	bcf		onesecupdate
	dcfsnz 	timeout_counter,F
	bra		comm_service_exit           ; Timeout -> Exit
comm_download_mode2:
	call	rs232_get_byte              ; Check for a byte
    btfsc   comm_service_enabled
	btg     LEDr                        ; Blink in Service mode
    btfss   vusb_in                     ; USB plugged in?
    bra     comm_service_exit_nousb     ; Disconnected -> Exit
	btfsc	switch_right				; Abort with right
	bra		comm_service_exit
	btfsc	onesecupdate
	bra		comm_download_mode1
	btfsc	rs232_recieve_overflow
	bra		comm_download_mode2	; Wait for command byte

	; command received!
	bcf		LEDr
	movlw	0xFF
	cpfseq	RCREG1
	bra		$+4
	bra		comm_service_exit			; exit
	movlw	"a"
	cpfseq	RCREG1
	bra		$+4
	bra		comm_send_headers			; Send all 256 dive headers
	movlw	"b"
	cpfseq	RCREG1
	bra		$+4
	bra		comm_set_time				; Read time and date from the PC and set clock
	movlw	"c"
	cpfseq	RCREG1
	bra		$+4
	bra		comm_set_custom_text		; Send a opt_name_length byte string of custom text.
	movlw	"f"
	cpfseq	RCREG1
	bra		$+4
	bra		comm_send_dive				; Send header and profile for one dive
	movlw	"i"
	cpfseq	RCREG1
	bra		$+4
	bra		comm_identify               ; Send firmware, serial, etc.
	movlw	"n"
	cpfseq	RCREG1
	bra		$+4
	goto	comm_send_string			; Send a 15byte string to the screen
	movlw	"l"
	cpfseq	RCREG1
	bra		$+4
    call	TFT_dump_screen             ; Dump the screen contents
	movlw	"r"
	cpfseq	RCREG1
	bra		$+4
    bra     comm_read_setting           ; Read a setting (And send via USB)
	movlw	"w"
	cpfseq	RCREG1
	bra		$+4
    bra     comm_write_setting          ; Write a setting (Into RAM)
	movlw	"x"
	cpfseq	RCREG1
	bra		$+4
    bra     comm_option_reset_all       ; Reset all options to factory default.


    btfss   comm_service_enabled        ; Done for Download mode
	bra		comm_download_mode0         ; Loop with timeout reset

	movlw	0x20
	cpfseq	RCREG1
	bra		$+4
	bra		comm_send_range             ; send hi:lo:temp1 bytes starting from ext_flash_address:3
	movlw	0x22
	cpfseq	RCREG1
	bra		$+4
	bra		comm_reset_logbook_pointers	; Resets all logbook pointers and the logbook (!)
	movlw	0x23
	cpfseq	RCREG1
	bra		$+4
	bra		comm_reset_battery_gauge    ; Resets battery gauge registers
	movlw	0x30
	cpfseq	RCREG1
	bra		$+4
	bra		comm_write_range            ; write bytes starting from ext_flash_address:3 (Stop when timeout)
	movlw	0x40
	cpfseq	RCREG1
	bra		$+4
	bra		comm_erase_4kb              ; erases 4kB block from ext_flash_address:3 (Warning: No confirmation or built-in security here...)
	movlw	0x42
	cpfseq	RCREG1
	bra		$+4
	bra		comm_erase_range4kb         ; erases range in 4kB steps (Get 3 bytes address and 1byte amount of 4kB blocks)
	movlw	0x50
	cpfseq	RCREG1
	bra		$+4
	bra		comm_send_firmware          ; sends firmware from external flash from 0x3E0000 to 0x3FD000 (118784bytes) via comm
	movlw	"t"
	cpfseq	RCREG1
	bra		$+4
    goto    testloop                    ; Start raw-data testloop
	movlw	0xC1
	cpfseq	RCREG1
	bra		$+4
	bra 	comm_service_ll_bootloader  ; Start low-level bootloader
    bra		comm_download_mode0         ; Loop with timeout reset

;-----------------------------------------------------------------------------

comm_send_headers:
	movlw	"a"								; send echo
	movwf	TXREG1
	; Send 256 bytes/dive (Header)
	; 1st: 200000h-2000FFh
	; 2nd: 201000h-2010FFh
	; 3rd: 202000h-2020FFh
	; 100: 264000h-2640FFh
	; 256: 2FF000h-2FF0FFh
	movlw	0x1F
	movwf	ext_flash_address+2
	movlw	0xF0
	movwf	ext_flash_address+1

comm_send_headers2:
	movlw	0x00
	movwf	ext_flash_address+0
	; Adjust address for next dive
	movlw	0x10
	addwf	ext_flash_address+1
	movlw	0x00
	addwfc	ext_flash_address+2

	movlw	0x30
	cpfseq	ext_flash_address+2				; All 256 dive send?
	bra		comm_send_headers4			; No, continue
	bra		comm_download_mode0		; Done. Loop with timeout reset

comm_send_headers4:
	clrf	lo							; Counter	
	call	rs232_wait_tx				; Wait for UART
	call	ext_flash_read_block_start	; 1st byte
	movwf	TXREG1
	bra		comm_send_headers3		; counter 24bit
comm_send_headers_loop:
	call	ext_flash_read_block		; Read one byte
	movwf	TXREG1						; Start new transmit
comm_send_headers3:
	call	rs232_wait_tx				; Wait for UART
	decfsz	lo,F
	bra		comm_send_headers_loop
	call	ext_flash_read_block_stop
	bra		comm_send_headers2		; continue

;-----------------------------------------------------------------------------

comm_option_reset_all:       ; Reset all options to factory default.
	movlw	"x"								; send echo
	movwf	TXREG1
    call    option_reset_all
    bra		comm_download_mode0             ; Done. back to loop with timeout reset

;-----------------------------------------------------------------------------

comm_set_time:
	movlw	"b"								; send echo
	movwf	TXREG1

	call	rs232_wait_tx					; wait for UART
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	movff	RCREG1, hours
	movlw	d'24'
	cpfslt	hours
	clrf	hours
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	movff	RCREG1, mins
	movlw	d'60'
	cpfslt	mins
	clrf	mins
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	movff	RCREG1, secs
	movlw	d'60'
	cpfslt	secs
	clrf	secs
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	movff	RCREG1, month
	movlw	d'13'
	cpfslt	month
	movwf	month
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	call	comm_check_day                  ; Check day
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	return                          		; No, abort!
	movff	RCREG1, year
	movlw	d'100'
	cpfslt	year
	clrf	year
	; All ok, set RTCC
	call	rtc_set_rtc                     ; writes mins,sec,hours,day,month and year to rtc module
    bra		comm_download_mode0             ; Done. back to loop with timeout reset

;-----------------------------------------------------------------------------
; Set OSTC3 custom text string (opt_name_length ascii chars).
;

comm_set_custom_text:
    movlw	"c"								; send echo
    movwf	TXREG1
    call	rs232_wait_tx					; wait for UART
    lfsr	FSR2,opt_name
    movlw	opt_name_length
    movwf	lo								; counter
comm_set_ctext_loop:
    call	rs232_get_byte
    btfsc	rs232_recieve_overflow          ; Got byte?
    bra		comm_download_mode0             ; No, loop with timeout reset
    movff	RCREG1,POSTINC2                 ; Store character
    decfsz	lo,F
    bra		comm_set_ctext_loop
    bra		comm_download_mode0             ; Done. Loop with timeout reset

;-----------------------------------------------------------------------------
; Reply Serial (2 bytes low:high), firmware (major.minor) and custom text.
;
  
comm_identify:
    movlw	"i"								; send echo
    movwf	TXREG1
    call	rs232_wait_tx					; wait for UART

    ;---- Read serial from internal EEPROM address 0000
	clrf	EEADRH
	clrf	EEADR                       ; Get Serial number LOW
	call	read_eeprom                 ; read byte
	movff	EEDATA,lo
	incf	EEADR,F                     ; Get Serial number HIGH
	call	read_eeprom                 ; read byte
	movff	EEDATA,hi

    ;---- Emit serial number
	movff   lo,TXREG1
	call	rs232_wait_tx
	movff   hi,TXREG1
	call	rs232_wait_tx

	;---- Emit fiwmware hi.lo
	movlw   softwareversion_x
	movwf   TXREG1
	call	rs232_wait_tx
	movlw   softwareversion_y
	movwf   TXREG1
	call	rs232_wait_tx

	;---- Emit custom text
	movlw   opt_name_length
	movwf   hi
	lfsr    FSR2,opt_name

common_identify_loop:
    movff   POSTINC2,TXREG1
	call	rs232_wait_tx
	decfsz	hi,F
	bra		common_identify_loop

    bra     comm_download_mode0             ; Done.


;-----------------------------------------------------------------------------

comm_send_dive:
	movlw	"f"								; send echo
	movwf	TXREG1
	
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_download_mode0		; No, abort!
	movff	RCREG1,lo						; Store dive number (0-255)
; First, send the header (again)
	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> lo=0
	; 2nd: 201000h-201FFFh -> lo=1
	; 3rd: 202000h-202FFFh -> lo=2
	; 256: 2FF000h-2FFFFFh -> lo=255
	clrf	ext_flash_address+0
	clrf	ext_flash_address+1
	movlw	0x20
	movwf	ext_flash_address+2
	movlw	.16
	mulwf	lo				; lo*16 = offset to 0x2000 (up:hi)
	movf	PRODL,W
	addwf	ext_flash_address+1,F
	movf	PRODH,W
	addwfc	ext_flash_address+2,F

	incf_ext_flash_address	d'2'				; Skip 0xFA, 0xFA
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+0
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+1
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+2
	call		ext_flash_byte_read_plus		; Read end address of profile
	movff		temp1,convert_value_temp+0
	call		ext_flash_byte_read_plus		; Read end address of profile
	movff		temp1,convert_value_temp+1
	call		ext_flash_byte_read_plus		; Read end address of profile
	movff		temp1,convert_value_temp+2
	decf_ext_flash_address	d'8'				; Back again to first 0xFA in header

    movf        ext_flash_log_pointer+0,W
    cpfseq      convert_value_temp+0            ; Equal?
    bra         comm_send_dive1                 ; No, Send header

    movf        ext_flash_log_pointer+1,W
    cpfseq      convert_value_temp+1            ; Equal?
    bra         comm_send_dive1                 ; No, Send header

    movf        ext_flash_log_pointer+2,W
    cpfseq      convert_value_temp+2            ; Equal?
    bra         comm_send_dive1                 ; No, Send header

    ; Start=End -> Not good, abort
    bra		comm_download_mode0                 ; Done. Loop with timeout reset

comm_send_dive1:
	; Send header
	clrf	hi							; Counter	
	call	rs232_wait_tx				; Wait for UART
	call	ext_flash_read_block_start	; 1st byte
	movwf	TXREG1
	bra		comm_send_dive_header
comm_send_dive_header2:
	call	ext_flash_read_block		; Read one byte
	movwf	TXREG1						; Start new transmit
comm_send_dive_header:
	call	rs232_wait_tx				; Wait for UART
	decfsz	hi,F
	bra		comm_send_dive_header2
	call	ext_flash_read_block_stop

	; Set address for profile
	movff	ext_flash_log_pointer+0,ext_flash_address+0
	movff	ext_flash_log_pointer+1,ext_flash_address+1
	movff	ext_flash_log_pointer+2,ext_flash_address+2

	movlw	.6								; Skip 6byte short header in profile - only for internal use
	call	incf_ext_flash_address0_0x20	; increases bytes in ext_flash_address:3 with 0x200000 bank switching

comm_send_dive_profile:
	call	ext_flash_byte_read_plus_0x20	; Read one byte into temp1, takes care of banking at 0x200000
	call	rs232_wait_tx					; Wait for UART
	movff	temp1,TXREG1						; Send a byte
	
	; 24bit compare with end address
	movff	convert_value_temp+0,WREG
	cpfseq	ext_flash_address+0
	bra		comm_send_dive_profile
	movff	convert_value_temp+1,WREG
	cpfseq	ext_flash_address+1
	bra		comm_send_dive_profile
	movff	convert_value_temp+2,WREG
	cpfseq	ext_flash_address+2
	bra		comm_send_dive_profile
	
	call	rs232_wait_tx					; Wait for UART
	bra		comm_download_mode0		; Done. Loop with timeout reset

;-----------------------------------------------------------------------------

comm_read_setting:
    movlw   "r"
	movwf	TXREG1
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_read_abort                 ; No, abort!
	call	rs232_wait_tx					; Wait for UART
    movf    RCREG1,W                        ; Copy
    bz      comm_read_unused                ; RCREG1=0
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=1
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=2
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=3
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=4
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=5
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=6
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=7
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=8
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=9
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=10
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=11
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=12
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=13
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=14
    dcfsnz  WREG
    bra     comm_read_unused                ; RCREG1=15
    dcfsnz  WREG
    bra     comm_read_gas1                  ; RCREG1=0x10
    dcfsnz  WREG
    bra     comm_read_gas2                  ; RCREG1=0x11
    dcfsnz  WREG
    bra     comm_read_gas3                  ; RCREG1=0x12
    dcfsnz  WREG
    bra     comm_read_gas4                  ; RCREG1=0x13
    dcfsnz  WREG
    bra     comm_read_gas5                  ; RCREG1=0x14
    dcfsnz  WREG
    bra     comm_read_dil1                  ; RCREG1=0x15
    dcfsnz  WREG
    bra     comm_read_dil2                  ; RCREG1=0x16
    dcfsnz  WREG
    bra     comm_read_dil3                  ; RCREG1=0x17
    dcfsnz  WREG
    bra     comm_read_dil4                  ; RCREG1=0x18
    dcfsnz  WREG
    bra     comm_read_dil5                  ; RCREG1=0x19
    dcfsnz  WREG
    bra     comm_read_sp1                   ; RCREG1=0x1A
    dcfsnz  WREG
    bra     comm_read_sp2                   ; RCREG1=0x1B
    dcfsnz  WREG
    bra     comm_read_sp3                   ; RCREG1=0x1C
    dcfsnz  WREG
    bra     comm_read_sp4                   ; RCREG1=0x1D
    dcfsnz  WREG
    bra     comm_read_sp5                   ; RCREG1=0x1E
    dcfsnz  WREG
    bra     comm_read_ccr_mode              ; RCREG1=0x1F
    dcfsnz  WREG
    bra     comm_read_dive_mode             ; RCREG1=0x20
    dcfsnz  WREG
    bra     comm_read_decotype              ; RCREG1=0x21
    dcfsnz  WREG
    bra     comm_read_ppo2_max              ; RCREG1=0x22
    dcfsnz  WREG
    bra     comm_read_ppo2_min              ; RCREG1=0x23
    dcfsnz  WREG
    bra     comm_read_ftts                  ; RCREG1=0x24
    dcfsnz  WREG
    bra     comm_read_gf_low                ; RCREG1=0x25
    dcfsnz  WREG
    bra     comm_read_gf_high               ; RCREG1=0x26
    dcfsnz  WREG
    bra     comm_read_agf_low               ; RCREG1=0x27
    dcfsnz  WREG
    bra     comm_read_agf_high              ; RCREG1=0x28
    dcfsnz  WREG
    bra     comm_read_agf_selectable        ; RCREG1=0x29
    dcfsnz  WREG
    bra     comm_read_saturation            ; RCREG1=0x2A
    dcfsnz  WREG
    bra     comm_read_desaturation          ; RCREG1=0x2B
    dcfsnz  WREG
    bra     comm_read_last_deco             ; RCREG1=0x2C
    dcfsnz  WREG
    bra     comm_read_brightness            ; RCREG1=0x2D
    dcfsnz  WREG
    bra     comm_read_units                 ; RCREG1=0x2E
    dcfsnz  WREG
    bra     comm_read_samplingrate          ; RCREG1=0x2F
    dcfsnz  WREG
    bra     comm_read_salinity              ; RCREG1=0x30
    dcfsnz  WREG
    bra     comm_read_divemode_colour       ; RCREG1=0x31
    dcfsnz  WREG
    bra     comm_read_language              ; RCREG1=0x32
    dcfsnz  WREG
    bra     comm_read_date_format           ; RCREG1=0x33
    dcfsnz  WREG
    bra     comm_read_compass_gain          ; RCREG1=0x34
    dcfsnz  WREG
    bra     comm_read_pressure_adjust       ; RCREG1=0x35



comm_read_unused:
comm_read_abort:
    bra		comm_download_mode0             ; Done. Loop with timeout reset

comm_read_setting_wait:
    call	rs232_wait_tx					; Wait for UART
    return

comm_read_gas1:
    movff   opt_gas_O2_ratio+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_He_ratio+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_type+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_deco_gas_change+0,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gas2:
    movff   opt_gas_O2_ratio+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_He_ratio+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_type+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_deco_gas_change+1,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gas3:
    movff   opt_gas_O2_ratio+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_He_ratio+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_type+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_deco_gas_change+2,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gas4:
    movff   opt_gas_O2_ratio+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_He_ratio+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_type+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_deco_gas_change+3,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gas5:
    movff   opt_gas_O2_ratio+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_He_ratio+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_gas_type+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_deco_gas_change+4,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset

comm_read_dil1:
    movff   opt_dil_O2_ratio+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_He_ratio+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_type+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_dil_change+0,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_dil2:
    movff   opt_dil_O2_ratio+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_He_ratio+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_type+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_dil_change+1,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_dil3:
    movff   opt_dil_O2_ratio+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_He_ratio+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_type+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_dil_change+2,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_dil4:
    movff   opt_dil_O2_ratio+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_He_ratio+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_type+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_dil_change+3,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_dil5:
    movff   opt_dil_O2_ratio+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_He_ratio+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   opt_dil_type+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_dil_change+4,TXREG1
    bra		comm_download_mode0             ; Done. Loop with timeout reset

comm_read_sp1:
    movff   char_I_setpoint_cbar+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_setpoint_change+0, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_sp2:
    movff   char_I_setpoint_cbar+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_setpoint_change+1, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_sp3:
    movff   char_I_setpoint_cbar+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_setpoint_change+2, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_sp4:
    movff   char_I_setpoint_cbar+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_setpoint_change+3, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_sp5:
    movff   char_I_setpoint_cbar+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    movff   char_I_setpoint_change+4, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset

comm_read_ccr_mode:
    movff   opt_ccr_mode, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_dive_mode:
    movff   opt_dive_mode, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_decotype:
    movff   char_I_deco_model, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_ppo2_max:
    movff   opt_ppO2_max, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_ppo2_min:
    movff   opt_ppO2_min, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_ftts:
    movff   char_I_extra_time, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gf_low:
    movff   opt_GF_low, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_gf_high:
    movff   opt_GF_high, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_agf_low:
    movff   opt_aGF_low, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_agf_high:
    movff   opt_aGF_high, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_agf_selectable:
    movff   opt_enable_aGF, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_saturation:
    movff   char_I_saturation_multiplier, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_desaturation:
    movff   char_I_desaturation_multiplier, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_last_deco:
    movff   opt_last_stop, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_brightness:
    movff   opt_brightness, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_units:
    movff   opt_units, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_samplingrate:
    movff   opt_sampling_rate, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_salinity:
    movff   opt_salinity, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_divemode_colour:
    movff   opt_dive_color_scheme, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_language:
    movff   opt_language, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_date_format:
    movff   opt_dateformat, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_compass_gain:
    movff   opt_compass_gain, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset
comm_read_pressure_adjust:
    movff   opt_pressure_adjust, TXREG1
    rcall   comm_read_setting_wait          ; Wait for UART
    bra		comm_download_mode0             ; Done. Loop with timeout reset

;-----------------------------------------------------------------------------


comm_write_setting:
    movlw   "w"
	movwf	TXREG1
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_write_abort                 ; No, abort!
	call	rs232_wait_tx					; Wait for UART
    movf    RCREG1,W                        ; Copy
    bz      comm_write_unused                ; RCREG1=0
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=1
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=2
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=3
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=4
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=5
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=6
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=7
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=8
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=9
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=10
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=11
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=12
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=13
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=14
    dcfsnz  WREG
    bra     comm_write_unused                ; RCREG1=15
    dcfsnz  WREG
    bra     comm_write_gas1                  ; RCREG1=0x10
    dcfsnz  WREG
    bra     comm_write_gas2                  ; RCREG1=0x11
    dcfsnz  WREG
    bra     comm_write_gas3                  ; RCREG1=0x12
    dcfsnz  WREG
    bra     comm_write_gas4                  ; RCREG1=0x13
    dcfsnz  WREG
    bra     comm_write_gas5                  ; RCREG1=0x14
    dcfsnz  WREG
    bra     comm_write_dil1                  ; RCREG1=0x15
    dcfsnz  WREG
    bra     comm_write_dil2                  ; RCREG1=0x16
    dcfsnz  WREG
    bra     comm_write_dil3                  ; RCREG1=0x17
    dcfsnz  WREG
    bra     comm_write_dil4                  ; RCREG1=0x18
    dcfsnz  WREG
    bra     comm_write_dil5                  ; RCREG1=0x19
    dcfsnz  WREG
    bra     comm_write_sp1                   ; RCREG1=0x1A
    dcfsnz  WREG
    bra     comm_write_sp2                   ; RCREG1=0x1B
    dcfsnz  WREG
    bra     comm_write_sp3                   ; RCREG1=0x1C
    dcfsnz  WREG
    bra     comm_write_sp4                   ; RCREG1=0x1D
    dcfsnz  WREG
    bra     comm_write_sp5                   ; RCREG1=0x1E
    dcfsnz  WREG
    bra     comm_write_ccr_mode              ; RCREG1=0x1F
    dcfsnz  WREG
    bra     comm_write_dive_mode             ; RCREG1=0x20
    dcfsnz  WREG
    bra     comm_write_decotype              ; RCREG1=0x21
    dcfsnz  WREG
    bra     comm_write_ppo2_max              ; RCREG1=0x22
    dcfsnz  WREG
    bra     comm_write_ppo2_min              ; RCREG1=0x23
    dcfsnz  WREG
    bra     comm_write_ftts                  ; RCREG1=0x24
    dcfsnz  WREG
    bra     comm_write_gf_low                ; RCREG1=0x25
    dcfsnz  WREG
    bra     comm_write_gf_high               ; RCREG1=0x26
    dcfsnz  WREG
    bra     comm_write_agf_low               ; RCREG1=0x27
    dcfsnz  WREG
    bra     comm_write_agf_high              ; RCREG1=0x28
    dcfsnz  WREG
    bra     comm_write_agf_selectable        ; RCREG1=0x29
    dcfsnz  WREG
    bra     comm_write_saturation            ; RCREG1=0x2A
    dcfsnz  WREG
    bra     comm_write_desaturation          ; RCREG1=0x2B
    dcfsnz  WREG
    bra     comm_write_last_deco             ; RCREG1=0x2C
    dcfsnz  WREG
    bra     comm_write_brightness            ; RCREG1=0x2D
    dcfsnz  WREG
    bra     comm_write_units                 ; RCREG1=0x2E
    dcfsnz  WREG
    bra     comm_write_samplingrate          ; RCREG1=0x2F
    dcfsnz  WREG
    bra     comm_write_salinity              ; RCREG1=0x30
    dcfsnz  WREG
    bra     comm_write_divemode_colour       ; RCREG1=0x31
    dcfsnz  WREG
    bra     comm_write_language              ; RCREG1=0x32
    dcfsnz  WREG
    bra     comm_write_date_format           ; RCREG1=0x33
    dcfsnz  WREG
    bra     comm_write_compass_gain          ; RCREG1=0x34
    dcfsnz  WREG
    bra     comm_write_pressure_adjust       ; RCREG1=0x35

comm_write_unused:
comm_write_abort:
    ; Check Options, gases and diluents
    call    option_check_all                ; Check all options (and reset if not within their min/max boundaries)
    bsf     ccr_diluent_setup               ; =1: Setting up Diluents ("Gas6-10")
    call    gaslist_cleanup_list            ; Takes care that only one gas can be first and first has 0m change depth
    bcf     ccr_diluent_setup               ; =1: Setting up Diluents ("Gas6-10")
    call    gaslist_cleanup_list            ; Takes care that only one gas can be first and first has 0m change depth
    call    get_first_gas_to_WREG           ; Makes sure at least one Gas is "First"
    call    get_first_dil_to_WREG           ; Makes sure at least one Diluent is "First"
    bra		comm_download_mode0             ; Done. Loop with timeout reset

comm_write_gas1:
    call	rs232_get_byte
    movff   RCREG1,opt_gas_O2_ratio+0
    call	rs232_get_byte
    movff   RCREG1,opt_gas_He_ratio+0
    call	rs232_get_byte
    movff   RCREG1,opt_gas_type+0
    call	rs232_get_byte
    movff   RCREG1,char_I_deco_gas_change+0
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gas2:
    call	rs232_get_byte
    movff   RCREG1,opt_gas_O2_ratio+1
    call	rs232_get_byte
    movff   RCREG1,opt_gas_He_ratio+1
    call	rs232_get_byte
    movff   RCREG1,opt_gas_type+1
    call	rs232_get_byte
    movff   RCREG1,char_I_deco_gas_change+1
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gas3:
    call	rs232_get_byte
    movff   RCREG1,opt_gas_O2_ratio+2
    call	rs232_get_byte
    movff   RCREG1,opt_gas_He_ratio+2
    call	rs232_get_byte
    movff   RCREG1,opt_gas_type+2
    call	rs232_get_byte
    movff   RCREG1,char_I_deco_gas_change+2
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gas4:
    call	rs232_get_byte
    movff   RCREG1,opt_gas_O2_ratio+3
    call	rs232_get_byte
    movff   RCREG1,opt_gas_He_ratio+3
    call	rs232_get_byte
    movff   RCREG1,opt_gas_type+3
    call	rs232_get_byte
    movff   RCREG1,char_I_deco_gas_change+3
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gas5:
    call	rs232_get_byte
    movff   RCREG1,opt_gas_O2_ratio+4
    call	rs232_get_byte
    movff   RCREG1,opt_gas_He_ratio+4
    call	rs232_get_byte
    movff   RCREG1,opt_gas_type+4
    call	rs232_get_byte
    movff   RCREG1,char_I_deco_gas_change+4
    bra		comm_write_abort             ; Done. Loop with timeout reset

comm_write_dil1:
    call	rs232_get_byte
    movff   RCREG1,opt_dil_O2_ratio+0
    call	rs232_get_byte
    movff   RCREG1,opt_dil_He_ratio+0
    call	rs232_get_byte
    movff   RCREG1,opt_dil_type+0
    call	rs232_get_byte
    movff   RCREG1,char_I_dil_change+0
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_dil2:
    call	rs232_get_byte
    movff   RCREG1,opt_dil_O2_ratio+1
    call	rs232_get_byte
    movff   RCREG1,opt_dil_He_ratio+1
    call	rs232_get_byte
    movff   RCREG1,opt_dil_type+1
    call	rs232_get_byte
    movff   RCREG1,char_I_dil_change+1
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_dil3:
    call	rs232_get_byte
    movff   RCREG1,opt_dil_O2_ratio+2
    call	rs232_get_byte
    movff   RCREG1,opt_dil_He_ratio+2
    call	rs232_get_byte
    movff   RCREG1,opt_dil_type+2
    call	rs232_get_byte
    movff   RCREG1,char_I_dil_change+2
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_dil4:
    call	rs232_get_byte
    movff   RCREG1,opt_dil_O2_ratio+3
    call	rs232_get_byte
    movff   RCREG1,opt_dil_He_ratio+3
    call	rs232_get_byte
    movff   RCREG1,opt_dil_type+3
    call	rs232_get_byte
    movff   RCREG1,char_I_dil_change+3
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_dil5:
    call	rs232_get_byte
    movff   RCREG1,opt_dil_O2_ratio+4
    call	rs232_get_byte
    movff   RCREG1,opt_dil_He_ratio+4
    call	rs232_get_byte
    movff   RCREG1,opt_dil_type+4
    call	rs232_get_byte
    movff   RCREG1,char_I_dil_change+4
    bra		comm_write_abort             ; Done. Loop with timeout reset

comm_write_sp1:
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_cbar+0
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_change+0
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_sp2:
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_cbar+1
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_change+1
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_sp3:
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_cbar+2
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_change+2
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_sp4:
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_cbar+3
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_change+3
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_sp5:
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_cbar+4
    call	rs232_get_byte
    movff   RCREG1,char_I_setpoint_change+4
    bra		comm_write_abort             ; Done. Loop with timeout reset

comm_write_ccr_mode:
    call	rs232_get_byte
    movff   RCREG1, opt_ccr_mode
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_dive_mode:
    call	rs232_get_byte
    movff   RCREG1, opt_dive_mode
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_decotype:
    call	rs232_get_byte
    movff   RCREG1, char_I_deco_model
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_ppo2_max:
    call	rs232_get_byte
    movff   RCREG1, opt_ppO2_max
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_ppo2_min:
    call	rs232_get_byte
    movff   RCREG1, opt_ppO2_min
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_ftts:
    call	rs232_get_byte
    movff   RCREG1, char_I_extra_time
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gf_low:
    call	rs232_get_byte
    movff   RCREG1, opt_GF_low
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_gf_high:
    call	rs232_get_byte
    movff   RCREG1, opt_GF_high
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_agf_low:
    call	rs232_get_byte
    movff   RCREG1, opt_aGF_low
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_agf_high:
    call	rs232_get_byte
    movff   RCREG1, opt_aGF_high
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_agf_selectable:
    call	rs232_get_byte
    movff   RCREG1, opt_enable_aGF
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_saturation:
    call	rs232_get_byte
    movff   RCREG1, char_I_saturation_multiplier
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_desaturation:
    call	rs232_get_byte
    movff   RCREG1, char_I_desaturation_multiplier
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_last_deco:
    call	rs232_get_byte
    movff   RCREG1, opt_last_stop
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_brightness:
    call	rs232_get_byte
    movff   RCREG1, opt_brightness
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_units:
    call	rs232_get_byte
    movff   RCREG1, opt_units
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_samplingrate:
    call	rs232_get_byte
    movff   RCREG1, opt_sampling_rate
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_salinity:
    call	rs232_get_byte
    movff   RCREG1, opt_salinity
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_divemode_colour:
    call	rs232_get_byte
    movff   RCREG1, opt_dive_color_scheme
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_language:
    call	rs232_get_byte
    movff   RCREG1, opt_language
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_date_format:
    call	rs232_get_byte
    movff   RCREG1, opt_dateformat
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_compass_gain:
    call	rs232_get_byte
    movff   RCREG1, opt_compass_gain
    bra		comm_write_abort             ; Done. Loop with timeout reset
comm_write_pressure_adjust:
    call	rs232_get_byte
    movff   RCREG1, opt_pressure_adjust
    bra		comm_write_abort             ; Done. Loop with timeout reset

;-----------------------------------------------------------------------------

comm_send_string:
	movlw	"n"								; send echo
	movwf	TXREG1
	call	rs232_wait_tx					; Wait for UART
	WIN_SMALL	comm_string_column, comm_string_row
	lfsr	FSR2,buffer
	movlw	.16
	movwf	lo								; counter
comm_send_string_loop:
	call	rs232_get_byte
	btfsc	rs232_recieve_overflow			; Got byte?
	bra		comm_send_string_abort          ; No, abort!
	movff	RCREG1,POSTINC2					; Store character
	decfsz	lo,F
	bra		comm_send_string_loop
comm_send_string_abort:
	STRCAT_PRINT ""							; Show the text
    goto    comm_download_mode0             ; Done. Loop with timeout reset

;-----------------------------------------------------------------------------

comm_check_day:
	movff	RCREG1, day
	movff	month,lo		; new month
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.28
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.30
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.30
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.30
	dcfsnz	lo,F
	movlw	.31
	dcfsnz	lo,F
	movlw	.30
	dcfsnz	lo,F
	movlw	.31
	cpfsgt	day						; day ok?
	return							; OK
	movlw	.1						; not OK, set to 1st
	movwf	day
	return	

;----------------------------------------------------------------------------
        END