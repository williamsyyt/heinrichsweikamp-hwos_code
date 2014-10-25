;=============================================================================
;
;   File start.asm
;
;   Startup subroutines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-06 : [mH] moving from OSTC code

#include    "ostc3.inc"                  ; Mandatory header
#include	"ms5541.inc"
#include	"isr.inc"
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include	"eeprom_rs232.inc"
#include	"math.inc"
#include	"tft.inc"
#include	"surfmode.inc"
#include	"wait.inc"
#include	"rtc.inc"
#include 	"external_flash.inc"
#include	"convert.inc"
#include	"strings.inc"
#include	"tft_outputs.inc"
#include    "adc_lightsensor.inc"
#include    "i2c.inc"

        extern  init_ostc3
        extern  option_restore_all

;=============================================================================
; Reset vector: What to do on device wake-up and hard reset.
;
reset_v code     0x00000
;    goto    start
	goto	0x1FF00     ; Bootloader

	ORG     0x00004			; Needed for second-level bootloader
	goto	start
;=============================================================================
boot   CODE
	global start

start:
	lfsr	FSR0,0x000				; Clear rambank 0-14
clear_rambank:
	clrf	POSTINC0
	movlw	0x0F
	cpfseq	FSR0H					; Bank 14 done?
	bra		clear_rambank			; clear...

    call    init_ostc3
  	call    speed_normal

	bsf		no_sensor_int			; disable sensor interrupt

	call	disable_rs232			; disable UART module
; Air pressure compensation	after reset
	call	get_calibration_data	; Get calibration data from pressure sensor
	banksel common                  ; get_calibration_data uses isr_backup
	bcf		no_sensor_int		    ; normal sensor interrupt mode

    call    piezo_config
    call	TFT_DisplayOff			; display off
    bsf     LEDr                    ; Status LED
	bcf		pressure_refresh
; First pass will not have valid temperature!
	btfss	pressure_refresh 		; Air pressure compensation
	bra		$-2
    bcf     LEDr
; Second pass
	bcf		pressure_refresh
	btfss	pressure_refresh 		; Air pressure compensation
	bra		$-2

	clrf	rel_pressure+0
	clrf	rel_pressure+1
	clrf	surface_interval+0
	clrf	surface_interval+1

    SAFE_2BYTE_COPY amb_pressure, last_surfpressure

	movlw	LOW		max_surfpressure
	movff	WREG,sub_a+0				; max. "allowed" airpressure in mbar
	movlw	HIGH	max_surfpressure
	movff	WREG,sub_a+1				; max. "allowed" airpressure in mbar
	movff	last_surfpressure+0,sub_b+0
	movff	last_surfpressure+1,sub_b+1
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag                ; Is 1080mbar < amb_pressure ?
	bra		start_copy_pressure		; NO: current airpressure is lower then "allowed" airpressure, ok!

    ; not ok! Overwrite with max. "allowed" airpressure
	movlw	LOW		max_surfpressure
	movff	WREG,last_surfpressure+0	; max. "allowed" airpressure in mbar
	movlw	HIGH	max_surfpressure
	movff	WREG,last_surfpressure+1	; max. "allowed" airpressure in mbar

start_copy_pressure:
	movff	last_surfpressure+0,last_surfpressure_15min+0
	movff	last_surfpressure+1,last_surfpressure_15min+1
	movff	last_surfpressure+0,last_surfpressure_30min+0
	movff	last_surfpressure+1,last_surfpressure_30min+1	; Rests all airpressure registers

; reset deco data for surface mode
	movlw	d'79'
	movff	WREG,char_I_N2_ratio            ; 79% N2
    SAFE_2BYTE_COPY amb_pressure,int_I_pres_respiration ; copy for deco routine
	movff	int_I_pres_respiration+0,int_I_pres_surface+0     ; copy for desat routine
	movff	int_I_pres_respiration+1,int_I_pres_surface+1		

	extern	deco_reset
	call	deco_reset
  	clrf	nofly_time+0	              	; Reset NoFly
  	clrf	nofly_time+1
	clrf	desaturation_time+0				; Reset Desat
	clrf	desaturation_time+1

	call	rtc_init						; init clock

    movlw   HIGH .512           ; =2
    movwf   EEADRH
    read_int_eeprom .0
    clrf    EEADRH
    movlw   0xAA
    cpfseq  EEDATA              ; =0xAA
    bra     no_deco_restore     ; No

    extern  restore_decodata_from_eeprom
    call    restore_decodata_from_eeprom    ; Reload deco data and date/time from eeprom

no_deco_restore:
	call	deco_calc_desaturation_time     ; calculate desaturation time
	banksel common
	call	deco_calc_wo_deco_step_1_min	; calculate deco in surface mode
	banksel common
	bcf		menubit							; clear menu flag
; Check for Power-on reset here
	extern	new_battery_menu	
	extern	use_old_batteries

	btfsc	RCON,POR						; Was this a power-on reset?
    goto	use_old_batteries				; No, load last stored battery values
	; "new_battery_menu" and "use_old_batteries" 'goto' back to "power_on_return"

    ; Yes
 	goto	new_battery_menu				; show "New battery dialog"
	; "new_battery_menu" and "use_old_batteries" 'goto' back to "power_on_return"

	global	power_on_return
power_on_return:
	bsf		RCON,POR						; Set bit for next detection

; check firmware and reset Custom Functions after an update
	movlw	d'1'
	movwf	EEADR
	movlw	d'1'
	movwf	EEADRH
	call	read_eeprom				; read current version x
	movff	EEDATA,temp1
	incf	EEADR,F					; set to 0x102
	call	read_eeprom				; read current version y
	movff	EEDATA,temp2
	clrf	EEADRH					; Reset EEADRH

	movlw	softwareversion_x
	cpfseq	temp1					; compare version x
	bra		check_firmware_new		; is not equal -> reset CF and store new version in EEPROM

	movlw	softwareversion_y
	cpfseq	temp2					; compare version y
	bra		check_firmware_new		; is not equal -> reset CF and store new version in EEPROM
	bra		restart					; x and y are equal -> do not reset cf
		
check_firmware_new:
    call	speed_normal
	call	TFT_boot                ; Initialize TFT (includes clear screen)
	clrf    CCPR1L          		; Backlight off
    WIN_TOP     .50
    WIN_LEFT    .10
    movlw   LOW     0x1E000
    movwf   TBLPTRL
    movlw   HIGH    0x1E000
    movwf   TBLPTRH
    movlw   UPPER   0x1E000
    movwf   TBLPTRU
    extern  color_image
    call    color_image             ; Show logo
	call	TFT_standard_color
	WIN_SMALL   .10,.100
	STRCPY_TEXT_PRINT	tNewFirmware1	; "Update successfull!"
	WIN_SMALL   .10,.140
	STRCPY_TEXT	tNewFirmware2			; "New Firmware: "
	movlw	softwareversion_x
	movwf	lo
	bsf		leftbind
	output_8
	PUTC	"."
	movlw	softwareversion_y
	movwf	lo
	output_99x
	bcf		leftbind
	STRCAT_PRINT	""					; Print second row
    call    TFT_Display_FadeIn          ; Display resulting surface screen.

; place "after-update reset" here...

    extern  oPressureAdjust, option_reset, option_save
    lfsr    FSR0,oPressureAdjust
    call    option_reset            ; Reset FSR0 option to factory default.
    lfsr    FSR0,oPressureAdjust
    call    option_save             ; Save in EEPROM

	movlw	d'1'					; store current version in EEPROM
	movwf	EEADR
	movlw	d'1'
	movwf	EEADRH
	movlw	softwareversion_x
	movwf	EEDATA		
	call	write_eeprom			; write version x
	incf	EEADR,F					; set to 0x102
	movlw	softwareversion_y
	movwf	EEDATA		
	call	write_eeprom			; write version y
	clrf	EEADRH					; Reset EEADRH

	movlw	.7
	movwf	lo
check_firmware_new2:
	; Wait 1 second
	bcf		onesecupdate
	btfss	onesecupdate
	bra		$-2
	decfsz	lo,F					; Wait 10 seconds...
	bra		check_firmware_new2

	global	restart	
restart:
    clrf    STKPTR                  ; Never return from here
	extern	option_save_all, option_check_all

	btfsc	menubit					; Return from Menu/COMM mode or timeout?
	call	option_save_all			; Yes, save all settings into EEPROM

    call    option_check_all        ; Check all options (and reset if not within their min/max boundaries)

	clrf	flag1					; clear all flags
	clrf	flag2
	clrf	flag3
	clrf	flag4
	clrf	flag5
	clrf	flag6
    clrf	flag7
    clrf	flag8
    clrf    flag9
    clrf    flag10
	bsf		tft_is_dimming	; TFT is dimming up (soon), ignore ambient sensor!

    call    lt2942_get_status       ; Check for gauge IC
    btfsc   c3_hardware             ; C3 hardware?
    call    lt2942_init             ; Yes, init battery gauge IC

	; Select high altitude (Fly) mode?
	movff	last_surfpressure_30min+0,sub_b+0
	movff	last_surfpressure_30min+1,sub_b+1
	movlw	HIGH	high_altitude_threshold
	movwf	sub_a+1
	movlw	LOW		high_altitude_threshold	; Hard-wired 880mbar
	movwf	sub_a+0
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag				; Result negative (Ambient>880mbar)?
	bsf		high_altitude_mode		; No, Set Flag!

    btfss   c3_hardware
    bsf     TRISB,3
    btfss   c3_hardware
    bsf     TRISG,0
	call	ext_flash_disable_protection	; Disable write protection for external flash

    bsf     flip_screen             ; Flip 180°
    TSTOSS  opt_flip_screen         ; =1: Flip the screen
    bcf     flip_screen             ; Normal orientation

	goto	surfloop				; Jump to Surfaceloop!

;=============================================================================
; Setup all flags and parameters for divemode and simulator computations.
;
	global	restart_set_modes_and_flags
restart_set_modes_and_flags:		    ; "Call"ed from divemode, as well!
    call    option_restore_all      	; Restore everything from EEPROM

    ; Setup sampling rate
    movlw   .2
    movwf   samplingrate
    TSTOSS  opt_sampling_rate           ; =1: 10s, =0: 2s
    bra     restart_set_modes_and_flags1
    movlw   .10
    movwf   samplingrate
restart_set_modes_and_flags1:
    movff   opt_dive_mode,lo            ; 0=OC, 1=CC, 2=Gauge, 3=Apnea

	bcf		FLAG_apnoe_mode
    bcf     FLAG_ccr_mode             ; =1: CCR mode (Fixed ppO2 or Sensor) active
    bcf     FLAG_gauge_mode           ; =1: In Gauge mode
    call    disable_ir                ; IR off

    tstfsz  lo
    bra     restart_set_modes_and_flags2
    ; OC Mode
    return

restart_set_modes_and_flags2:
    decfsz  lo,F
    bra     restart_set_modes_and_flags3
    ; CC Mode
	bsf     FLAG_ccr_mode               ; =1: CCR mode (Fixed ppO2 or Sensor) active
    call    enable_ir_s8                ; Enable IR/S8-Port
    return

restart_set_modes_and_flags3:
    decfsz  lo,F
    bra     restart_set_modes_and_flags4
    ; Gauge Mode
    bsf     FLAG_gauge_mode           ; =1: In Gauge mode
    return

restart_set_modes_and_flags4:
    ; Apnea Mode
	bsf		FLAG_apnoe_mode
    return							    ; start in Surfacemode

	END