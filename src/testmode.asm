;=============================================================================
;
;   File tesmode.asm
;
;   Test Mode for Hardware check
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2012-10-31 : [mH] Creation

#include    "ostc3.inc"                 ; Mandatory header
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include	"start.inc"
#include	"tft.inc"
#include	"tft_outputs.inc"
#include	"isr.inc"
#include	"adc_lightsensor.inc"
#include	"strings.inc"
#include	"sleepmode.inc"
#include 	"wait.inc"                  ; speed_*
#include    "mcp.inc"                   ; RX
#include    "i2c.inc"
#include    "surfmode.inc"
#include    "math.inc"
#include    "eeprom_rs232.inc"

    extern  compass
    extern  compass_filter
    extern  compass_filter_init
    extern  compass_reset_calibration
    extern  compass_add_calibration
    extern  compass_solve_calibration

testmode    CODE

;=============================================================================
; Boot tasks for all modes
	global	testloop
testloop:
    call	speed_normal
    bcf     no_sensor_int           ; Sensor ISR

    clrf	CCP1CON					; stop PWM
	bcf		PORTC,2					; Pull PWM output to GND
	call	TFT_boot                ; Initialize TFT (includes clear screen)
    call    enable_ir               ; Enable IR-Port
	WIN_TOP		.0
	WIN_LEFT	.0
	WIN_FONT 	FT_SMALL
	WIN_INVERT	.0					    ; Init new Wordprocessor
	call    TFT_standard_color
    call    mcp_reset_rssi              ; Setup RX
    call    TFT_Display_FadeIn

    call    I2C_init_compass
    call    I2C_init_accelerometer

    ; Init compass/accel filtering value.
    call    I2C_RX_compass
    call    I2C_RX_accelerometer
    call    compass_filter_init

 	clrf	timeout_counter2				
	clrf 	timeout_counter3
	bcf		premenu						; clear premenu flag
	bcf		menubit						; clear menu flag

	bcf		switch_left
	bcf		switch_right

testloop_loop:
    btfss	onesecupdate				; do every second tasks?
	bra		testloop_loop2				; no, loop

; One Second tasks
	call	speed_normal
    call    I2C_RX_compass              ; Test Compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter Raw compass + accel readings.
    banksel common

    ; Make a second measure and filter, to show faster reactions.
    call    I2C_RX_compass              ; Test Compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter Raw compass + accel readings.
    call    compass                     ; Do compass corrections.
    banksel common

    call    TFT_update_raw_data
	movlw	.240
    call	timeout_testmode			; check timeout

	bcf		onesecupdate				; every second tasks done
	
testloop_loop2:
; Tasks approx. every 50ms for all modes
    bcf     LEDg
    btfsc   vusb_in
    bsf     LEDg

; Mode tasks
    extern  surfloop
    btfsc   switch_left
   	goto	surfloop    				; Exit

    btfsc   switch_right
   	goto	compass_calibration_loop

    btfsc	sleepmode					; Sleepmode active?
	goto	sleeploop					; Yes, switch into sleepmode!

	bra		testloop_loop				; loop testmode


    global  compass_calibration_loop
compass_calibration_loop:               ; Compass calibration
    bsf     no_sensor_int               ; No Sensor ISR
    call    I2C_init_accelerometer
    call    I2C_init_compass_fast
    call	TFT_ClearScreen

    ; Mask
    WIN_COLOR   color_greenish
	WIN_SMALL	.16,.0
	STRCPY_TEXT_PRINT	tCompassMenu
    call	TFT_standard_color
	WIN_SMALL	.0,.215
    STRCPY_TEXT_PRINT  tExit

 	clrf	timeout_counter2
	clrf 	timeout_counter3

    call	speed_fastest
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; read Accelerometer
    call    compass_filter_init         ; set DX_f values
    call	compass_reset_calibration   ; Reset CX_f values
    banksel common

compass_calibration_loop2:
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter compass raw data
    banksel common

    ; Twice
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter compass raw data
    banksel common
;
;    ; Three
;    call    I2C_RX_compass              ; read compass
;    call    I2C_RX_accelerometer        ; Test Accelerometer
;    call    compass_filter              ; Filter compass raw data
;    banksel common
;
;    ; Four times to get cleaner values
;    call    I2C_RX_compass              ; read compass
;    call    I2C_RX_accelerometer        ; Test Accelerometer
;    call    compass_filter              ; Filter compass raw data

    ; And register only one value out of four:
    call    compass_add_calibration     ; check and store new max/min values
    banksel common

    call    TFT_compass_fast            ; show values
;    btg     LEDg                       ; Show read-out interval

    btfsc	sleepmode					; Sleepmode active?
    bra     compass_calibration_exit    ; Yes, exit

    btfsc   switch_left                 ; Button pressed?
    bra     compass_calibration_exit    ; Yes, exit

    btfss	onesecupdate				; do every second tasks?
    bra     compass_calibration_loop2   ; no, loop here

	movlw	.240
    call	timeout_testmode			; check timeout
    bcf     onesecupdate                ; clear flag

    bra     compass_calibration_loop2   ; loop here

compass_calibration_exit:
    call    compass_solve_calibration
    banksel common
    extern  option_save_all
	call	option_save_all             ; save all settings into EEPROM
    bcf     sleepmode                   ; Clear the flag before exiting to surfacemode
    movlw   .6
    movwf   menupos3                    ; Set to compass view...
   	goto	surfloop       				; ...and exit

    END