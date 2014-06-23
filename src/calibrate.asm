;=============================================================================
;
;   File calibration.asm
;
;   o2 sensor calibration subroutines
;
;   Copyright (c) 2014, Heinrichs Weikamp, all right reserved.
;=============================================================================

#include 	"ostc3.inc"
#include	"shared_definitions.h"		; Mailbox between c and asm
#include	"math.inc"
#include    "adc_lightsensor.inc"
#include    "eeprom_rs232.inc"

calibrate     CODE

	global	calibrate_mix
calibrate_mix:
    ; calibrate S8 HUD
    btfss   s8_digital          ; S8 Digital?
    bra     calibrate_mix2      ; No

    clrf    temp1               ; Chksum
    movlw   0xAA                ; Start Byte
    addwf   temp1,F
    movff   WREG,TXREG2
    call    rs232_wait_tx2

    movlw   0x31                ; Calibrate
    addwf   temp1,F
    movff   WREG,TXREG2
    call    rs232_wait_tx2

    movff   opt_calibration_O2_ratio,WREG         ; Calibration gas %O2
    addwf   temp1,F
    movff   WREG,TXREG2
    call    rs232_wait_tx2

    movff   amb_pressure+0,WREG         ; Ambient pressure
    addwf   temp1,F
    movff   WREG,TXREG2
    call    rs232_wait_tx2
    movff   amb_pressure+1,WREG
    addwf   temp1,F
    movff   WREG,TXREG2
    call    rs232_wait_tx2

    movff   temp1,TXREG2                ; Chksum
    call    rs232_wait_tx2

calibrate_mix2:
    movff   opt_calibration_O2_ratio,WREG         ; Calibration gas %O2
    mullw   .100
	movff	PRODL,xA+0
	movff	PRODH,xA+1
; (%O2*100)*[ambient,mbar]/100 -> xC
	movff	amb_pressure+0,xB+0
	movff	amb_pressure+1,xB+1
	call	mult16x16		;xA*xB=xC
	movlw	LOW		.100
	movwf	xB+0
	movlw	HIGH	.100
	movwf	xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	o2_mv_sensor1+0,xB+0
	movff	o2_mv_sensor1+1,xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	; xC= ppO2/mV
	movff	xC+0,opt_x_s1+0
	movff	xC+1,opt_x_s1+1				; Factor for Sensor1

    movff   opt_calibration_O2_ratio,WREG         ; Calibration gas %O2
    mullw   .100
	movff	PRODL,xA+0
	movff	PRODH,xA+1
; (%O2*100)*[ambient,mbar]/100 -> xC
	movff	amb_pressure+0,xB+0
	movff	amb_pressure+1,xB+1
	call	mult16x16		;xA*xB=xC
	movlw	LOW		.100
	movwf	xB+0
	movlw	HIGH	.100
	movwf	xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	o2_mv_sensor2+0,xB+0
	movff	o2_mv_sensor2+1,xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	; xC= ppO2/mV
	movff	xC+0,opt_x_s2+0
	movff	xC+1,opt_x_s2+1				; Factor for Sensor2

    movff   opt_calibration_O2_ratio,WREG         ; Calibration gas %O2
    mullw   .100
	movff	PRODL,xA+0
	movff	PRODH,xA+1
; (%O2*100)*[ambient,mbar]/100 -> xC
	movff	amb_pressure+0,xB+0
	movff	amb_pressure+1,xB+1
	call	mult16x16		;xA*xB=xC
	movlw	LOW		.100
	movwf	xB+0
	movlw	HIGH	.100
	movwf	xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	o2_mv_sensor3+0,xB+0
	movff	o2_mv_sensor3+1,xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	; xC= ppO2/mV
	movff	xC+0,opt_x_s3+0
	movff	xC+1,opt_x_s3+1				; Factor for Sensor3

	; Result is in 100�V
	movff	o2_mv_sensor1+0, sub_a+0
	movff	o2_mv_sensor1+1, sub_a+1
	movlw	LOW		min_mv
	movwf	sub_b+0
	movlw	HIGH	min_mv
	movwf	sub_b+1
	call	sub16			;  sub_c = sub_a - sub_b
	bsf		sensor1_active	; Sensor active!
	btfsc	neg_flag
	bcf		sensor1_active

	; Result is in 100�V
	movff	o2_mv_sensor2+0, sub_a+0
	movff	o2_mv_sensor2+1, sub_a+1
	movlw	LOW		min_mv
	movwf	sub_b+0
	movlw	HIGH	min_mv
	movwf	sub_b+1
	call	sub16			;  sub_c = sub_a - sub_b
	bsf		sensor2_active	; Sensor active!
	btfsc	neg_flag
	bcf		sensor2_active

	; Result is in 100�V
	movff	o2_mv_sensor3+0, sub_a+0
	movff	o2_mv_sensor3+1, sub_a+1
	movlw	LOW		min_mv
	movwf	sub_b+0
	movlw	HIGH	min_mv
	movwf	sub_b+1
	call	sub16			;  sub_c = sub_a - sub_b
	bsf		sensor3_active	; Sensor active!
	btfsc	neg_flag
	bcf		sensor3_active

	; When no sensor is found, enable all three to show error state
	btfsc	sensor1_active
	return
	btfsc	sensor2_active
	return
	btfsc	sensor3_active
	return
	bsf		sensor1_active
	bsf		sensor2_active
	bsf		sensor3_active
	; Clear factors
    banksel opt_x_s1+0
	clrf	opt_x_s1+0
	clrf	opt_x_s1+1
	clrf	opt_x_s2+0
	clrf	opt_x_s2+1
	clrf	opt_x_s3+0
	clrf	opt_x_s3+1
    banksel common
	return

compute_ppo2_analog:
    call    get_analog_inputs
    bra     compute_ppo2_common

	global	compute_ppo2			; compute mv_sensorX and ppo2_sensorX arrays
compute_ppo2:
    btfss   c3_hardware                 ; C3 hardware?
    return                              ; No

    btfss   s8_digital                  ; =1: Digital I/O
    bra     compute_ppo2_analog         ; use analog

    ; use digital
    btfss   new_s8_data_available       ; =1: New data frame recieved
    return
	call	compute_mvolts_for_all_sensors

compute_ppo2_common:
	; o2_mv_sensor1:2 * opt_x_s1:2 = o2_ppo2_sensor1/10000
	movff	o2_mv_sensor1+0,xA+0
	movff	o2_mv_sensor1+1,xA+1
	movff	opt_x_s1+0,xB+0
	movff	opt_x_s1+1,xB+1
	call	mult16x16		;xA:2*xB:2=xC:4
	movlw	LOW		.1000
	movwf	xB+0
	movlw	HIGH	.1000
	movwf	xB+1
	call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movlw	d'1'
	addwf	xC+0,F
	movlw	d'0'
	addwfc	xC+1,F
	movff	xC+0,o2_ppo2_sensor1+0
	movff	xC+1,o2_ppo2_sensor1+1		; result in 0.01bar
	; Set to zero if sensor is not active!
	btfss	sensor1_active
	clrf	o2_ppo2_sensor1+0
	btfss	sensor1_active
	clrf	o2_ppo2_sensor1+1

	; o2_mv_sensor2:2 * opt_x_s1:2 = o2_ppo2_sensor2/10000
	movff	o2_mv_sensor2+0,xA+0
	movff	o2_mv_sensor2+1,xA+1
	movff	opt_x_s2+0,xB+0
	movff	opt_x_s2+1,xB+1
	call	mult16x16		;xA:2*xB:2=xC:4
	movlw	LOW		.1000
	movwf	xB+0
	movlw	HIGH	.1000
	movwf	xB+1
	call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movlw	d'1'
	addwf	xC+0,F
	movlw	d'0'
	addwfc	xC+1,F
	movff	xC+0,o2_ppo2_sensor2+0
	movff	xC+1,o2_ppo2_sensor2+1		; result in 0.01bar
	; Set to zero if sensor is not active!
	btfss	sensor2_active
	clrf	o2_ppo2_sensor2+0
	btfss	sensor2_active
	clrf	o2_ppo2_sensor2+1

	; o2_mv_sensor3:2 * opt_x_s1:2 = o2_ppo2_sensor3/10000
	movff	o2_mv_sensor3+0,xA+0
	movff	o2_mv_sensor3+1,xA+1
	movff	opt_x_s3+0,xB+0
	movff	opt_x_s3+1,xB+1
	call	mult16x16		;xA:2*xB:2=xC:4
	movlw	LOW		.1000
	movwf	xB+0
	movlw	HIGH	.1000
	movwf	xB+1
	call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movlw	d'1'
	addwf	xC+0,F
	movlw	d'0'
	addwfc	xC+1,F
	movff	xC+0,o2_ppo2_sensor3+0
	movff	xC+1,o2_ppo2_sensor3+1		; result in 0.01bar
	; Set to zero if sensor is not active!
	btfss	sensor3_active
	clrf	o2_ppo2_sensor3+0
	btfss	sensor3_active
	clrf	o2_ppo2_sensor3+1

	return							; Done.


compute_mvolts_for_all_sensors:          ; Compute mV or all sensors (S8 Mode)
; compute AD results in 100�V steps (16bit/sensor)
; 24bit AD result is in 244,1406541nV
; Devide 24bit value through 409,5999512 -> 410 (0,01% error)
        #DEFINE	ad2mv_factor	.410
        ; Sensor 1
        clrf	xC+3
        movff	ir_buffer+.6,xC+2
        movff	ir_buffer+.5,xC+1
        movff	ir_buffer+.4,xC+0
        movlw	LOW		ad2mv_factor
        movwf	xB+0
        movlw	HIGH	ad2mv_factor
        movwf	xB+1
        call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
        movff	xC+1,o2_mv_sensor1+1
        movff	xC+0,o2_mv_sensor1+0		; in 100uV steps
        ; Sensor 2
        clrf	xC+3
        movff	ir_buffer+.9,xC+2
        movff	ir_buffer+.8,xC+1
        movff	ir_buffer+.7,xC+0
        movlw	LOW		ad2mv_factor
        movwf	xB+0
        movlw	HIGH	ad2mv_factor
        movwf	xB+1
        call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
        movff	xC+1,o2_mv_sensor2+1
        movff	xC+0,o2_mv_sensor2+0		; in 100uV steps
        ; Sensor 3
        clrf	xC+3
        movff	ir_buffer+.12,xC+2
        movff	ir_buffer+.11,xC+1
        movff	ir_buffer+.10,xC+0
        movlw	LOW		ad2mv_factor
        movwf	xB+0
        movlw	HIGH	ad2mv_factor
        movwf	xB+1
        call	div32x16  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
        movff	xC+1,o2_mv_sensor3+1
        movff	xC+0,o2_mv_sensor3+0		; in 100uV steps

        bcf     new_s8_data_available       ; Clear flag
        return                  ; Done.



	END