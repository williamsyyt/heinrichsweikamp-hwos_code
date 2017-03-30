;=============================================================================
;
;   File sleepmode.asm
;
;   Sleepmode
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-12 : [mH] moving from OSTC code

#include    "hwos.inc"                  ; Mandatory header
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include	"surfmode.inc"
#include	"tft.inc"
#include	"isr.inc"
#include	"start.inc"
#include	"adc_lightsensor.inc"
#include	"math.inc"
#include	"ms5541.inc"
#include	"wait.inc"
#include	"eeprom_rs232.inc"
#include 	"external_flash.inc"
#include	"ghostwriter.inc"
#include    "i2c.inc"
#include    "mcp.inc"


    extern  vault_decodata_into_eeprom

gui     CODE

	global	sleeploop
sleeploop:							; enter sleepmode!
    call    disable_ir_s8           ; IR/S8 off
    call    mcp_sleep
	bcf	LEDg
	bcf	LEDr
	movff   menupos3,customview_surfmode; save last customview
	call	TFT_Display_FadeOut
	call	TFT_DisplayOff			; display off
	bcf     enable_screen_dumps     ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
	call	disable_rs232			; USB off
	call    vault_decodata_into_eeprom  ; store deco data
	call	ext_flash_enable_protection	; enable write protection for external flash
	call	update_battery_registers	; update battery registers into EEPROM
	clrf	divemins+0
	clrf	divemins+1
	clrf	apnoe_max_pressure+0
	clrf	apnoe_max_pressure+1
	call	speed_normal
	bsf	no_sensor_int			; No sensor interrupt
	clrf	ADCON0				; Power-Down ADC Module
sleeploop_pre:
	bcf	deep_sleep			; Normal sleepmode
	call    I2C_sleep_accelerometer
	call    I2C_sleep_compass
        btfss   analog_switches
        bra	sleeploop_loop     ; no analog switches

	bsf	power_sw1
	bsf	power_sw2
	movlw	.4				; Wait for button circuity
	movwf	apnoe_max_pressure+0		; Used as temp
	bcf	onesecupdate
sleeploop_pre1:
	rcall	sleepmode_sleep
	btfss	onesecupdate			; Wait 1 second
	bra	sleeploop_pre1
	bcf	onesecupdate
	decfsz	apnoe_max_pressure+0,F
	bra	sleeploop_pre1

	movlw	.32				; Wait for button circuity
	movwf	apnoe_max_pressure+0		; Used as temp
sleeploop_pre2:	
	call	get_analog_switches
	decfsz	apnoe_max_pressure+0,F
	bra	sleeploop_pre2
	
	bcf	PIR1,TMR1IF
	bcf	INTCON,INT0IF
	bcf	INTCON3,INT1IF
	bcf	PIR5,TMR7IF
	bcf	switch_left
	bcf	switch_right
	bcf	analog_sw2_pressed
	bcf	analog_sw1_pressed
	bsf	PIE1,0		; (Re)Start Timer 1 Interrupt
	bsf	PIE2,1		; (Re)Start Timer 2 Interrupt
	bsf	PIE5,3		; (Re)Start Timer 7 Interrupt
	bsf	INTCON,4	; (Re)Start INT0 Interrupt
	bsf	INTCON3,3	; (Re)Start INT1 Interrupt

sleeploop_loop:
	btfsc	onesecupdate			; one second in sleep?
	rcall	onesec_sleep			; check switches, check pressure sensor, etc.

	btfss	sleepmode			; wake up? (This bit will be set in other routines)
	goto	restart				; yes
	
	btfsc	deep_sleep			; Enter deep sleep?
	bra	deepsleep			; Yes
no_deepsleep:
    
	rcall 	sleepmode_sleep			; Wait at least 35ms (every 62,5ms Timer7 wakeup)

	; Any button pressed in sleep?
;	btfsc	switch_left	
;	rcall	onesec_sleep1a
;	btfsc	switch_right
;	rcall	onesec_sleep1a
;
;	btfss	sleepmode				; wake up? (This bit will be set in other routines)
;	goto	restart					; yes

	bra		sleeploop_loop			; do loop until someting happens

deepsleep:
	btfss   analog_switches
        bra	no_deepsleep			; no analog switches, no deep sleep required

	bcf	PIE1,0		; Stop Timer 1 Interrupt
	bcf	PIE2,1		; Stop Timer 2 Interrupt
	bcf	PIE5,3		; Stop Timer 7 Interrupt
	bcf	INTCON,4	; Stop INT0 Interrupt
	bcf	INTCON3,3	; Stop INT1 Interrupt
	bcf	power_sw1
	bcf	power_sw2
	rcall	deepsleep_get_accel	    ; Read accelerometer into WREG
	movwf	apnoe_max_pressure+0	    ; Store init value
    
deepsleep_loop:	
    	btfsc	onesecupdate			; one second in sleep?
	rcall	onesec_deepsleep		; check accelerometer

	btfsc	onesecupdate			; one second in sleep?
	rcall	onesec_sleep			; check switches, check pressure sensor, etc.

	rcall	sleepmode_sleep
    
	btfss	deep_sleep			; Enter normal sleepmode?
	bra	sleeploop_pre			; Yes

	bra	deepsleep_loop			; do loop until someting happens

onesec_deepsleep:
	rcall	deepsleep_get_accel	    ; Read accelerometer into WREG
	subwf	apnoe_max_pressure+0,W	    ; apnoe_max_pressure+0 - accel_DZ+0 -> WREG
	btfsc	STATUS,N		    ; Result negative?
	negf	WREG			    ; Yes, negate it
	movwf	apnoe_max_pressure+1	    ; change of acceleration in Z-axis
	movlw	.50			    ; Threshold (mg)
	cpfslt	apnoe_max_pressure+1	    ; bigger then the threshold?
	bcf	deep_sleep		    ; Yes!

;	extern	piezo_config_tx
;	movff	apnoe_max_pressure+0,WREG
;	call	piezo_config_tx
;	movff	accel_DZ+0,WREG
;	call	piezo_config_tx
;	movff	apnoe_max_pressure+1,WREG
;	call	piezo_config_tx
	return
	
deepsleep_get_accel:
	call    I2C_init_compass
	call    I2C_RX_accelerometer        ; read Accelerometer
	call    I2C_sleep_compass
	movff	accel_DZ+0,WREG
	return
	
onehour_sleep:
    call	update_battery_registers    ; update battery registers into EEPROM
    call    vault_decodata_into_eeprom  ; update deco data
    bcf		onehourupdate               ; all done
    return

onemin_sleep:
	btfsc	onehourupdate		; one hour in sleep?
	rcall   onehour_sleep           ; Yes

	btfsc   battery_gauge_available
	call    get_battery_voltage     ; Check for charger
	
    ;---- adjust airpressure compensation any 15 minutes
	incf	divemins+1,F			; counts to 14...
	movlw	d'14'
	cpfsgt	divemins+1
	bra		onemin_sleep2			; 15 minutes not done!

; Tasks every 15 minutes in sleep
	bsf	deep_sleep		; enter deep-sleep mode

	clrf	divemins+1				; reset counter

	call	deco_calc_CNS_decrease_15min		; compute CNS decay in sleep only
	banksel	common

	SAFE_2BYTE_COPY last_surfpressure_15min, last_surfpressure_30min		; save older airpressure
	SAFE_2BYTE_COPY amb_pressure, last_surfpressure_15min		; save new airpressure

	movlw	LOW		max_surfpressure
	movff	WREG,sub_a+0				; max. "allowed" airpressure in mbar
	movlw	HIGH	max_surfpressure
	movff	WREG,sub_a+1				; max. "allowed" airpressure in mbar
	movff	last_surfpressure_15min+0,sub_b+0
	movff	last_surfpressure_15min+1,sub_b+1
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag                ; Is 1080mbar < amb_pressure ?
	bra		onemin_sleep2			; NO: current airpressure is lower then "allowed" airpressure, ok!

    ; not ok! Overwrite with max. "allowed" airpressure
	movlw	LOW		max_surfpressure
	movff	WREG,last_surfpressure_15min+0	; max. "allowed" airpressure in mbar
	movlw	HIGH	max_surfpressure
	movff	WREG,last_surfpressure_15min+1	; max. "allowed" airpressure in mbar

onemin_sleep2:
; Tasks every minute in sleep
    SAFE_2BYTE_COPY amb_pressure, int_I_pres_respiration ; LOW copy pressure to deco routine
	call	deco_calc_wo_deco_step_1_min	; "calc_tissue_sleep"
	banksel		common

	bcf		oneminupdate			; all done
	return

onesec_sleep:
	btfsc	oneminupdate			; one minute in sleep?
	rcall	onemin_sleep			; do oneminute tasks, e.g. calculate desaturation

	btfsc   battery_gauge_available
	call    get_battery_voltage     ; Check for charger

	incf	divemins+0,F 			; counts to #test_pressure_in_sleep (10)
	movlw	d'10'
	cpfsgt	divemins+0			; here: temp variable
	bra	onesec_sleep1			; #test_pressure_in_sleep not done yet

	clrf	divemins+0				; clear counter
	rcall	pressuretest_sleep_fast	; Gets pressure without averaging (faster!)
        ; compare current ambient pressure with wake_up_from_sleep
	movlw	LOW	wake_up_from_sleep
	movwf	sub_a+0					; power on if ambient pressure is greater threshold
	movlw	HIGH	wake_up_from_sleep
	movwf	sub_a+1					; power on if ambient pressure is greater threshold
	SAFE_2BYTE_COPY amb_pressure, sub_b
	call	subU16					; Is (1160mbar - averaged(amb_pressure)) < 0 ?
	btfsc	neg_flag				; Wake up from Sleep?
	bra		onesec_sleep1a			; Yes, skip button checks, wake up!

    btfsc   battery_gauge_available
    bra     onesec_sleep1           ; No wake-up with cR hardware
    btfsc   vusb_in                 ; USB plugged in?
 	bra		onesec_sleep1a			; Yes, skip button checks, wake up!

onesec_sleep1:
	bcf	onesecupdate			; all done.
; Check switches
	btfsc	switch_left
	bra	onesec_sleep1a
	btfsc	switch_right
	bra	onesec_sleep1a				
; No button pressed
;	bcf		INTCON,INT0IF				; Clear flag
;	bcf		INTCON3,INT1IF				; Clear flag
	return

onesec_sleep1a:	; At least one button pressed or amb_pressure > wake_up_from_sleep
;	bcf		INTCON,INT0IF				; Clear flag
;	bcf		INTCON3,INT1IF				; Clear flag
	bcf		sleepmode                   ; wake up!
	SAFE_2BYTE_COPY last_surfpressure_30min, amb_pressure	; copy for compatibility
	movlw	.0
	movff	WREG,sensor_state_counter	; Reset sensor state counter
	bcf		no_sensor_int				; normal sensor interrupt mode
	return
	
pressuretest_sleep_fast:				; Get pressure without averaging (Faster to save some power in sleep mode)
	banksel 	isr_backup              ; Back to Bank0 ISR data
	clrf		amb_pressure_avg+0  	; pressure average registers
	clrf		amb_pressure_avg+1
	clrf		temperature_avg+0
	clrf		temperature_avg+1
	call		get_temperature_start	; and start temperature integration (73,5us)
	banksel		common
	rcall 		sleepmode_sleep			; Wait at least 35ms (every 62,5ms Timer7 wakeup)
	rcall 		sleepmode_sleep			; Wait at least 35ms (every 62,5ms Timer7 wakeup)
	banksel 	isr_backup              ; Back to Bank0 ISR data
	call		get_temperature_value	; State 1: Get temperature	
	call		get_pressure_start	 	; Start pressure integration.
	banksel 	common
	rcall 		sleepmode_sleep			; Wait at least 35ms (every 62,5ms Timer7 wakeup)
	rcall 		sleepmode_sleep			; Wait at least 35ms (every 62,5ms Timer7 wakeup)
    bsf         LEDg                    ; Show some activity
	banksel 	isr_backup              ; Back to Bank0 ISR data
	call		get_pressure_value		; State2: Get pressure (51us)
	call		calculate_compensation		; calculate temperature compensated pressure (27us)
	banksel 	common
    SAFE_2BYTE_COPY amb_pressure_avg, amb_pressure	; copy for compatibility
    bcf         LEDg
	return

sleepmode_sleep:
	banksel 	0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
	clrf		T7GCON				; Reset Timer7 Gate Control register
	movlw		b'10001101'			; 1:1 Prescaler -> 2seconds@32768Hz, not synced
	movwf		T7CON
	sleep
	sleep
	clrf		T7GCON				; Reset Timer7 Gate Control register
	movlw		b'10001001'			; 1:1 Prescaler -> 2seconds@32768Hz, synced
	movwf		T7CON
    banksel 	common              ; Bank1
	return

 END