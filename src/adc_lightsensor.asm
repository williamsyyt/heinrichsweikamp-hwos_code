;=============================================================================
;
;   File adc.asm
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-08 : [mH] moving from OSTC code

#include "hwos.inc"
#include "math.inc"
#include "wait.inc"
#include "eeprom_rs232.inc"
#include "i2c.inc"

sensors CODE

wait_adc:
	movwf	ADCON0
    nop
	bsf		ADCON0,1			; start ADC
wait_adc2:
	btfsc	ADCON0,1            ; Wait...
	bra		wait_adc2
    return

	global	get_battery_voltage
get_battery_voltage:			; starts ADC and waits until fnished
    btfss   battery_gauge_available
    bra     get_battery_voltage1     ; Normal ostc3 hardware

    call    lt2942_get_accumulated_charge
    call    lt2942_get_voltage
    
    tstfsz  batt_voltage+1		; <256mV?
    bra	    get_battery_voltage_noretry	; No

    ; Retry
    call    lt2942_get_accumulated_charge
    call    lt2942_get_voltage
    
get_battery_voltage_noretry:
    btfsc   divemode
    return                          ; Not in divemode

    bcf     cv_active
    bcf     cc_active
    bcf     LEDr
    bcf     TRISJ,2                 ; Chrg-Out output
    bsf     CHRG_OUT

    btfss   CHRG_IN
    bra     charge_cc_active

    bcf     CHRG_OUT
    bsf     TRISJ,2                 ; Chrg-Out high impedance

    WAITMS  d'1'

    btfsc   CHRG_IN
    return
;cv_active:
    decfsz  safety_stop_countdown,F
    return
    movlw   .15
    cpfsgt  batt_voltage+1          ; Batt Voltage >= 16*256mV (4,096V)?
    bra     charge_cc_active        ; No
    bsf     cc_active
    bsf     cv_active
    bsf     LEDr                    ; Indicate charging
    call    lt2942_charge_done      ; Reset accumulating registers to 0xFFFF
    WAITMS  d'10'
    bcf     LEDr                    ; Indicate charging
    bsf     safety_stop_countdown,0 ; =1
    return

charge_cc_active:
    bsf     cc_active
    bsf     LEDr                    ; Indicate charging
    bcf     CHRG_OUT
    bsf     TRISJ,2                 ; Chrg-Out high impedance
    movlw   .10
    movwf   safety_stop_countdown
    return

get_battery_voltage1:
    bsf     adc_running         ; =1: The ADC is in use
	movlw	b'00100000'			; 2.048V Vref+ -> 1LSB = 500µV
	movwf	ADCON1
	movlw	b'00011001'			; power on ADC, select AN6
	rcall   wait_adc

	movff	ADRESH,batt_voltage+1	; store value
	movff	ADRESL,batt_voltage+0	; store value
	bcf		ADCON0,0			; power off ADC

; Multiply with 2,006 to be excact here...
;	bcf		STATUS,C
;	rlcf	xA+0,F
;
;	rlcf	xA+1,F              ; x2

;	movff	xA+0,batt_voltage+0	; store value
;	movff	xA+1,batt_voltage+1

	movlw	LOW		lithium_36v_low
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_low
	movwf	sub_a+1
	movff	batt_voltage+0,sub_b+0
	movff	batt_voltage+1,sub_b+1
	call	subU16				; sub_c = sub_a - sub_b
; Battery is 3,6V (>lithium_36v_low?)
	btfss	neg_flag
    bra     get_battery_voltage4    ; No, use 1,5V

	bsf		battery_is_36v	; Yes, set flag (Cleared in power-on reset only!)

; Check if the battery is near-dead already
	movlw	LOW		lithium_36v_empty
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_empty
	movwf	sub_a+1
	call	subU16				; sub_c = sub_a - sub_b
; Battery is not dead yet (>lithium_36v_empty?)
	btfsc	neg_flag
    bra     get_battery_voltage2    ; Yes, battery is still ok

    ; Battery is probably dead very soon
    ; Set ">=24Ah used" into battery gauge registers
    movlw   .128
    movff   WREG,battery_gauge+5

get_battery_voltage2:
    ; Use 3,6V battery gauging mode
	movff	battery_gauge+5,xC+3
	movff	battery_gauge+4,xC+2
	movff	battery_gauge+3,xC+1
	movff	battery_gauge+2,xC+0
	; battery_gauge:6 is nAs
	; devide through 65536
	; devide through battery_capacity:2
	; Result is in percent
	movff	internal_battery_capacity+0,xB+0
	movff	internal_battery_capacity+1,xB+1
	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	xC+0,lo
    ; Limit to 100
    movlw   .100
    cpfslt  lo
    movwf   lo
	; lo will be between 0 (Full) and 100 (empty)
	movf	lo,W
	sublw	.100
	movwf	lo
get_battery_voltage3:
	movlw	.100
	cpfslt	lo
	movwf	lo
	; lo will be between 100 (Full) and 0 (empty)

; use 3,6V battery sensing based on 50mA load
; 75%
	movff	batt_voltage+0,sub_b+0
	movff	batt_voltage+1,sub_b+1
	movlw	LOW		lithium_36v_75
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_75
	movwf	sub_a+1
	call	subU16				; sub_c = sub_a - sub_b
	btfsc	neg_flag
    bra     get_battery_voltage3a
    movlw   .75
    movwf   lo
get_battery_voltage3a:
; 50%
	movlw	LOW		lithium_36v_50
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_50
	movwf	sub_a+1
	call	subU16				; sub_c = sub_a - sub_b
	btfsc	neg_flag
    bra     get_battery_voltage3b
    movlw   .50
    movwf   lo
get_battery_voltage3b:
    ; 25%
	movlw	LOW		lithium_36v_25
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_25
	movwf	sub_a+1
	call	subU16				; sub_c = sub_a - sub_b
	btfsc	neg_flag
    bra     get_battery_voltage3c
    movlw   .25
    movwf   lo
get_battery_voltage3c:
    ; 10%
	movlw	LOW		lithium_36v_10
	movwf	sub_a+0
	movlw	HIGH	lithium_36v_10
	movwf	sub_a+1
	call	subU16				; sub_c = sub_a - sub_b
	btfsc	neg_flag
    bra     get_battery_voltage3d
    movlw   .10
    movwf   lo
get_battery_voltage3d:
	movlw	.100
	cpfslt	lo
	movwf	lo
	; lo will be between 100 (Full) and 0 (empty)
    movf    batt_percent,W
    cpfsgt  lo                      ; keep batt_percent on the lowest value found
    movff   lo,batt_percent         ; store value
    btfsc   battery_is_36v          ; but always use computed value for 3,6V battery
    movff   lo,batt_percent         ; store value
    bcf     adc_running              ; =1: The ADC is in use
	return

get_battery_voltage4:
    ; Use 1,5V battery voltage mode
    ; Use approximation (batt_voltage:2-aa_15v_low)/4 = lo
	movff	batt_voltage+0,sub_a+0
	movff	batt_voltage+1,sub_a+1
   	movlw	LOW		aa_15v_low
    movwf	sub_b+0
	movlw	HIGH	aa_15v_low
	movwf	sub_b+1
	call	subU16				; sub_c = sub_a - sub_b
    bcf     STATUS,C
    rrcf    sub_c+1
    rrcf    sub_c+0             ; /2
    bcf     STATUS,C
    rrcf    sub_c+1
    rrcf    sub_c+0             ; /4
    movff   sub_c+0,lo
    bra     get_battery_voltage3d    ; Check limits and return

	global	get_ambient_level
get_ambient_level:              ; starts ADC and waits until finished
    btfsc   adc_running         ; ADC in use?
    return                      ; Yes, return

    btfsc   ambient_sensor
    bra     get_ambient_level1  ; Normal ostc3 hardware

  	banksel isr_backup              ; Back to Bank0 ISR data
	movff	opt_brightness,isr1_temp
	incf	isr1_temp,F				; adjust 0-2 to 1-3
	movlw	ambient_light_max_high_cr; cR and 2 hardware brightest setting
	dcfsnz	isr1_temp,F
	movlw	ambient_light_max_eco	; brightest setting
	dcfsnz	isr1_temp,F
	movlw	ambient_light_max_medium; brightest setting

	movff	WREG,ambient_light+0		; Set to max.
	movff	ambient_light+0,max_CCPR1L	; Store value for dimming in TMR7 interrupt
	return

get_ambient_level1:
	movlw	b'00000000'         ; Vref+ = Vdd
	movwf	ADCON1
	movlw	b'00011101'			; power on ADC, select AN7
	rcall   wait_adc

	movff	ADRESH,ambient_light+1
	movff	ADRESL,ambient_light+0
	bcf		ADCON0,0			; power off ADC

	; ambient_light:2 is between 4096 (direct sunlight) and about 200 (darkness)
	; First: Devide through 16
	bcf		STATUS,C
	rrcf	ambient_light+1
	rrcf	ambient_light+0
	bcf		STATUS,C
	rrcf	ambient_light+1
	rrcf	ambient_light+0
	bcf		STATUS,C
	rrcf	ambient_light+1
	rrcf	ambient_light+0
	bcf		STATUS,C
	rrcf	ambient_light+1
	rrcf	ambient_light+0
	; Result: ambient_light:2/16
	; Now, make sure to have value between ambient_light_low and ambient_light_max

    movlw   .254
    tstfsz  ambient_light+1         ; >255?
    movwf   ambient_light+0         ; avoid ADC clipping

    incfsz  ambient_light+0,W       ; =255?
    bra     get_ambient_level2      ; No, continue

    movlw   .254
    movwf   ambient_light+0         ; avoid ADC clipping

get_ambient_level2:
;    movlw   .10
;    subwf   ambient_light+0,F       ; Subtract 10 (ADC Offset)
;    btfsc   STATUS,N
;    movwf   ambient_light+0         ; avoid clipping

  	banksel isr_backup              ; Back to Bank0 ISR data
	movff	opt_brightness,isr1_temp

	btfsc	RCSTA1,7				; UART module on?
	clrf	isr1_temp				; Yes, set temporally to eco mode

	incf	isr1_temp,F				; adjust 0-2 to 1-3

    banksel common                  ; flag is in bank1
	movlw	ambient_light_max_high_cr; cR and 2 hardware brightest setting
    btfss   battery_gauge_available
    movlw	ambient_light_max_high_15V; 1,5V battery brightest setting
    btfsc	battery_is_36v          ; 3,6V battery in use?
	movlw	ambient_light_max_high_36V	; 3,6V battery brightest setting
	banksel isr_backup              ; Back to Bank0 ISR data

	dcfsnz	isr1_temp,F
	movlw	ambient_light_max_eco	; brightest setting	
	dcfsnz	isr1_temp,F
	movlw	ambient_light_max_medium; brightest setting		

	banksel common                  ; ambient_light is in Bank1
    incf    ambient_light+0,F       ; +1
	cpfslt	ambient_light+0			; smaller then WREG?
	movwf	ambient_light+0			; No, set to max.

	banksel isr_backup              ; Back to Bank0 ISR data
	movff	opt_brightness,isr1_temp
	incf	isr1_temp,F				; adjust 0-2 to 1-3
	movlw	ambient_light_min_high	; darkest setting

	dcfsnz	isr1_temp,F
	movlw	ambient_light_min_eco	; darkest setting
	dcfsnz	isr1_temp,F
	movlw	ambient_light_min_medium; darkest setting
	dcfsnz	isr1_temp,F
	movlw	ambient_light_min_high	; darkest setting
	
	banksel common                  ; ambient_light is in Bank1
	cpfsgt	ambient_light+0			; bigger then WREG?
	movwf	ambient_light+0			; No, set to min
	
	movff	ambient_light+0,max_CCPR1L	; Store value for dimming in TMR7 interrupt
	return

	global	get_analog_inputs
get_analog_inputs:			; starts ADC and waits until finished
    bsf     adc_running         ; =1: The ADC is in use
    btfsc   TFT_PWM
    bra     get_analog_inputs   ; Wait for PWM low
    movlw	b'00100000'			; 2.048V Vref+ -> 1LSB = 500µV
	movwf	ADCON1
	movlw	b'00100001'			; power on ADC, select AN8
	rcall   wait_adc
    bcf     STATUS,C
    rrcf    ADRESH,F                    ; /2
    rrcf    ADRESL,W
    ; add to o2_mv_sensor1:2
    addwf   o2_mv_sensor1+0,F
    movf    ADRESH,W
    addwfc  o2_mv_sensor1+1,F
    ; Devide by 2
    bcf     STATUS,C
    rrcf    o2_mv_sensor1+1,F           ; /2
    rrcf    o2_mv_sensor1+0,F

    movlw   HIGH    ignore_mv
    cpfsgt  o2_mv_sensor1+1     ; >ignore_mv?
    bra     get_analog_inputs2a ; No
    ; Yes, ignore this reading
    clrf    o2_mv_sensor1+1
    clrf    o2_mv_sensor1+0
get_analog_inputs2a:
    ; Ignore 1,9mV noise for not-connected inputs
    tstfsz  o2_mv_sensor1+1     ; >25,5mV?
    bra     get_analog_inputs2  ; Yes, skip here
    movlw   .19
    cpfsgt  o2_mv_sensor1+0     ; >1,9mV?
    clrf    o2_mv_sensor1+0     ; no, clear result
get_analog_inputs2:
	movlw	b'00100101'			; power on ADC, select AN9
	rcall   wait_adc
    bcf     STATUS,C
    rrcf    ADRESH,F                    ; /2
    rrcf    ADRESL,W
    ; add to o2_mv_sensor2:2
    addwf   o2_mv_sensor2+0,F
    movf    ADRESH,W
    addwfc  o2_mv_sensor2+1,F
    ; Devide by 2
    bcf     STATUS,C
    rrcf    o2_mv_sensor2+1,F           ; /2
    rrcf    o2_mv_sensor2+0,F

    movlw   HIGH    ignore_mv
    cpfsgt  o2_mv_sensor2+1     ; >ignore_mv?
    bra     get_analog_inputs3a ; No
    ; Yes, ignore this reading
    clrf    o2_mv_sensor2+1
    clrf    o2_mv_sensor2+0
get_analog_inputs3a:
    ; Ignore 1,9mV noise for not-connected inputs
    tstfsz  o2_mv_sensor2+1     ; >25,5mV?
    bra     get_analog_inputs3  ; Yes, skip here
    movlw   .19
    cpfsgt  o2_mv_sensor2+0     ; >1,9mV?
    clrf    o2_mv_sensor2+0     ; no, clear result
get_analog_inputs3:
	movlw	b'00101001'			; power on ADC, select AN10
	rcall   wait_adc
    bcf     STATUS,C
    rrcf    ADRESH,F                    ; /2
    rrcf    ADRESL,W
    ; add to o2_mv_sensor3:2
    addwf   o2_mv_sensor3+0,F
    movf    ADRESH,W
    addwfc  o2_mv_sensor3+1,F
    ; Devide by 2
    bcf     STATUS,C
    rrcf    o2_mv_sensor3+1,F           ; /2
    rrcf    o2_mv_sensor3+0,F

    movlw   HIGH    ignore_mv
    cpfsgt  o2_mv_sensor3+1     ; >ignore_mv?
    bra     get_analog_inputs4a ; No
    ; Yes, ignore this reading
    clrf    o2_mv_sensor3+1
    clrf    o2_mv_sensor3+0
get_analog_inputs4a:
    ; Ignore 1,9mV noise for not-connected inputs
    tstfsz  o2_mv_sensor3+1     ; >25,5mV?
    bra     get_analog_inputs4  ; Yes, skip here
    movlw   .19
    cpfsgt  o2_mv_sensor3+0     ; >1,9mV?
    clrf    o2_mv_sensor3+0     ; no, clear result
get_analog_inputs4:
	bcf		ADCON0,0			; power off ADC
    bcf     adc_running         ; =1: The ADC is in use
    return

    global  piezo_config            ; Sets up piezo sensitivity of heinrichs weikamp Piezo buttons (~30ms)
piezo_config:   ; Settings between 20 and 200
        clrf	TMR5H
	clrf	TMR5L	    ; ~2sec
	bcf		PIR5,TMR5IF			; Clear flag
	bcf	switch_right
	bcf	switch_left
piezo_config0:
	btfsc	switch_right
	bra	piezo_config
	btfsc	switch_left
	bra	piezo_config		; Restart on button press
    
	btfss	PIR5,TMR5IF
	bra	piezo_config0			; Wait loop

    bcf	    INTCON,GIE
    movff   opt_cR_button_right,WREG; right button
    btfsc   flip_screen             ; 180° rotation ?
    movff   opt_cR_button_left,WREG ; Yes, left button
    rcall   piezo_config_tx

    movff   opt_cR_button_left,WREG ; left button
    btfsc   flip_screen             ; 180° rotation ?
    movff   opt_cR_button_right,WREG; Yes, right button
    rcall   piezo_config_tx

    movlw   .20                    ; reserved
    rcall   piezo_config_tx
    movlw   .20                    ; reserved
    rcall   piezo_config_tx
    bsf	    INTCON,GIE
    return

piezo_config_tx:                    ; Send one byte
    movwf   uart1_temp              ; Store byte
    movlw   .8
    movwf   uart2_temp              ; Bit counter
    bcf     TX3_PIEZO_CFG           ; Startbit
    rcall   piezo_config_wait_bit
piezo_config_tx_loop:
    btfss   uart1_temp,0            ; LSB first
    bcf     TX3_PIEZO_CFG
    btfsc   uart1_temp,0            ; LSB first
    bsf     TX3_PIEZO_CFG
    rcall   piezo_config_wait_bit
    rrncf   uart1_temp,F
    decfsz  uart2_temp,F
    bra     piezo_config_tx_loop
    bsf     TX3_PIEZO_CFG           ; Stopbit
    rcall   piezo_config_wait_bit
    return

piezo_config_wait_bit:
    setf	TMR5H
	movlw	.255-.26 			;26 x 31,5µs = 819us
	movwf	TMR5L
	bcf		PIR5,TMR5IF			; Clear flag
piezo_config_wait_bit3:
    btfss	PIR5,TMR5IF
	bra		piezo_config_wait_bit3			; Wait loop
	return

    global  reset_battery_pointer, reset_battery_internal_only
reset_battery_pointer:       ; Resets battery pointer 0x07-0x0C and battery_gauge:5
	extern  lt2942_charge_done
    btfsc   battery_gauge_available            ; Something to reset?
    call    lt2942_charge_done      ; Yes, reset accumulating registers to 0xFFFF
reset_battery_internal_only:
    clrf	EEADRH
    clrf	EEDATA					; Delete to zero
    write_int_eeprom 0x07
    write_int_eeprom 0x08
    write_int_eeprom 0x09
    write_int_eeprom 0x0A
    write_int_eeprom 0x0B
    write_int_eeprom 0x0C
    banksel battery_gauge+0
    clrf    battery_gauge+0
    clrf    battery_gauge+1
    clrf    battery_gauge+2
    clrf    battery_gauge+3
    clrf    battery_gauge+4
    clrf    battery_gauge+5
    banksel common
    movlw   .100
    movwf   batt_percent
    return

    global	get_analog_switches
get_analog_switches:              ; starts ADC and waits until finished
    btfsc   analog_switches
    bra	    get_analog_switches2
    ; no analog switches
    bcf		analog_sw2_pressed
    bcf		analog_sw1_pressed
    return	; Done.
get_analog_switches2:    
    btfsc   adc_running         ; ADC in use?
    return                      ; Yes, return
    
    movlw	b'00001001'	    ; left justified
    movwf	ADCON2
;    movlw	b'00000000'         ; Vref+ = Vdd
    clrf	ADCON1
    movlw	b'00100101'	    ; power on ADC, select AN9
    rcall	wait_adc
    banksel	analog_counter
    movff	ADRESH,WREG
    addwf	analog_sw2_raw+0
    movlw	.0
    addwfc	analog_sw2_raw+1
    decfsz	analog_counter,F    ; continue averaging?
    bra		get_analog_switches2a	; Yes
    ; Done. Compute average
    bcf     STATUS,C
    rrcf    analog_sw2_raw+1
    rrcf    analog_sw2_raw+0    ; /2
    bcf     STATUS,C
    rrcf    analog_sw2_raw+1
    rrcf    analog_sw2_raw+0    ; /4
    bcf     STATUS,C
    rrcf    analog_sw2_raw+1
    rrcf    analog_sw2_raw+0    ; /8
    bcf     STATUS,C
    rrcf    analog_sw2_raw+1
    rrcf    analog_sw2_raw+0    ; /16
    movff   analog_sw2_raw+0, analog_sw2
    clrf    analog_sw2_raw+1
    clrf    analog_sw2_raw+0	; Reset average registers
;    movlw   .16
;    movwf   analog_counter	; only once...
get_analog_switches2a:    
    banksel	common
    bcf		analog_sw2_pressed
    movff	opt_cR_button_left,WREG		;20-100
    bcf		STATUS,C
    rrcf	WREG		;/2 -> 10-50
    bcf		STATUS,C
    rrcf	WREG		;/2 -> 5-25
    decf	WREG,W	    	;-1
    decf	WREG,W	    	;-1
    decf	WREG,W	    	;-1 -> 2-22
    banksel	analog_sw2
    btfss	button_polarity,1;(1= normal, 0=inverted)
    bra		sw2_inverted
    addwf	analog_sw2,W 	; average (~128)
    cpfsgt	ADRESH
    bra		get_analog_sw1
    banksel	common
    bsf		analog_sw2_pressed	; Left button normal
    bra		get_analog_sw1
sw2_inverted:
    subwf	analog_sw2,W 	; average (~128)
    cpfslt	ADRESH
    bra		get_analog_sw1
    banksel	common
    bsf		analog_sw2_pressed	; Left button inverted
get_analog_sw1:
    banksel	common
    movlw	b'00101001'	    ; power on ADC, select AN10
    rcall	wait_adc
    banksel	analog_counter
    movff	ADRESH,WREG
    addwf	analog_sw1_raw+0
    movlw	.0
    addwfc	analog_sw1_raw+1
    tstfsz	analog_counter    ; continue averaging?
    bra		get_analog_switches1a	; Yes
    ; Done. Compute average
    bcf     STATUS,C
    rrcf    analog_sw1_raw+1
    rrcf    analog_sw1_raw+0    ; /2
    bcf     STATUS,C
    rrcf    analog_sw1_raw+1
    rrcf    analog_sw1_raw+0    ; /4
    bcf     STATUS,C
    rrcf    analog_sw1_raw+1
    rrcf    analog_sw1_raw+0    ; /8
    bcf     STATUS,C
    rrcf    analog_sw1_raw+1
    rrcf    analog_sw1_raw+0    ; /16
    movff   analog_sw1_raw+0, analog_sw1
    clrf    analog_sw1_raw+1
    clrf    analog_sw1_raw+0	; Reset average registers
    movlw   .16
    movwf   analog_counter	; only once...
get_analog_switches1a:    
    banksel	common
    bcf		analog_sw1_pressed
    movff	opt_cR_button_right,WREG		;20-100
    bcf		STATUS,C
    rrcf	WREG		;/2 -> 10-50
    bcf		STATUS,C
    rrcf	WREG		;/2 -> 5-25
    decf	WREG,W	    	;-1
    decf	WREG,W	    	;-1
    decf	WREG,W	    	;-1 -> 2-22
    banksel	analog_sw1
    btfss	button_polarity,0;(1= normal, 0=inverted)
    bra		sw1_inverted
    addwf	analog_sw1,W 	; average (~128)
    cpfsgt	ADRESH
    bra		get_analog_sw_done
    banksel	common
    bsf		analog_sw1_pressed	; right button normal
    bra		get_analog_sw_done
sw1_inverted:
    subwf	analog_sw1,W 	; average (~128)
    cpfslt	ADRESH
    bra		get_analog_sw_done
    banksel	common
    bsf		analog_sw1_pressed	; right button inverted
get_analog_sw_done:
    banksel	common
    movlw	b'10001101'	    ; Restore to right justified
    movwf	ADCON2
    btfsc	analog_sw1_pressed
    return
    btfsc	analog_sw2_pressed
    return
    setf	TMR1H		; No button pressed, enhance timer1 to overflow quickly
    return

	END