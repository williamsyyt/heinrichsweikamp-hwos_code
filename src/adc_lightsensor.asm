;=============================================================================
;
;   File adc.asm
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-08 : [mH] moving from OSTC code

#include "ostc3.inc"
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
    btfss   c3_hardware
    bra     get_battery_voltage1     ; Normal ostc3 hardware

    call    lt2942_get_accumulated_charge
    call    lt2942_get_voltage
    bcf     LEDr
    bcf     TRISJ,2                 ; Chrg-Out output
    bsf     CHRG_OUT

    btfss   CHRG_IN
    bra     cc_active

    bcf     CHRG_OUT
    bsf     TRISJ,2                 ; Chrg-Out high impedance

    WAITMS  d'1'

    btfsc   CHRG_IN
    return
;cv_active:
    bsf     LEDr                    ; Indicate charging
    call    lt2942_charge_done      ; Reset accumulating registers to 0xFFFF
    WAITMS  d'10'
    bcf     LEDr                    ; Indicate charging
    return

cc_active:
    bsf     LEDr                    ; Indicate charging
    bcf     CHRG_OUT
    bsf     TRISJ,2                 ; Chrg-Out high impedance
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
	; devide through 364
	; Result is in percent of a 2,4Ah Battery
	movlw	LOW		.364
	movwf	xB+0
	movlw	HIGH	.364
	movwf	xB+1
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
;    btfsc   battery_is_36v          ; but always use computed value for 3,6V battery
;    movff   lo,batt_percent         ; store value
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

    btfss   c3_hardware
    bra     get_ambient_level1  ; Normal ostc3 hardware
    movlw   .250
    movwf   ambient_light+0
    clrf    ambient_light+1     ; Set to max
    bra     get_ambient_level2  ; Continue as normal

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
  	banksel isr_backup              ; Back to Bank0 ISR data
	movff	opt_brightness,isr1_temp

	btfsc	RCSTA1,7				; UART module on?
	clrf	isr1_temp				; Yes, set temporally to eco mode

	incf	isr1_temp,F				; adjust 0-2 to 1-3

    banksel common                  ; flag is in bank1
	movlw	ambient_light_max_high_c3; c3 hardware brightest setting
    btfss   c3_hardware
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
    ; Ignore 1,2mV noise for not-connected inputs
    tstfsz  o2_mv_sensor1+1     ; >25,5mV?
    bra     get_analog_inputs2  ; Yes, skip here
    movlw   .12
    cpfsgt  o2_mv_sensor1+0     ; >1,2mV?
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
    ; Ignore 1,2mV noise for not-connected inputs
    tstfsz  o2_mv_sensor2+1     ; >25,5mV?
    bra     get_analog_inputs3  ; Yes, skip here
    movlw   .12
    cpfsgt  o2_mv_sensor2+0     ; >1,2mV?
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
    ; Ignore 1,2mV noise for not-connected inputs
    tstfsz  o2_mv_sensor3+1     ; >25,5mV?
    bra     get_analog_inputs4  ; Yes, skip here
    movlw   .12
    cpfsgt  o2_mv_sensor3+0     ; >1,2mV?
    clrf    o2_mv_sensor3+0     ; no, clear result
get_analog_inputs4:
	bcf		ADCON0,0			; power off ADC
    bcf     adc_running         ; =1: The ADC is in use
    return

    global  piezo_config            ; Sets up piezo sensitivity of heinrichs weikamp Piezo buttons (~30ms)
piezo_config:   ; Settings between 20 and 200
    movlw   .75                     ; right button
    rcall   piezo_config_tx
    movlw   .75                     ; left button
    rcall   piezo_config_tx
    movlw   .200                    ; reserved
    rcall   piezo_config_tx
    movlw   .200                    ; reserved
    rcall   piezo_config_tx
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

    global  reset_battery_pointer
reset_battery_pointer:       ; Resets battery pointer 0x07-0x0C and battery_gauge:5
	extern  lt2942_charge_done
    call    lt2942_charge_done                 ; Reset accumulating registers to 0xFFFF
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


	END