;=============================================================================
;
;   File ms5541.asm
;
;   Sensor subroutines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-03 : [mH] moving from OSTC code

#include    "hwos.inc"                  ; Mandatory header
#include 	"math.inc"					; Math routines

sensors     CODE

;=============================================================================
; Expose internal variables, to ease debug:
    global D1, D2
    global C1, C2, C3, C4, C5, C6
    global xdT, xdT2, OFF, SENS, amb_pressure_avg, temperature_avg

;=============================================================================
	global calculate_compensation
calculate_compensation:
	; xdT = D2 - C5    (s16 range -11.400 .. +12.350)
	movf   C5+0,W                       ;  Get Value to be subtracted
	subwf  D2+0,W             	        ;  Do the Low Byte
	movwf  xdT+0
	movf   C5+1,W                       ;  Then the high byte.
	subwfb D2+1,W
	movwf  xdT+1

    ; Second order temperature calculation
    ; xdT/128 is in range -89..+96, hence signed 8bit. dT/128 = (2*dT)/256
    rlcf    xdT+0,W                     ; put hit bit in carry.
    rlcf    xdT+1,W                     ; inject in high byte.
    movwf   isr_xA+0                    ; and put result in low byte.
    clrf    isr_xA+1
    btfsc   xdT+1,7                     ; If dT < 0
    setf    isr_xA+1                    ; then signextend to -1
	movff	isr_xA+0,isr_xB+0           ; copy A to B
	movff	isr_xA+1,isr_xB+1
	call	isr_signed_mult16x16        ; dT*dT --> xC (32 bits)

	; dT >= 0: divide by 8, ie. 3 shifts rights.
	; dT <  0: divide by 2, ie. 1 shifts rights.
    movlw   .3
	btfss	xdT+1,7                     ; Was dT negative ?
	movlw   .1
calc_loop_1:
    bcf     STATUS,C                    ;dT^2 is positive, so injected zeros.
    rrcf    isr_xC+1,F
    rrcf    isr_xC+0,F
    decfsz  WREG
    bra     calc_loop_1

    movf    isr_xC+0,W                  ; dT2 = dT - (dT/128)*(dT/128)/(2 ...or... 8)
	subwf   xdT+0,W
	movwf   xdT2+0
	movf    isr_xC+1,W
	subwfb  xdT+1,W
	movwf   xdT2+1

    ; Calculate OFF = C2 + ((C4-250)*dT2)/2^12 + 10000
    ; (range +9.246 .. +18.887)
    movff   C4+0,isr_xA+0
    movff   C4+1,isr_xA+1
;	movlw   LOW(-.250)                  ; C4 - 250 --> A
;	addwf	C4+0,W
;	movwf   isr_xA+0
;	movlw   -1                          ; HIGH(- .250) is not understood...
;	addwfc  C4+1,W
;	movwf   isr_xA+1
	
	movff   xdT2+0,isr_xB+0             ; dT2 --> B
	movff   xdT2+1,isr_xB+1
	call    isr_signed_mult16x16
    movlw   .12-.8                      ; A 12bit shift = 1 byte + 4 bits.
    call    isr_shift_C31

    movlw   LOW(.10000)                 ; Add 10000
    addwf   isr_xC+1,F
    movlw   HIGH(.10000)
    addwfc  isr_xC+2,F
    
    movf    C2+0,W                      ; Add C2, and save into OFF
	addwf   isr_xC+1,W
	movwf   OFF+0
	movf	C2+1,W
	addwfc  isr_xC+2,W
	movwf   OFF+1

    ; Calculate SENS = C1/2 + ((C3+200)*dT)/2^13 + 3000
    movlw   LOW(.200)                   ; C3+200 --> A
    addwf   C3+0,W
    movwf   isr_xA+0
    movlw   HIGH(.200)
    addwfc  C3+1,W
    movwf   isr_xA+1
                                        ; B still contains dT2
	call    isr_signed_mult16x16        ; A*B --> C
    movlw   .13-.8                      ; A 13bit shift = 1 byte + 5 bits.
    call    isr_shift_C31
    
    bcf     STATUS,C                    ; SENS = C1 / 2
    rrcf    C1+1,W
    movwf   SENS+1
    rrcf    C1+0,W
    movwf   SENS+0

    movlw   LOW(.3000)                  ; Add 3000
    addwf   isr_xC+1,F
    movlw   HIGH(.3000)
    addwfc  isr_xC+2,F

    movf    isr_xC+1,W                  ; And sum into SENS
    addwf   SENS+0,F
    movf    isr_xC+2,W
    addwfc  SENS+1,F

    ; calculate amb_pressure = (sens * (d1-off))/2^12 + 1000
    movf    OFF+0,W                      ; d1-off --> a
    subwf   D1+0,W
    movwf   isr_xA+0
    movf    OFF+1,W
    subwfb  D1+1,W
    movwf   isr_xA+1

	movff   SENS+0,isr_xB+0             ; sens --> b
	movff   SENS+1,isr_xB+1
	call    isr_signed_mult16x16
    movlw   .12-.8                      ; a 12bit shift = 1 byte + 4 bits.
    call    isr_shift_C31

    movlw   LOW(.1000)                  ; add 1000
    addwf   isr_xC+1,F
    movlw   HIGH(.1000)
    addwfc  isr_xC+2,F

    ; Add opt_pressure_adjust to result (SIGNED!)
    movff   opt_pressure_adjust,isr_xC+0

    btfss   isr_xC+0,7              ; <0?
    bra     pressure_extra_add      ; No
    ; Yes
    comf    isr_xC+0,F
    incf    isr_xC+0,F
    ; Check for max. of 20mbar
    movlw   .21
    cpfslt  isr_xC+0
    clrf    isr_xC+0
    ; Subtract
    movf    isr_xC+0,W
    subwf   isr_xC+1,F
    movlw   .0
    subwfb  isr_xC+2,F
    bra     pressure_extra_common

pressure_extra_add:
    ; Check for max. of 20mbar
    movlw   .21
    cpfslt  isr_xC+0
    clrf    isr_xC+0
    ; Add
    movf    isr_xC+0,W
    addwf   isr_xC+1,F
    movlw   .0
    addwfc  isr_xC+2,F

pressure_extra_common:
	banksel	common                      ; flag2 is in bank 1
	btfss	simulatormode_active		; are we in simulator mode?
	bra		calc_compensation_2			; no

    banksel isr_xC+2
    movlw   .5
    cpfsgt  isr_xC+2                    ; >1280mbar ?
    bra     pressure_extra_common2      ; No
    ; Yes, reset sim_pressure:2 to 1000mbar (End of sim)
    movlw   LOW     .1000
    movwf   sim_pressure+0
    movlw   HIGH    .1000
    movwf   sim_pressure+1

pressure_extra_common2:
	movff	sim_pressure+0,isr_xC+1	    ; override readings with simulator values
	movff	sim_pressure+1,isr_xC+2
	
calc_compensation_2:
	banksel	isr_backup
    movf    isr_xC+1,W                  ; Then sum_up to pressure averaging buffer.
    addwf   amb_pressure_avg+0,F
    movf    isr_xC+2,W
    addwfc  amb_pressure_avg+1,F

    ; calculate temp = 200 + dT*(C6+100)/2^11
    movlw   LOW(.100)                   ; C6 + 100 --> A
    addwf   C6+0,W
    movwf   isr_xA+0
    movlw   HIGH(.100)
    addwfc  C6+1,W
    movwf   isr_xA+1

    movff   xdT2+0,isr_xB+0             ; dT2 --> B
    movff   xdT2+1,isr_xB+1
	call    isr_signed_mult16x16        ; A*B
    movlw   .11-.8                      ; A 12bit shift = 1 byte + 3 bits.
    call    isr_shift_C31

    movlw   LOW(.200)                   ; Add 200
    addwf   isr_xC+1,F
    movlw   HIGH(.200)
    addwfc  isr_xC+2,F

    movf    isr_xC+1,W
    addwf   temperature_avg+0,F
    movf    isr_xC+2,W
    addwfc  temperature_avg+1,F

	return			                    ; Done.

;=============================================================================
	global	get_pressure_start
get_pressure_start:
	rcall	reset_MS5541
	movlw	b'10100000'	;+3*high as start and 1+low as stop!
get_pressure_start2:
	movwf	isr1_temp
	movlw	d'12'
	rcall	send_data_MS5541
	return

	global get_pressure_value
get_pressure_value:
	btfsc	MS5541_miso				; Conversion done?
	return							; No, Return
	rcall	get_2bytes_MS5541
	movff	dMSB,D1+1	
	movff	dLSB,D1+0
	return

;=============================================================================
	global	get_temperature_start
get_temperature_start:
	rcall	reset_MS5541
	movlw	b'10010000'	;+3*high as start and 1+low as stop!
	bra		get_pressure_start2	; continue in "get_pressure"

	global	get_temperature_value
get_temperature_value:
	btfsc	MS5541_miso				; Conversion done?
	return							; No, Return
	rcall	get_2bytes_MS5541
	movff	dMSB,D2+1
	movff	dLSB,D2+0
	return

;=============================================================================
	global	get_calibration_data
get_calibration_data:
	banksel	common
	bsf		no_sensor_int			; disable sensor interrupts
    banksel isr_backup              ; Back to Bank0 ISR data

	rcall	reset_MS5541
	movlw	b'01010100'	;+3*high as start and 1+low as stop!
	movwf	isr1_temp
	movlw	d'13'
	rcall	send_data_MS5541
	rcall	get_2bytes_MS5541
	movff	dMSB,ir_s8_buffer+1	
	movff	dLSB,ir_s8_buffer+0

	movlw	b'01011000'	;+3*high as start and 1+low as stop!
	movwf	isr1_temp
	movlw	d'13'
	rcall	send_data_MS5541
	rcall	get_2bytes_MS5541
	movff	dMSB,ir_s8_buffer+3	
	movff	dLSB,ir_s8_buffer+2

	movlw	b'01100100'	;+3*high as start and 1+low as stop!
	movwf	isr1_temp
	movlw	d'13'
	rcall	send_data_MS5541
	rcall	get_2bytes_MS5541
	movff	dMSB,ir_s8_buffer+5	
	movff	dLSB,ir_s8_buffer+4

	movlw	b'01101000'	;+3*high as start and 1+low as stop!
	movwf	isr1_temp
	movlw	d'13'
	rcall	send_data_MS5541
	rcall	get_2bytes_MS5541
	movff	dMSB,ir_s8_buffer+7	
	movff	dLSB,ir_s8_buffer+6

; calculate C1 (16Bit)
	movff	ir_s8_buffer+1, C1+1
	bcf		STATUS,C
	rrcf	C1+1
	bcf		STATUS,C
	rrcf	C1+1
	bcf		STATUS,C
	rrcf	C1+1
	movff	ir_s8_buffer+0, C1+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+1,0
	bcf		STATUS,C
	rrcf	C1+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+1,1
	bcf		STATUS,C
	rrcf	C1+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+1,2
	bcf		STATUS,C
	rrcf	C1+0

; calculate C2 (16Bit)
	movff	ir_s8_buffer+2, C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,0
	bcf		STATUS,C
	rrcf	C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,1
	bcf		STATUS,C
	rrcf	C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,2
	bcf		STATUS,C
	rrcf	C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,3
	bcf		STATUS,C
	rrcf	C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,4
	bcf		STATUS,C
	rrcf	C2+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+3,5
	bcf		STATUS,C
	rrcf	C2+0

	movff	ir_s8_buffer+3, C2+1
	bsf		STATUS,C
	btfss	ir_s8_buffer+0,0
	bcf		STATUS,C
	rrcf	C2+1
	bsf		STATUS,C
	btfss	ir_s8_buffer+0,1
	bcf		STATUS,C
	rrcf	C2+1
	bsf		STATUS,C
	btfss	ir_s8_buffer+0,2
	bcf		STATUS,C
	rrcf	C2+1
	bcf		STATUS,C
	rrcf	C2+1
	bcf		STATUS,C
	rrcf	C2+1
	bcf		STATUS,C
	rrcf	C2+1

; calculate C3 (16Bit)
	movff	ir_s8_buffer+5,C3+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+4,7
	bcf		STATUS,C
	rlcf	C3+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+4,6
	bcf		STATUS,C
	rlcf	C3+0
	clrf	C3+1
	btfsc	ir_s8_buffer+5,7
	bsf		C3+1,1
	btfsc	ir_s8_buffer+5,6
	bsf		C3+1,0
	
; calculate C4 (16Bit)	
	movff	ir_s8_buffer+7,C4+0
	bsf		STATUS,C
	btfss	ir_s8_buffer+6,7
	bcf		STATUS,C
	rlcf	C4+0
	clrf	C4+1
	btfsc	ir_s8_buffer+7,7
	bsf		C4+1,0

; C4=C4-250
	movlw   LOW(-.250)                  ; C4 - 250 --> C4
	addwf	C4+0,W
	movwf   C4+0
	movlw   -1                          ; HIGH(- .250) is not understood...
	addwfc  C4+1,W
	movwf   C4+1
	
; calculate C5 (16Bit)		
	movff	ir_s8_buffer+4,C5+0
	bcf		C5+0,6
	btfsc	ir_s8_buffer+2,0
	bsf		C5+0,6
	bcf		C5+0,7
	btfsc	ir_s8_buffer+2,1
	bsf		C5+0,7
	clrf	C5+1
	btfsc	ir_s8_buffer+2,2
	bsf		C5+1,0
	btfsc	ir_s8_buffer+2,3
	bsf		C5+1,1
	btfsc	ir_s8_buffer+2,4
	bsf		C5+1,2
	btfsc	ir_s8_buffer+2,5
	bsf		C5+1,3

    ; calculate C5 = UT1
    ; C5 = 8*C5 + 10000 (u16 range 10.000 .. +42.760)
	clrf	isr_xA+1
	movlw	d'8'
	movwf	isr_xA+0
	movff	C5+0,isr_xB+0
	movff	C5+1,isr_xB+1
	call	isr_unsigned_mult16x16      ;isr_xA*isr_xB=isr_xC
    movff   isr_xC+0,C5+0
    movff   isr_xC+1,C5+1
	movlw   LOW		d'10000'
	addwf   C5+0,F
	movlw   HIGH 	d'10000'
	addwfc  C5+1,F    ; = 8*C5 + 10000

; calculate C6 (16Bit)		
	clrf	C6+1
	movff	ir_s8_buffer+6,C6+0
	bcf		C6+0,7

	banksel	common
	bcf		no_sensor_int		; enable sensor interrupts
	bcf		pressure_refresh 		; Clear flag
    banksel isr_backup              ; Back to Bank0 ISR data

	clrf		sensor_state_counter			; Then reset State counter

	return

;=============================================================================
reset_MS5541_one:
	bsf		MS5541_mosi
    bra    send_clk_pulse  ; Send one high-low sequence on MS5541_clk  -> and return

reset_MS5541_zero:
	bcf		MS5541_mosi
    bra    send_clk_pulse  ; Send one high-low sequence on MS5541_clk  -> and return

reset_MS5541:
	rcall	reset_MS5541_one			;0
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero
	rcall	reset_MS5541_one
	rcall	reset_MS5541_zero			;15
	rcall	reset_MS5541_zero	
	rcall	reset_MS5541_zero	
	rcall	reset_MS5541_zero	
	rcall	reset_MS5541_zero	
	rcall	reset_MS5541_zero			;20
	return

get_2bytes_MS5541:
	movlw	d'8'
	movwf	clock_count
	rcall	recieve_loop
	movff	isr1_temp,dMSB

	movlw	d'8'
	movwf	clock_count
	rcall	recieve_loop
	movff	isr1_temp,dLSB
    bra    send_clk_pulse  ; Send one high-low sequence on MS5541_clk  -> and return
	;return

recieve_loop:
    rcall   send_clk_pulse  ; Send one high-low sequence on MS5541_clk
	btfss	MS5541_miso	;MSB first
	bcf		STATUS,C
	btfsc	MS5541_miso	;MSB first
	bsf		STATUS,C
	rlcf	isr1_temp,F
	decfsz	clock_count,F
	bra		recieve_loop
	return

send_clk_pulse:
	bsf		MS5541_clk
	nop
	nop
	nop
	nop
	nop
	nop
	bcf		MS5541_clk
	nop
	nop
	nop
	nop
    return

send_data_MS5541:
	movwf	clock_count     ; From WREG
	; send three startbits first
	bcf		MS5541_clk
	nop
	nop
	bsf		MS5541_mosi
	movlw	d'3'
	subwf	clock_count,F	; total bit counter
    rcall   send_clk_pulse  ; Send one high-low sequence on MS5541_clk
    rcall   send_clk_pulse  ; Send one high-low sequence on MS5541_clk
    rcall   send_clk_pulse  ; Send one high-low sequence on MS5541_clk
	; now send 8 bytes from isr_temp1 and fill-up with zeros
send_data_MS5541_2:
	bcf		MS5541_clk
	nop
	nop

	btfss	isr1_temp,7	;MSB first
	bcf		MS5541_mosi
	btfsc	isr1_temp,7	;MSB first
	bsf		MS5541_mosi

	bsf		MS5541_clk

	bcf		STATUS,C
	rlcf	isr1_temp,F
	nop
	nop
;	nop
;	nop
;	nop
;	nop
;	bcf		MS5541_clk

	decfsz	clock_count,F
	bra		send_data_MS5541_2
	bcf		MS5541_clk
	return

        END