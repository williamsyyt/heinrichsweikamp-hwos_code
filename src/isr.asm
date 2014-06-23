;=============================================================================
;
;   File isr.asm
;
;   INTERUPT subroutines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-05-24 : [jDG] Cleanups from initial Matthias code.

#include "ostc3.inc"
#include "shared_definitions.h"         ; Mailbox from/to p2_deco.c
#include "ms5541.inc"
#include "adc_lightsensor.inc"

;=============================================================================

	extern start

isr_high    CODE    0x0008              ;High Priority Interrups
		bra		HighInt
		nop	
		nop
		nop
		nop
		nop
		nop
		bra		HighInt

isr_low     CODE    0x00018             ;Low Priority Interrups
;	*** low priority interrupts not used
		retfie FAST						; Restores BSR, STATUS and WREG

HighInt:
        movff   PRODL,isr_prod+0
        movff   PRODH,isr_prod+1

; Buttons
		btfsc	PIR1,TMR1IF             ; Timer1 INT (Button hold-down Timer)
		rcall	timer1int
		btfsc	INTCON,INT0IF			; Buttons
		rcall	isr_switch_right
		btfsc	INTCON3,INT1IF			; Buttons
		rcall	isr_switch_left

; IR-Link
        btfsc   PIR3,RC2IF              ; UART2
        rcall   isr_uart2               ; IR-Link
		btfsc	PIR2,TMR3IF				; Timer 3
		rcall	isr_timer3				; IR-Link Timeout

; Pressure sensor and others
		btfsc	PIR5,TMR7IF				; Timer 7
		rcall	isr_tmr7        		; Every 62,5ms

;; IR-Link (again)
;        btfsc   PIR3,RC2IF              ; UART2
;        rcall   isr_uart2               ; IR-Link

; RTCC
		btfsc	PIR3,RTCCIF				; Real-time-clock interrupt
		rcall	isr_rtcc                ; May return in bank common!

        movff   isr_prod+1,PRODH
        movff   isr_prod+0,PRODL
		retfie FAST						; Restores BSR, STATUS and WREG

;=============================================================================

isr_uart2:               ; IR-Link
        banksel RCREG2
        movf    RCREG2,W
    	bcf		RCSTA2,CREN		; Clear receiver status
    	bsf		RCSTA2,CREN
        banksel isr_backup
        bcf     PIR3,RC2IF              ; Clear flag
        incf    ir_counter,F            ; Increase counter
        movff   ir_counter,isr1_temp    ; Copy
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.0
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.1
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.2
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.3
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.4
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.5
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.6
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.7
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.8
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.9
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.10
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.11
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.12
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.13
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.14
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.15
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.16
        dcfsnz  isr1_temp,F
        movwf    ir_buffer+.17

        clrf    TMR3L                   ; Preload timer
        movlw   .253
        movwf   TMR3H
        bsf     T3CON,TMR3ON            ; (Re)Start Timeout counter
        return

isr_timer3:             ; IR-Link Timeout
		bcf		T3CON,TMR3ON			; Stop Timer3
        movff   ir_counter,char_I_extra_time
    	banksel isr_backup              ; Select Bank0 for ISR data.
        movlw   .15
        cpfseq  ir_counter              ; Got exact 15bytes?
        bra     isr_timer3_1            ; No, test for 16bytes
        bra     isr_timer3_ir           ; Got 15 bytes, compute local checksum
isr_timer3_1:
        movlw   .16
        cpfseq  ir_counter              ; Got exact 16bytes?
        bra     isr_timer3_2            ; No, test for 17bytes
        tstfsz  ir_buffer+.15           ; Last byte=0x00
        bra     isr_timer3_exit         ; No, exit
        bra     isr_timer3_ir           ; Got 16 bytes, compute local checksum
isr_timer3_2:
        movlw   .17
        cpfseq  ir_counter              ; Got exact 17bytes?
        bra     isr_timer3_exit         ; No, exit
        bra     isr_timer3_s8           ; S8 data

isr_timer3_ir:  ; IR input
        movff   ir_buffer+.0,PRODL
        clrf    PRODH
        movf    ir_buffer+.1,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.2,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.3,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.4,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.5,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.6,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.7,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.8,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.9,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.10,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.11,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.12,W
        rcall   isr_timer3_checksum

            ; Compare checksum
        movf    ir_buffer+.13,W
        cpfseq  PRODL                   ; Checksum ok?
        bra     isr_timer3_exit         ; No, exit
        movf    ir_buffer+.14,W
        cpfseq  PRODH                   ; Checksum ok?
        bra     isr_timer3_exit         ; No, exit

           ; Checksum OK, copy results
        movff   ir_buffer+.1,hud_status_byte
        movff   ir_buffer+.2,o2_mv_sensor1+0
        movff   ir_buffer+.3,o2_mv_sensor1+1
        movff   ir_buffer+.4,o2_mv_sensor2+0
        movff   ir_buffer+.5,o2_mv_sensor2+1
        movff   ir_buffer+.6,o2_mv_sensor3+0
        movff   ir_buffer+.7,o2_mv_sensor3+1
        movff   ir_buffer+.8,o2_ppo2_sensor1
        movff   ir_buffer+.9,o2_ppo2_sensor2
        movff   ir_buffer+.10,o2_ppo2_sensor3
        movff   ir_buffer+.11,hud_battery_mv+0
        movff   ir_buffer+.12,hud_battery_mv+1

        movlw   ir_timeout_value        ; multiples of 62,5ms
        movwf   ir_timeout              ; Reload timeout
 
isr_timer3_exit:
        clrf    ir_counter              ; Clear pointer
	    bcf		PIR2,TMR3IF				; Clear flag
        return

isr_timer3_checksum:
        addwf   PRODL,F
        movlw   .0
        addwfc  PRODH,F
        return

isr_timer3_s8:  ; IR input
        movff   ir_buffer+.0,PRODL
        clrf    PRODH
        movf    ir_buffer+.1,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.2,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.3,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.4,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.5,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.6,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.7,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.8,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.9,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.10,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.11,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.12,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.13,W
        rcall   isr_timer3_checksum
        movf    ir_buffer+.14,W
        rcall   isr_timer3_checksum

            ; Compare checksum
        movf    ir_buffer+.15,W
        cpfseq  PRODL                   ; Checksum ok?
        bra     isr_timer3_exit         ; No, exit
        movf    ir_buffer+.16,W
        cpfseq  PRODH                   ; Checksum ok?
        bra     isr_timer3_exit         ; No, exit

           ; Checksum OK, copy results
        movff   ir_buffer+.3,hud_status_byte
        movff   ir_buffer+.13,hud_battery_mv+0
        movff   ir_buffer+.14,hud_battery_mv+1

        banksel common
        bsf     new_s8_data_available       ; set flag
        banksel ir_timeout

        movlw   ir_timeout_value        ; multiples of 62,5ms
        movwf   ir_timeout              ; Reload timeout
        bra     isr_timer3_exit         ; Exit


;=============================================================================

isr_tmr7:       						; each 62,5ms
		bcf		PIR5,TMR7IF				; clear flag
		banksel 0xF16					; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
		movlw	.248
    	movwf	TMR7H					; -> Rollover after 2048 cycles -> 62,5ms

        banksel	isr_backup
	    movf	max_CCPR1L,W			; Dimm value
		cpfseq	CCPR1L					; = current PWM value?
		rcall	isr_dimm_tft			; No, adjust until max_CCPR1L=CCPR1L !

        banksel isr_backup
        decfsz  ir_timeout,F            ; IR Data still valid?
        bra     isr_tmr7_2              ; Yes, continue
        ; timeout, clear IR-Data

        movlw   ir_timeout_value        ; multiples of 62,5ms
        movwf   ir_timeout              ; Reload timeout

        banksel common
        btfss   c3_hardware
        bra     isr_tmr7_1a             ; Always with normal ostc3 hardware
        btfss   s8_digital
        bra     isr_tmr7_2              ; only when digital
isr_tmr7_1a:
        clrf    o2_mv_sensor1+0
        clrf    o2_mv_sensor1+1
        clrf    o2_mv_sensor2+0
        clrf    o2_mv_sensor2+1
        clrf    o2_mv_sensor3+0
        clrf    o2_mv_sensor3+1
        clrf    hud_battery_mv+0
        clrf    hud_battery_mv+1
        clrf    hud_status_byte
        clrf    o2_ppo2_sensor1
        clrf    o2_ppo2_sensor2
        clrf    o2_ppo2_sensor3

isr_tmr7_2:
        banksel common
		btfss	no_sensor_int			; No sensor interrupt (because it's addressed during sleep)
		bra		isr_sensor_state2		; No, continue
		banksel isr_backup              ; Back to Bank0 ISR data
		return						

isr_set_speed_to_normal:
	; Set Speed to normal
		movlw	b'01110010'
		movwf	OSCCON				; 16MHz INTOSC
		movlw	b'00000000'			
		movwf	OSCTUNE				; 4x PLL Disable (Bit6) - only works with 8 or 16MHz (=32 or 64MHz)
		movlw	b'00001101'			; 1:2 Postscaler, 1:4 Prescaler, Timer 2 start -> 1960Hz (no-flicker)
		movwf	T2CON
		btfss	OSCCON,HFIOFS
		bra		$-2					; Wait until clock is stable
		return

isr_sensor_state2:
		banksel	common
        movff   sensor_state_counter,WREG
        btfss   WREG,0                  ; every 1/4 second
        bsf     quarter_second_update   ; Set flag
		movlw	d'2'
		cpfseq	speed_setting           ; Set to normal in case it's not already in normal speed mode
		rcall	isr_set_speed_to_normal
		banksel isr_backup              ; Back to Bank0 ISR data

		incf	sensor_state_counter,F	; counts to eight for state maschine

; State 1: Clear flags and average registers, get temperature (51us) and start pressure integration (73,5us)
; State 2: Get pressure (51us), start temperature integration (73,5us) and calculate temperature compensated pressure (233us)
; State 3: Get temperature (51us) and start pressure integration (73,5us)
; State 4: Get pressure (51us), start temperature integration (73,5us) and calculate temperature compensated pressure (233us)
; State 5: Get temperature (51us) and start pressure integration (73,5us)
; State 6: Get pressure (51us), start temperature integration (73,5us) and calculate temperature compensated pressure (233us)
; State 7: Get temperature (51us) and start pressure integration (73,5us)
; State 8: Get pressure (51us), start temperature integration (73,5us), calculate temperature compensated pressure (233us) and build average for half-second update of tempperature and pressure
	
		movff	sensor_state_counter,WREG		; WREG used as temp here...
		dcfsnz	WREG,F
		bra		sensor_int_state1_plus_restart	; Do State 1
		dcfsnz	WREG,F
		bra		sensor_int_state2				; Do State 2
		dcfsnz	WREG,F
		bra		sensor_int_state1				; Do State 3
		dcfsnz	WREG,F
		bra		sensor_int_state2				; Do State 4
		dcfsnz	WREG,F
		bra		sensor_int_state1				; Do State 5
		dcfsnz	WREG,F
		bra		sensor_int_state2				; Do State 6
		dcfsnz	WREG,F
		bra		sensor_int_state1				; Do State 7
;		bra		sensor_int2_plus_average		; Do State 8
;sensor_int2_plus_average:
		; First, do state2:
		call		get_pressure_value		; State2: Get pressure (51us)
		call		get_temperature_start	; and start temperature integration (73,5us)
		call		calculate_compensation	; calculate temperature compensated pressure (27us)
; Build average
		bcf			STATUS,C            ; clear carry bit.
		rrcf		amb_pressure_avg+1  ; amb_pressure sum / 2
		rrcf		amb_pressure_avg+0
		bcf			STATUS,C            ; clear carry bit, twice.
		rrcf		amb_pressure_avg+1  ; amb_pressure sum / 4
		rrcf		amb_pressure_avg+0

;        ; Even more averaging
;        movf        amb_pressure_avg+0,W
;        addwf       amb_pressure+0
;        movf        amb_pressure_avg+1,W
;        addwfc      amb_pressure+1
;		bcf			STATUS,C            ; clear carry bit
;		rrcf		amb_pressure+1      ; amb_pressure sum / 2
;		rrcf		amb_pressure+0

		movff		amb_pressure_avg+1,amb_pressure+1	; copy into actual register
		movff		amb_pressure_avg+0,amb_pressure+0

        bcf			STATUS,C
        btfsc       temperature_avg+1,7 ; Copy sign bit to carry
        bsf         STATUS,C
		rrcf		temperature_avg+1   ; Signed temperature /2
		rrcf		temperature_avg+0
        bcf			STATUS,C
        btfsc       temperature_avg+1,7 ; Copy sign bit to carry
        bsf         STATUS,C
		rrcf		temperature_avg+1   ; Signed temperature /4
		rrcf		temperature_avg+0

		movff		temperature_avg+1,temperature+1     ; copy into actual register
		movff		temperature_avg+0,temperature+0

		banksel 	common                  ; flag1 is in Bank1
        bcf         temp_changed			; Clear flag for temperature update
        bcf         pressure_refresh        ; Clear flag for pressure update
        banksel 	isr_backup              ; Back to Bank0 ISR data

        ; Temp changed?
        movf	temperature+0,W
        cpfseq	last_temperature+0
        bra     isr_sensor_state2_2         ; Yes
        movf	temperature+1,W
        cpfseq	last_temperature+1
        bra     isr_sensor_state2_2         ; Yes

        bra     isr_sensor_state2_3         ; no change

isr_sensor_state2_2:
        banksel 	common                  ; flag1 is in Bank1
        bsf		temp_changed			    ; Yes
        banksel 	isr_backup              ; Back to Bank0 ISR data
isr_sensor_state2_3:
        movff   temperature+0,last_temperature+0    ; Copy for compare
        movff   temperature+1,last_temperature+1

    	movf	amb_pressure+0,W
    	cpfseq	last_pressure+0
        bra     isr_sensor_state2_4         ; Yes
    	movf	amb_pressure+1,W
    	cpfseq	last_pressure+1
        bra     isr_sensor_state2_4         ; Yes

        bra     isr_sensor_state2_5         ; No change
isr_sensor_state2_4:
        banksel 	common                  ; flag1 is in Bank1
    	bsf		pressure_refresh			; Yes
        banksel 	isr_backup              ; Back to Bank0 ISR data
isr_sensor_state2_5:
        movff   amb_pressure+0,last_pressure+0    ; Copy for compare
        movff   amb_pressure+1,last_pressure+1

		clrf		sensor_state_counter	; Then reset State counter
		btfss		simulatormode_active	; are we in simulator mode?
		bra			comp_air_pressure		; no
comp_air_pressure0:	
		movlw		LOW		d'1000'			; yes, so simulate 1000mbar surface pressure
		movwf		last_surfpressure+0
		movlw		HIGH	d'1000'
		movwf		last_surfpressure+1
        ; Always set pressure_refresh flag in simulator mode
        banksel 	common                  ; flag1 is in Bank1
    	bsf         pressure_refresh        ; Yes
        banksel 	isr_backup              ; Back to Bank0 ISR data

comp_air_pressure:
		movf		last_surfpressure+0,W		; compensate airpressure
		subwf   	amb_pressure+0,W             
		movwf   	rel_pressure+0				; rel_pressure stores depth!

		movf		last_surfpressure+1,W
		subwfb  	amb_pressure+1,W
		movwf   	rel_pressure+1
		btfss		STATUS,N					; result is below zero?
		bra			sensor_int_state_exit
		clrf		rel_pressure+0				; Yes, do not display negative depths
		clrf		rel_pressure+1				; e.g. when surface air pressure dropped during the dive
		bra			sensor_int_state_exit

sensor_int_state1_plus_restart:
		clrf		amb_pressure_avg+0  ; pressure average registers
		clrf		amb_pressure_avg+1
		clrf		temperature_avg+0
		clrf		temperature_avg+1

sensor_int_state1:
		call		get_temperature_value	; State 1: Get temperature
		call		get_pressure_start	 	; and start pressure integration.
		bra			sensor_int_state_exit

sensor_int_state2:
		call		get_pressure_value		; State2: Get pressure (51us)
		call		get_temperature_start	; and start temperature integration (73,5us)
		call		calculate_compensation	; calculate temperature compensated pressure (233us)
;		bra			sensor_int_state_exit
sensor_int_state_exit:
		rcall		isr_restore_clock		; Restore clock
		return
;=============================================================================

isr_dimm_tft:				; Adjust until max_CCPR1L=CCPR1L !
        banksel     common
		btfsc		tft_is_dimming			; Ignore while dimming
		return
        banksel     isr_backup
		movf		max_CCPR1L,W
		cpfsgt		CCPR1L					; CCPR1L>max_CCPR1L?
		bra			isr_dimm_tft2			; No, dimm up
	; dimm down
		decf		CCPR1L,F				; -1
		return
isr_dimm_tft2:
		movf		max_CCPR1L,W
		sublw		ambient_light_min_eco
		cpfsgt		CCPR1L					; CCPR1L>max_CCPR1L-ambient_light_min_eco?
		bra			isr_dimm_tft3			; No, dimm up slow
		; dimm up faster
		movlw		.5
		addwf		CCPR1L,F
isr_dimm_tft3:
		incf		CCPR1L,F				; +1
		return


isr_rtcc:								; each second
		bcf		PIR3,RTCCIF				; clear flag
		banksel 0xF16					; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
		bsf		RTCCFG,RTCPTR1
		bsf		RTCCFG,RTCPTR0			; year
		movff	RTCVALL,year			; format is BCD!
		movff	RTCVALH,day				; dummy read
		movff	RTCVALL,day				; format is BCD!
		movff	RTCVALH,month			; format is BCD!
		movff	RTCVALL,hours			; format is BCD!
		movff	RTCVALH,secs			; format is BCD!
		movff	RTCVALL,secs			; format is BCD!
		movff	RTCVALH,mins			; format is BCD!
        banksel isr_backup              ; Back to Bank0 ISR data
        
		; Convert BCD to DEC and set registers
		movff	mins, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,mins
		movff	secs, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,secs
		movff	hours, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,hours
		movff	month, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,month
		movff	day, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,day
		movff	year, isr1_temp
		rcall	isr_rtcc_convert		; Converts to dec with result in WREG
		movff	WREG,year

	; Place once/second tasks for ISR here (Be sure of the right bank!)
		banksel common                  ; flag1 is in Bank1
		btfss	sleepmode				; in Sleepmode?
		call	get_ambient_level		; No, get ambient light level and set max_CCPR1L

		rcall	isr_battery_gauge		; Add amount of battery consumption to battery_gauge:6

		banksel common                  ; flag1 is in Bank1
		bsf		onesecupdate			; A new second has begun
		btfsc	divemode				; in divemode?
		rcall	isr_divemode_1sec		; Yes, do some divemode stuff in bank common

        tstfsz  secs                    ; Secs == 0 ?
        return                          ; No, Done.

;        banksel isr_backup              ; Back to Bank0 ISR data
;		movff	secs,isr1_temp			; Copy to Bank0
;		movlw	d'0'
;		cpfseq	isr1_temp               ; Secs == 0 ?
;        return                          ; Done.

;		banksel common                  ; flag1 is in Bank1
		bsf		oneminupdate			; A new minute has begun

		btfss	divemode				; In Divemode?
		rcall	check_nofly_desat_time	; No, so reduce NoFly and Desat and increase interval

        ; Check if a new hour has just begun
;		banksel common                  ; flag1 is in Bank1
		tstfsz	mins					; mins=0?
		bra		isr_rtcc2				; No
		bsf		onehourupdate			; Yes, set flag
		
isr_rtcc2:
		banksel isr_backup              ; Back to Bank0 ISR data
		return							; Done.

isr_battery_gauge:	
	    banksel isr_backup              ; Bank0 ISR data
		movlw	current_sleepmode		; 100µA/3600 -> nAs	(Sleepmode current)
		movwf	isr1_temp				; Store value (low byte)
		clrf	isr2_temp				; High byte
		
		banksel common                  ; flag1 is in Bank1
		btfss	sleepmode				; in Sleepmode?
		rcall	isr_battery_gauge2		; No, compute current consumtion value into isr1_temp and isr2_temp

	    banksel isr_backup              ; Bank0 ISR data
		movf	isr1_temp,W				; 48Bit add of isr1_temp and isr2_temp into battery_gauge:6
		addwf	battery_gauge+0,F
		movf	isr2_temp,W
		addwfc	battery_gauge+1,F
		movlw	.0
		addwfc	battery_gauge+2,F
		movlw	.0
		addwfc	battery_gauge+3,F
		movlw	.0
		addwfc	battery_gauge+4,F
		movlw	.0
		addwfc	battery_gauge+5,F
		return
		
isr_battery_gauge2:
	; set consumtion rate in nAs for an one second interval
	; Example:
	; movlw	LOW		.55556			; 0,2A/3600*1e9s = nAs
	; movwf	isr1_temp				; Low byte
	; movlw	HIGH	.55556			; 0,2A/3600*1e9s = nAs
	; movwf	isr2_temp				; High byte

	; Current consumption for LED backlight is 47*CCPR1L+272
		movf	CCPR1L,W
		mullw	current_backlight_multi
		movlw	LOW		current_backlight_offset
		addwf	PRODL,F
		movlw	HIGH	current_backlight_offset
		addwfc	PRODH,F
		movff	PRODL,isr1_temp
		movff	PRODH,isr2_temp			; isr1_temp and isr2_temp hold value for backlight

	; Add current for CPU and GPU 
	; speed_setting=1: ECO (3,1mA -> 861nAs), =2: NORMAL (5,50mA -> 1528nAs) or =3: FASTEST (8,04mA -> 2233nAs)
		movlw	.1
		cpfseq	speed_setting
		bra		isr_battery_gauge3
		banksel isr_backup              ; Bank0 ISR data
		movlw	LOW		current_speed_eco
		addwf	isr1_temp,F
		movlw	HIGH	current_speed_eco
		addwfc	isr2_temp,F
		bra		isr_battery_gauge5
isr_battery_gauge3:
		movlw	.2
		cpfseq	speed_setting
		bra		isr_battery_gauge4
		banksel isr_backup              ; Bank0 ISR data
		movlw	LOW		current_speed_normal
		addwf	isr1_temp,F
		movlw	HIGH	current_speed_normal
		addwfc	isr2_temp,F
		bra		isr_battery_gauge5
isr_battery_gauge4:
;		movlw	.3
;		cpfseq	speed_setting
;		bra		isr_battery_gauge5
		banksel isr_backup              ; Bank0 ISR data
		movlw	LOW		current_speed_fastest
		addwf	isr1_temp,F
		movlw	HIGH	current_speed_fastest
		addwfc	isr2_temp,F
isr_battery_gauge5:
    ; Add current if IR reciever is on
        btfss   ir_power                    ; IR enabled?
        bra     isr_battery_gauge6          ; no
		movlw	LOW		current_ir_reciever
		addwf	isr1_temp,F
		movlw	HIGH	current_ir_reciever
		addwfc	isr2_temp,F
isr_battery_gauge6:
    ; Add current for compass/accelerometer
        btfss   compass_enabled             ; compass active?
        bra     isr_battery_gauge7          ; no
		movlw	LOW		current_compass
		addwf	isr1_temp,F
		movlw	HIGH	current_compass
		addwfc	isr2_temp,F
isr_battery_gauge7:
		return

isr_divemode_1sec:
		incf		samplesecs,F			; "samplingrate" diving seconds done 
		decf		samplesecs_value,W		; holds "samplingrate" value  (minus 1 into WREG)
		cpfsgt		samplesecs				; Done?
		bra			isr_divemode_1sec2		; no

		clrf		samplesecs				; clear counter...
		bsf			store_sample			; ...and set bit for profile storage
isr_divemode_1sec2:
; Increase re-setable average depth divetime counter
		incf		average_divesecs+0,F	; increase stopwatch registers
		btfsc		STATUS,Z
		incf		average_divesecs+1,F	; increase stopwatch registers
; Increase total divetime (Regardless of start_dive_threshold)
		incf		total_divetime_seconds+0,F
		movlw		.0
		addwfc		total_divetime_seconds+1,F	; Total dive time (Regardless of start_dive_threshold)
	
		btfss		divemode2				; displayed divetime is running?
		return								; No (e.g. too shallow)

; increase divetime registers (Displayed dive time)
		incf		divesecs,F				
		movlw		d'59'
		cpfsgt		divesecs
		bra			isr_divemode_1sec2a

		clrf		divesecs
		bsf			realdive				; this bit is always set (again) if the dive is longer then one minute
		incf		divemins+0,F
		movlw		.0
		addwfc		divemins+1,F			; increase divemins
		
isr_divemode_1sec2a:	
		btfss		FLAG_apnoe_mode			; Are we in Apnoe mode?
		return								; No
		
		incf		apnoe_secs,F			; increase descent registers
		movlw		d'59'
		cpfsgt		apnoe_secs				; full minute?
		return								; No
		clrf		apnoe_secs
		incf		apnoe_mins,F			; increase descent mins
		return

;=============================================================================
; BCD to Binary convertion.
; Input: isr1_temp = Value in BCD
; Output WREG = value in binary.
isr_rtcc_convert:
        swapf   isr1_temp, W
        andlw   0x0F           		  	; W= tens
        rlncf   WREG, W         		; W= 2*tens
        subwf   isr1_temp, F          	; 16*tens + ones - 2*tens
        subwf   isr1_temp, F          	; 14*tens + ones - 2*tens
        subwf   isr1_temp, W          	; 12*tens + ones - 2*tens
		return

;=============================================================================

isr_switch_right:						; 
        bcf     INTCON,INT0IE           ; Disable INT0
;        bcf     power_sw2               ; Power-down switch circuity
		banksel common                  ; flag1 is in Bank1
		bsf		switch_right			; Set flag, button press is OK
        bra     isr_switch_common       ; Continue...

isr_switch_left:						; 
        bcf     INTCON3,INT1IE          ; Disable INT1
 ;       bcf     power_sw1               ; Power-down switch circuity
		banksel common                  ; flag1 is in Bank1
  		bsf		switch_left				; Set flag, button press is OK

isr_switch_common:
        ; load timer1 for first press
        clrf    TMR1L
        movlw   TMR1H_VALUE_FIRST       ; in steps of 7,8125ms
        movwf   TMR1H
        bsf     T1CON,TMR1ON            ; Start Timer 1
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop

		banksel isr_backup              ; Select Bank0 for ISR data.
;        bsf     power_sw1
		bcf		INTCON3,INT1IF			; Clear flag
;        bsf     power_sw2               ; Power-up switch circuity
		bcf		INTCON,INT0IF			; Clear flag
		return

timer1int:
   		bcf		PIR1,TMR1IF             ; Clear flag
        banksel common                  ; flag1 is in Bank1
        btfss   switch_left1            ; Left button hold-down?
        bra     timer1int_left          ; Yes
        btfss   switch_right2           ; Right button hold-down?
        bra     timer1int_right         ; Yes

        ; No button hold-down, stop Timer 1
        bcf     T1CON,TMR1ON            ; Stop Timer 1
        bsf     INTCON,INT0IE           ; Enable INT0
        bsf     INTCON3,INT1IE          ; Enable INT1
		bcf		INTCON,INT0IF			; Clear flag
		bcf		INTCON3,INT1IF			; Clear flag
;		bcf		switch_left
;        bcf		switch_right
		return

timer1int_left:
;        bcf     power_sw1               ; Power-down switch circuity
		bsf		switch_left				; (Re-)Set flag
        bra     timer1int_common        ; Continue
timer1int_right:
 ;       bcf     power_sw2               ; Power-down switch circuity
		bsf		switch_right			; (Re-)Set flag
timer1int_common:
        ; load timer1 for next press
        clrf    TMR1L
        movlw   TMR1H_VALUE_CONT        ; Surface mode
        btfsc   divemode
        movlw   TMR1H_VALUE_CONT_DIVE   ; Dive mode
        movwf   TMR1H
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        nop
;        bsf     power_sw1
		bcf		INTCON3,INT1IF			; Clear flag
;        bsf     power_sw2               ; Power-up switch circuity
		bcf		INTCON,INT0IF			; Clear flag
        return                          ; Return from timer1int with timer1 kept running

;=============================================================================

check_nofly_desat_time:
    	movf    nofly_time+0,W              ; Is nofly null ?
    	iorwf   nofly_time+1,W
    	bz     	check_nofly_desat_time2     ; yes...

		movlw	d'1'
		subwf	nofly_time+0,F
		movlw	d'0'
		subwfb	nofly_time+1,F               ; reduce by one

check_nofly_desat_time2:
    	movf    desaturation_time+0,W        ; Is Desat null ?
    	iorwf   desaturation_time+1,W
    	bz     	check_nofly_desat_time3      ; yes...

		movlw	d'1'
		subwf	desaturation_time+0,F
		movlw	d'0'
		subwfb	desaturation_time+1,F	   	; reduce by one...

	; Increase surface interval timer 
		movlw	d'1'
		addwf	surface_interval+0,F
		movlw	d'0'
		addwfc	surface_interval+1,F
		return								; Done

check_nofly_desat_time3:
		clrf	surface_interval+0
		clrf	surface_interval+1			; Clear surface interval timer
		return								; Done.

;=============================================================================

isr_restore_clock:
		banksel	isr_backup
		movff	speed_setting,isr1_temp			; Copy to Bank0
		movlw	d'1'
		cpfseq	isr1_temp
		bra		isr_restore_speed2
	; Reset to eco	
		movlw	b'00000000'			
		movwf	OSCTUNE				; 4x PLL Disable (Bit6) - only works with 8 or 16MHz (=32 or 64MHz)
		movlw	b'00110010'
		movwf	OSCCON				; 1MHz INTOSC
		movlw	T2CON_ECO
		movwf	T2CON
		bra		isr_restore_exit
isr_restore_speed2:
		movlw	d'2'
		cpfseq	isr1_temp
		bra		isr_restore_speed3
	; Reset to normal
		movlw	b'01110010'
		movwf	OSCCON				; 16MHz INTOSC
		movlw	b'00000000'
		movwf	OSCTUNE				; 4x PLL Disable (Bit6) - only works with 8 or 16MHz (=32 or 64MHz)
		movlw	T2CON_NORMAL
		movwf	T2CON
		bra		isr_restore_exit

isr_restore_speed3:
	; Reset to fastest
		movlw	b'01110010'			; 16MHz INTOSC
		movwf	OSCCON				
		movlw	b'01000000'			
		movwf	OSCTUNE				; 4x PLL Enable (Bit6) - only works with 8 or 16MHz (=32 or 64MHz)
		movlw	T2CON_FASTEST
		movwf	T2CON
;		bra		isr_restore_exit
isr_restore_exit:
		btfss	OSCCON,HFIOFS
		bra		isr_restore_exit	; loop until PLL is stable
		return

		END