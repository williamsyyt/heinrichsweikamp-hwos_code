;=============================================================================
;
;   File ghostwriter.asm
;
;   Ghostwriter (Log profile recorder)
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-11-27 : [mH] Creation

#include    "hwos.inc"                  ; Mandatory header
#include 	"shared_definitions.h"         ; Mailbox from/to p2_deco.c
#include	"external_flash.inc"
#include	"surfmode.inc"
#include    "eeprom_rs232.inc"
#include 	"strings.inc"
#include	"isr.inc"
#include	"tft_outputs.inc"
#include	"divemode.inc"
#include    "rtc.inc"

ghostwriter	CODE

	global	store_dive_data
store_dive_data:						; 5 seconds gone
	bcf		store_sample				; update only any 5 seconds

    ifndef __DEBUG
    	btfsc	simulatormode_active    ; Are we in simulator mode?
    	return                          ; Yes, discard everything
    endif

    btfsc	FLAG_apnoe_mode             ; In Apnoe mode?
    return                              ; Yes, discard everything

    SAFE_2BYTE_COPY rel_pressure, lo
	movf	lo,W				        ; store depth with every sample
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash
	movf	hi,W
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash

;First, find out how many bytes will append to this sample....
	clrf	ProfileFlagByte					; clear number of bytes to append

; Check Extented informations
	decfsz	divisor_temperature,W	; Check divisor
	bra		check_extended1		
	movlw	infolength_temperature
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended1:
	decfsz	divisor_deco,W		; Check divisor
	bra		check_extended2		
	movlw	infolength_deco
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended2:
	decfsz	divisor_gf,W		; Check divisor
	bra		check_extended3		
	movlw	infolength_gf
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended3:
	decfsz	divisor_ppo2_sensors,W		; Check divisor
	bra		check_extended4		
	movlw	infolength_ppo2_sensors
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended4:
	decfsz	divisor_decoplan,W     ; Check divisor
	bra		check_extended5		
	movlw	infolength_decoplan
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended5:
	decfsz	divisor_cns,W		; Check divisor
	bra		check_extended6		
	movlw	infolength_cns
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended6:
	decfsz	divisor_tank,W		; Check divisor
	bra		check_extended7
	movlw	infolength_tank
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
check_extended7:

; Second, check global event flag
	btfss	event_occured		; Check global event flag
	bra		store_dive_data3	; No Event

	incf    ProfileFlagByte,F	; add one byte (The EventByte)

	clrf	EventByte			; reset EventByte
    clrf	EventByte2			; reset EventByte2

	movf	AlarmType,W			; Type of Alarm Bit 0-3
	addwf	EventByte,F			; Copy to EventByte Bit 0-3
	clrf	AlarmType			; Reset AlarmType
	
; Third, check events and add aditional bytes
	btfss	gas6_changed    	; Check flag
	bra		check_event2
	movlw	d'2'				; Information length
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
	bsf		EventByte,4			; Also set Flag in EventByte!
check_event2:
	btfss	stored_gas_changed	; Check flag
	bra		check_event3
	movlw	d'1'				; Information length
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
	bsf		EventByte,5			; Also set Flag in EventByte!
check_event3:
    btfss	setpoint_changed	; Check flag
	bra		check_event4
	movlw	d'1'				; Information length
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
	bsf		EventByte,6			; Also set Flag in EventByte!
check_event4:
    btfss   bailoutgas_event    ; =1: bailout was selected or a gaschange during bailout
    bra     check_event5
	movlw	d'2'				; Information length
	addwf	ProfileFlagByte,F	; add to ProfileFlagByte
	bsf		EventByte2,0		; set flag in EventByte2!
    bsf     EventByte,7			; =1: Another Eventbyte is available

check_event5:
	; more events?

store_dive_data3:
    btfsc   EventByte,7                 ; =1: Another Eventbyte is available
    incf    ProfileFlagByte,F           ; add one byte (The EventByte2)

    btfsc	event_occured               ; Check global event flag
	bsf		ProfileFlagByte,7           ; Set EventByte Flag in ProfileFlagByte

	movf	ProfileFlagByte,W           ; finally, write ProfileFlagByte!
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash

	btfss	event_occured               ; Check global event flag (again)
	bra		store_dive_data4            ; No Event

; Store the EventByte(s) + additional bytes now
	movf	EventByte,W		
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash

    movf    EventByte2,W                ; Write second event byte...
    btfsc   EventByte,7                 ; =1: Another Eventbyte is available
    rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash

	btfss	gas6_changed                ; Check flag
	bra		store_dive_data3b
    movff   char_I_O2_ratio,WREG
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash
    movff   char_I_He_ratio,WREG
	rcall   ghostwrite_byte_profile     ; WREG -> Profile in ext. Flash
	bcf		gas6_changed    	; Clear this event
store_dive_data3b:
	btfss	stored_gas_changed	; Check flag
	bra		store_dive_data3c
	movf	active_gas,W		; Store active gas
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	bcf		stored_gas_changed	; Clear this event
store_dive_data3c:
	btfss	setpoint_changed	; Check flag
	bra		store_dive_data3d
	movff	char_I_const_ppO2,WREG
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	bcf		setpoint_changed	; Clear this event
store_dive_data3d:
	btfss	bailoutgas_event	; Check flag
	bra		store_dive_data3e
    movff   char_I_O2_ratio,WREG
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    movff   char_I_He_ratio,WREG
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	bcf		bailoutgas_event	; Clear this event
store_dive_data3e:

store_dive_data4:

; Store extended informations
	decfsz	divisor_temperature,F	; Check divisor
	bra		store_extended1	
	rcall	store_dive_temperature
store_extended1:
	decfsz	divisor_deco,F			; Check divisor
	bra		store_extended2	
	rcall	store_dive_decodata
store_extended2:
	decfsz	divisor_gf,F			; Check divisor
	bra		store_extended3	
	rcall	store_dive_gf
store_extended3:
	decfsz	divisor_ppo2_sensors,F	; Check divisor
	bra		store_extended4	
	rcall	store_dive_ppO2_sensors
store_extended4:
	decfsz	divisor_decoplan,F		; Check divisor
	bra		store_extended5	
	rcall	store_dive_decoplan
store_extended5:
	decfsz	divisor_cns,F			; Check divisor
	bra		store_extended6	
	rcall	store_dive_cns
store_extended6:
	decfsz	divisor_tank,F			; Check divisor
	bra		store_extended7
	rcall	store_dive_tank
store_extended7:

; The next block is required to take care of "store never"
	btfsc	divisor_temperature,7	; Test highest Bit (Register must have been zero before the "decfsz" command!)
	clrf	divisor_temperature		; And clear register again, so it will never reach zero...
	btfsc	divisor_deco,7
	clrf	divisor_deco
	btfsc	divisor_gf,7
	clrf	divisor_gf
	btfsc	divisor_ppo2_sensors,7
	clrf	divisor_ppo2_sensors
	btfsc	divisor_decoplan,7
	clrf	divisor_decoplan
	btfsc	divisor_cns,7
	clrf	divisor_cns
	btfsc	divisor_tank,7
	clrf	divisor_tank

store_dive_data5:
	bcf		event_occured		; Clear the global event flag
	clrf	EventByte			; reset EventByte
    clrf	EventByte2			; reset EventByte2
	return						; Done. (Sample with all informations written to external flash)
	
store_dive_cns:
	movff	int_O_CNS_fraction+0,WREG
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movff	int_O_CNS_fraction+1,WREG
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movlw	div_cns
	movwf	divisor_cns						; Reload divisor from CF
	return

store_dive_tank:
	movlw	div_tank
	movwf	divisor_tank						; Reload divisor from CF
	return

store_dive_decoplan:
    ; Store the decoplan
    lfsr    FSR1,char_O_deco_time_for_log+.0
    movlw   .15
    movwf   lo
store_dive_decoplan_loop:
    movf    POSTINC1,W
    rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    decfsz  lo,F
    bra     store_dive_decoplan_loop
	movlw	div_decoplan
	movwf	divisor_decoplan             ; Reload divisor from CF
	return

store_dive_ppO2_sensors:
    movf    o2_ppo2_sensor1,W            ; Sensor1 ppO2 (in 0.01bar steps)
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    SAFE_2BYTE_COPY o2_mv_sensor1,lo     ; o2_mv_sensor may be modifified via ISR during the two writes here...
    movf    lo,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    movf    hi,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash

    movf    o2_ppo2_sensor2,W            ; Sensor2 ppO2 (in 0.01bar steps)
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    SAFE_2BYTE_COPY o2_mv_sensor2,lo     ; o2_mv_sensor may be modifified via ISR during the two writes here...
    movf    lo,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    movf    hi,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash

    movf    o2_ppo2_sensor3,W            ; Sensor3 ppO2 (in 0.01bar steps)
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    SAFE_2BYTE_COPY o2_mv_sensor3,lo     ; o2_mv_sensor may be modifified via ISR during the two writes here...
    movf    lo,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
    movf    hi,W                         ; in 0.1mV steps
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash

	movlw	div_ppo2_sensors
	movwf	divisor_ppo2_sensors        ; Reload divisor
	return

store_dive_gf:
	movff	char_O_gradient_factor,WREG		; gradient factor absolute
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movlw	div_gf
	movwf	divisor_gf						; Reload divisor
	return

store_dive_decodata:
	movf	decodata+0,W					; =0:no stop dive, if in deco mode: ceiling in m
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movf	decodata+1,W					; no stop time of length of first stop
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movlw	div_deco
	movwf	divisor_deco					; Reload divisor
	return

store_dive_temperature:
    SAFE_2BYTE_COPY temperature,lo
	movf	lo,W			            	; append temperature to current sample!
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movf	hi,W
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movlw	div_temperature
	movwf	divisor_temperature				; Reload divisor
	return

ghostwrite_byte_header:
	goto	write_byte_ext_flash_plus_header	; (This call will also delete the 4kB TOC entry first)
    ; returns...

ghostwrite_byte_profile:
    goto    write_byte_ext_flash_plus	; writes byte and increases address with banking at 0x200000
    ; returns...

	global	ghostwriter_end_dive
ghostwriter_end_dive:
	movff	ext_flash_address+0,ext_flash_log_pointer+0
	movff	ext_flash_address+1,ext_flash_log_pointer+1
	movff	ext_flash_address+2,ext_flash_log_pointer+2	; Save end-of-profile pointer to store in header

    movff   menupos3,customview_divemode            ; store last customview

	btfss	realdive					; dive longer then one minute
	goto	ghostwriter_end_dive_common				; No, discard everything

; In DEBUG compile, keep all simulated dives in logbook, Desat time, nofly, etc...
    ifndef __DEBUG
    	btfsc	simulatormode_active		; Are we in simulator mode?
    	goto	ghostwriter_end_dive_common_sim		; Yes, discard everything
    endif

    btfsc	FLAG_apnoe_mode                         ; In Apnoe mode?
    goto	ghostwriter_end_dive_common				; Yes, discard everything

	; Dive finished (and longer then one minute)

	btfsc	FLAG_apnoe_mode					; Calc max. depth (again) for very short apnoe dives
	call	apnoe_calc_maxdepth

	; calculate desaturation time
	movff	last_surfpressure_30min+0,int_I_pres_surface+0          ; Pass surface to desat routine !
	movff	last_surfpressure_30min+1,int_I_pres_surface+1

	call	deco_calc_desaturation_time ; calculate desaturation time
	movlb	b'00000001'                 ; select ram bank 1
	movff	int_O_desaturation_time+0, desaturation_time+0
	movff	int_O_desaturation_time+1, desaturation_time+1	; Buffer
    call	calc_deko_surfmode
	rcall	calculate_noflytime         ; Calc NoFly time

										; store header and ...
	movlw	0xFD						; .... End-of-Profile Bytes
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movlw	0xFD
	rcall   ghostwrite_byte_profile      ; WREG -> Profile in ext. Flash
	movff	ext_flash_address+0,ext_flash_log_pointer+0
	movff	ext_flash_address+1,ext_flash_log_pointer+1
	movff	ext_flash_address+2,ext_flash_log_pointer+2	; Save end-of-profile pointer to store in header

    ; Set to first address again to store dive length ext_flash_dive_counter:3
    rcall   ghostwriter_load_pointer        ; Load ext_flash_address:3 from EEPROM .4-.6

    incf_ext_flash_address_0x20	d'6'        ; Skip internal "0xFA 0xFA #Divenumber:2 0xFA 0xFA" Header
    ; Store dive length
    movf   ext_flash_dive_counter+0,W
	call    write_byte_ext_flash_plus_nodel ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase) and does NOT delete 4kB page
    movf   ext_flash_dive_counter+1,W
	call    write_byte_ext_flash_plus_nodel ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase) and does NOT delete 4kB page
    movf   ext_flash_dive_counter+2,W
	call    write_byte_ext_flash_plus_nodel ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase) and does NOT delete 4kB page

; profile recording done.

	; Load total number of dives
	read_int_eeprom .2
	movff	EEDATA,lo
	read_int_eeprom .3
	movff	EEDATA,hi
	; +1                        increase total dive counter
    infsnz  lo,F
	incf	hi,F
	; Store new number in EEPROM
	movff	lo,EEDATA
	write_int_eeprom	.2
	movff	hi,EEDATA
	write_int_eeprom	.3

	decf	lo,F		; -1
	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> lo=0
	; 2nd: 201000h-201FFFh -> lo=1
	; 3rd: 202000h-202FFFh -> lo=2
	; 255: 2FF000h-2FFFFFh -> lo=255

	clrf	ext_flash_address+0
	clrf	ext_flash_address+1
	movlw	0x20
	movwf	ext_flash_address+2
	movlw	.16
	mulwf	lo							; lo*16 = offset to 0x2000 (up:hi)
	movf	PRODL,W
	addwf	ext_flash_address+1,F
	movf	PRODH,W
	addwfc	ext_flash_address+2,F
	
; Now, write header

	movlw	0xFA						; Header start
    rcall   ghostwrite_byte_header  ; (This call will also delete the 4kB TOC entry first)
	movlw	0xFA
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	; store pointer to begin of diveprofile
	read_int_eeprom	.4
	movf	EEDATA,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	read_int_eeprom	.5
	movf	EEDATA,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	read_int_eeprom	.6
	movf	EEDATA,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

		; store pointer to end of diveprofile
	movf	ext_flash_log_pointer+0,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movf	ext_flash_log_pointer+1,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movf	ext_flash_log_pointer+2,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	; write rest of header
	movlw	logbook_profile_version     ; Defined in hwos.inc
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

    ; Store dive length
    movf    ext_flash_dive_counter+0,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    movf    ext_flash_dive_counter+1,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    movf    ext_flash_dive_counter+2,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	movff	start_year,WREG         ; Date
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	start_month,WREG
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	start_day,WREG
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	start_hours,WREG    	; Start of dive time
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	start_mins,WREG
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	btfss	FLAG_apnoe_mode				; Store apnoe max or normal max (Which is only max from the last descent)
	bra		end_dive1					; Store normal depth

	movff	apnoe_max_pressure+0,lo
	movff	apnoe_max_pressure+1,hi
	call	adjust_depth_with_salinity	; computes salinity setting into lo:hi [mbar]
	movff	lo,apnoe_max_pressure+0
	movff	hi,apnoe_max_pressure+1
	
	movf	apnoe_max_pressure+0,W		; Max. depth
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movf	apnoe_max_pressure+1,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	bra		end_dive2					; skip normal max. depth
		
end_dive1:
	movff	max_pressure+0,lo
	movff	max_pressure+1,hi
	call	adjust_depth_with_salinity	; computes salinity setting into lo:hi [mbar]
	movff	lo,max_pressure+0
	movff	hi,max_pressure+1
	
	movff	max_pressure+0,WREG			; Max. depth
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	max_pressure+1,WREG
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

end_dive2:
	movf	divemins+0,W            ; divetime minutes
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movf	divemins+1,W
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movf	divesecs,W				; divetime seconds
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	minimum_temperature+0,WREG			; minimum temperature
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	minimum_temperature+1,WREG		
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	last_surfpressure_30min+0,WREG		; airpressure before dive
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	last_surfpressure_30min+1,WREG		
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	int_O_desaturation_time+0,WREG		; desaturation time in minutes
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	int_O_desaturation_time+1,WREG
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

    btfss   FLAG_ccr_mode           ; In CCR mode...
    bra     end_dive_oc_gaslist     ; No, write OC gases
    ; Write Diluents...
	movff	opt_dil_O2_ratio+0,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_dil_He_ratio+0,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	char_I_dil_change+0,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_dil_type+0,WREG                   
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2=Normal

	movff	opt_dil_O2_ratio+1,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_dil_He_ratio+1,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	char_I_dil_change+1,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_dil_type+1,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2=Normal

	movff	opt_dil_O2_ratio+2,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_dil_He_ratio+2,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	char_I_dil_change+2,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_dil_type+2,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2=Normal

	movff	opt_dil_O2_ratio+3,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_dil_He_ratio+3,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	char_I_dil_change+3,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_dil_type+3,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2=Normal

	movff	opt_dil_O2_ratio+4,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_dil_He_ratio+4,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	char_I_dil_change+4,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_dil_type+4,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2=Normal
    bra     end_dive_oc_cc_common

end_dive_oc_gaslist:            	; OC Gases...
	movff	opt_gas_O2_ratio+0,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_gas_He_ratio+0,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	opt_OC_bail_gas_change+0,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_gas_type+0,WREG                   
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2= Travel, 3= Deco

	movff	opt_gas_O2_ratio+1,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_gas_He_ratio+1,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	opt_OC_bail_gas_change+1,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_gas_type+1,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2= Travel, 3= Deco

	movff	opt_gas_O2_ratio+2,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_gas_He_ratio+2,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	opt_OC_bail_gas_change+2,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_gas_type+2,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2= Travel, 3= Deco

	movff	opt_gas_O2_ratio+3,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_gas_He_ratio+3,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	opt_OC_bail_gas_change+3,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_gas_type+3,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2= Travel, 3= Deco

	movff	opt_gas_O2_ratio+4,WREG
	rcall	ghostwrite_byte_header	; %O2
	movff	opt_gas_He_ratio+4,WREG
	rcall	ghostwrite_byte_header	; %He
	movff	opt_OC_bail_gas_change+4,WREG
	rcall	ghostwrite_byte_header	; Configured change depth in m
    movff   opt_gas_type+4,WREG
    rcall	ghostwrite_byte_header	; 0=Disabled, 1=First, 2= Travel, 3= Deco
;    bra     end_dive_oc_cc_common

end_dive_oc_cc_common:
	movlw	softwareversion_x			; Firmware version
	rcall	ghostwrite_byte_header
	movlw	softwareversion_y
	rcall	ghostwrite_byte_header
	movf	batt_voltage+0,W			; Battery voltage 
	rcall	ghostwrite_byte_header
	movf	batt_voltage+1,W
	rcall	ghostwrite_byte_header

	movf    samplingrate,W			; Sampling rate
    btfsc	FLAG_apnoe_mode         ; Apnoe mode?
	movlw	samplingrate_apnoe		; Apnoe sampling rate
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

    ; CNS at beginning of dive
	movff	CNS_start+0,WREG
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    movff	CNS_start+1,WREG
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    ; Gradient factor
    movff	GF_start,WREG
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    movff   char_O_relative_gradient_GF,WREG
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

    ; Logbook offset
    call    do_logoffset_common_read; Read into lo:hi
    movf    lo,W
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    movf    hi,W
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

    ; Spare at Byte 59
    movlw   0xFF
    rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
    ; Store 5 Setpoints
    movff   char_I_setpoint_cbar+0,WREG
    rcall	ghostwrite_byte_header	; Setpoint in cbar
    movff   char_I_setpoint_change+0,WREG
    rcall	ghostwrite_byte_header	; Change depth
    movff   char_I_setpoint_cbar+1,WREG
    rcall	ghostwrite_byte_header	; Setpoint in cbar
    movff   char_I_setpoint_change+1,WREG
    rcall	ghostwrite_byte_header	; Change depth
    movff   char_I_setpoint_cbar+2,WREG
    rcall	ghostwrite_byte_header	; Setpoint in cbar
    movff   char_I_setpoint_change+2,WREG
    rcall	ghostwrite_byte_header	; Change depth
    movff   char_I_setpoint_cbar+3,WREG
    rcall	ghostwrite_byte_header	; Setpoint in cbar
    movff   char_I_setpoint_change+3,WREG
    rcall	ghostwrite_byte_header	; Change depth
    movff   char_I_setpoint_cbar+4,WREG
    rcall	ghostwrite_byte_header	; Setpoint in cbar
    movff   char_I_setpoint_change+4,WREG
    rcall	ghostwrite_byte_header	; Change depth

    movff   opt_salinity,WREG       ; Salinity (0-4%)
	rcall	ghostwrite_byte_header	; Store Salinity to Dive

	movff	int_O_CNS_fraction+0,WREG		; copy into bank1
    rcall	ghostwrite_byte_header; Stores CNS%
    movff	int_O_CNS_fraction+1,WREG		; copy into bank1
	rcall	ghostwrite_byte_header; Stores CNS%

	movff	avg_rel_pressure_total+0,WREG	; Average Depth
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	avg_rel_pressure_total+1,WREG	; Average Depth
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	movff	total_divetime_seconds+0,WREG	; Total dive time (Regardless of start_dive_threshold)
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movff	total_divetime_seconds+1,WREG	; Total dive time (Regardless of start_dive_threshold)
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	movff	char_I_GF_Low_percentage,WREG	; GF_lo
	movff	char_I_deco_model,lo
	decfsz	lo,F							; jump over next line if char_I_deco_model == 1
	movff	char_I_saturation_multiplier,WREG ; Saturation Multiplier
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	movff	char_I_GF_High_percentage,WREG	; GF_hi
	movff	char_I_deco_model,lo
	decfsz	lo,F							; jump over next line if char_I_deco_model == 1
	movff	char_I_desaturation_multiplier,WREG ; Desaturation Multiplier
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash

	movff	char_I_deco_model,WREG			; 0 = ZH-L16, 1 = ZH-L16-GF
	rcall	ghostwrite_byte_header; writes byte and increases address (no banking)

	read_int_eeprom	.2
	movf	EEDATA,W
	rcall	ghostwrite_byte_header  ; Total dive counter, low
	read_int_eeprom	.3
	movf	EEDATA,W
	rcall	ghostwrite_byte_header  ; Total dive counter, high

    movff   opt_dive_mode,WREG
	rcall	ghostwrite_byte_header  ; 0=OC, 1=CC, 2=Gauge, 3=Apnea

;   Store all tissue data available
    movlw   .16
    movwf   lo
    lfsr    FSR1,char_O_tissue_N2_saturation+0
end_dive_store_tissues_N2:
    movf    POSTINC1,W
    rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    decfsz  lo,F
    bra     end_dive_store_tissues_N2   ; No

    movlw   .64
    movwf   lo
    lfsr    FSR1,0x700;pres_tissue_N2+0       ; 16*4Byte Float = 64Bytes
end_dive_store_tissues_N2_2:
    movf    POSTINC1,W
    rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    decfsz  lo,F
    bra     end_dive_store_tissues_N2_2 ; No

    movlw   .16
    movwf   lo
    lfsr    FSR1,char_O_tissue_He_saturation+0
end_dive_store_tissues_He:
    movf    POSTINC1,W
    rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    decfsz  lo,F
    bra     end_dive_store_tissues_He   ; No

    movlw   .64
    movwf   lo
    lfsr    FSR1,0x740;pres_tissue_He+0       ; 16*4Byte Float = 64Bytes
end_dive_store_tissues_He_2:
    movf    POSTINC1,W
    rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    decfsz  lo,F
    bra     end_dive_store_tissues_He_2 ; No

    ; Some deco stuff
    movff   char_I_depth_last_deco,WREG ; last stop [m]
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   char_I_deco_distance,WREG   ; assumed distance to shown stop
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash

    ; Last HUD data
    movff   hud_battery_mv+0,WREG       ; Last HUD battery value
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   hud_battery_mv+1,WREG       ; Last HUD battery value
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   hud_status_byte,WREG        ; Last HUD status
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash

    ; Battery gauge registers [nAs]
    movff   battery_gauge+0,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   battery_gauge+1,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   battery_gauge+2,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   battery_gauge+3,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   battery_gauge+4,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash
    movff   battery_gauge+5,WREG        ; Battery gauge register
	rcall	ghostwrite_byte_header      ; WREG -> Header in ext. flash

    ; Header stop
	movlw	0xFB
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	movlw	0xFB
	rcall	ghostwrite_byte_header	; WREG -> Header in ext. flash
	
	call	divemode_store_statistics		; Store/update statistics for this unit

	clrf	surface_interval+0
	clrf	surface_interval+1				; Clear surface interval timer

ghostwriter_end_dive_common:
; Update ext_flash_log_pointer into EEPROM
	clrf	EEADRH
	movff	ext_flash_log_pointer+0,EEDATA
	write_int_eeprom .4
	movff	ext_flash_log_pointer+1,EEDATA
	write_int_eeprom .5
	movff	ext_flash_log_pointer+2,EEDATA
	write_int_eeprom .6

	bcf		simulatormode_active			; if we were in simulator mode

; In DEBUG compile, keep all simulated dives in logbook, Desat time, nofly, etc...
    ifndef __DEBUG
		extern	deco_pull_tissues_from_vault
		btfsc	restore_deco_data			; Restore decodata?
	    call    deco_pull_tissues_from_vault
        banksel common                  ; Bank1
	endif
	call	update_battery_registers	; update battery registers into EEPROM
	goto	surfloop					; and return to surfaceloop

ghostwriter_end_dive_common_sim:
    tstfsz  surface_interval+0              ; Was interval zero?
    bra     ghostwriter_end_dive_common_sim2    ; No
    tstfsz  surface_interval+1              ; Was interval zero?
    bra     ghostwriter_end_dive_common_sim2    ; No
    bra     ghostwriter_end_dive_common     ; Yes, done.

ghostwriter_end_dive_common_sim2:
    movf    divemins+0,W
    addwf   surface_interval+0,F
    movf    divemins+1,W
	addwfc  surface_interval+1				; Add simulated divetime to surface interval
    bra     ghostwriter_end_dive_common

ghostwriter_load_pointer:               ; Load ext_flash_address:3 from EEPROM .4-.6
	clrf    EEADRH                      ; Make sure to select eeprom bank 0
	read_int_eeprom	.4
	movff	EEDATA,ext_flash_address+0
	read_int_eeprom	.5
	movff	EEDATA,ext_flash_address+1
	read_int_eeprom	.6
	movff	EEDATA,ext_flash_address+2
    return

ghostwriter_short_header_init:          ; Proceed one page forward
    clrf    EEDATA
    write_int_eeprom .4                 ; ext_flash_address+0 = 0
    movlw   .16
    addwf   ext_flash_address+1,F
    movlw   .0
    addwfc  ext_flash_address+2,F
    movlw	0x20
	cpfseq	ext_flash_address+2			; at address 0x200000?
	bra     ghostwriter_short_header_init2  ; No
	clrf	ext_flash_address+2			; Yes, rollover to 0x000000
ghostwriter_short_header_init2:
    movlw   0xF0
    andwf   ext_flash_address+1,F       ; keep higher nibble, set lower nibble to 0

    movff   ext_flash_address+1,EEDATA
    write_int_eeprom .5                 ; Write new pointer
    movff   ext_flash_address+2,EEDATA
    write_int_eeprom .6                 ; Write new pointer
    bra     ghostwriter_short_header2   ; Done.

	global	ghostwriter_short_header
ghostwriter_short_header:	; Write short header with divenumber into profile memory
	; load pointer for profile storing into RAM (Updated in EEPROM after the dive)
    rcall   ghostwriter_load_pointer        ; Load ext_flash_address:3 from EEPROM .4-.6

    ; The following code is used to write a clean new dive after the previous hasn't been
    ; stored correctly. e.g. after a battery fail during the dive
    call    ext_flash_byte_read_plus_0x20   ; Into temp1
    incfsz  temp1,F
    bra     ghostwriter_short_header_init   ; Not 0xFF -> init page
    call    ext_flash_byte_read_plus_0x20   ; Into temp1
    incfsz  temp1,F
    bra     ghostwriter_short_header_init   ; Not 0xFF -> init page

ghostwriter_short_header2:
    ; All ok, reload the pointer and start
    rcall   ghostwriter_load_pointer        ; Load ext_flash_address:3 from EEPROM .4-.6

    ; Clear dive length counter
    clrf    ext_flash_dive_counter+0
    clrf    ext_flash_dive_counter+1
    clrf    ext_flash_dive_counter+2

	; Write short header with divenumber into profile memory
	movlw	0xFA
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	0xFA
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	; Load total number of dives (low byte only)
	read_int_eeprom .2
	incf	EEDATA,W							;+1	
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	read_int_eeprom .3
	movf	EEDATA,W
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	0xFA
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	0xFA
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    ; Keep room for dive length ext_flash_dive_counter:3 (Stored at the end of the dive)
    ; Writing 0xFF three times here is mandatory
    ; - 0xFF can be overwritten after the dive
    ; - ghostwrite_byte_profile takes care of 4kB Page switching
    ; - fixes an issue when we are at exactly 0xXXX000 here...

    movlw   0xFF
    call    write_byte_ext_flash_plus_nocnt ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase)
    movlw   0xFF
    call    write_byte_ext_flash_plus_nocnt ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase)
    movlw   0xFF
    call    write_byte_ext_flash_plus_nocnt ; WREG -> Profile in ext. flash (No ext_flash_dive_counter:3 increase)

	movf	samplingrate,W  			; Sampling rate
	btfsc	FLAG_apnoe_mode				; Apnoe mode?
	movlw	samplingrate_apnoe			; Apnoe sampling rate
	rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .7                          ; Number of divisors
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .0                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_temperature
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_temperature             ; Divisor temperature
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .1                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_deco
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_deco                    ; Divisor decodata
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .2                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_gf
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_gf                      ; Divisor gf
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .3                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_ppo2_sensors
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_ppo2_sensors            ; Divisor ppO2
    btfss   FLAG_ccr_mode               ; =1: CCR mode (Fixed ppO2 or Sensor) active
    movlw   .0                          ; No ppO2 data in OC mode
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .4                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_decoplan
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_decoplan                ; Divisor debug
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .5                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_cns
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_cns                     ; Divisor CNS
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

    movlw   .6                          ; Type
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	infolength_tank
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash
	movlw	div_tank                    ; Divisor Tank
    rcall	ghostwrite_byte_profile 	; WREG -> Profile in ext. flash

	return

calculate_noflytime:
	; calculate nofly time
	movff	int_O_desaturation_time+0,xA+0
	movff	int_O_desaturation_time+1,xA+1

    btfsc   xA+1,7                  ; Is desat time negative ?
    bra     calculate_noflytime_3   ; Then surely not valid !

	tstfsz	xA+0			; Desat=0?
	bra		calculate_noflytime2
	tstfsz	xA+1			; Desat=0?
	bra		calculate_noflytime2

calculate_noflytime_3:	
	; Desaturation time = zero
	clrf	nofly_time+0			; Clear nofly time
	clrf	nofly_time+1			; Clear nofly time
	return

calculate_noflytime2:	
	movff	xA+0,int_I_temp+0
	movff	xA+1,int_I_temp+1
	movlw	no_fly_time_ratio		; nofly_time_ratio                
	movff	WREG,char_I_temp
	call	deco_calc_percentage
	movlb	b'00000001'				; select ram bank 1
	movff	int_I_temp+0,xA+0
	movff	int_I_temp+1,xA+1
	tstfsz	xA+0			; Desat=0?
	bra		calculate_noflytime_2_final
	tstfsz	xA+1			; Desat=0?
	bra		calculate_noflytime_2_final
	bra     calculate_noflytime_3

calculate_noflytime_2_final:
	movff	xA+0,nofly_time+0
	movff	xA+1,nofly_time+1
	return


divemode_store_statistics:	; Store/update statistics for this unit
    rcall   vault_decodata_into_eeprom  ; update deco data
	rcall	do_logoffset_common_read	; Existing logbook offset into lo:hi
	
	tstfsz	lo							; lo=0?
	bra		change_logbook_offset1		; No, adjust offset	
	tstfsz	hi							; hi=0?
	bra		change_logbook_offset1		; No, adjust offset
	bra		change_logbook_offset2		; lo=0 and hi=0 -> skip Offset routine
change_logbook_offset1:
	movlw	d'1'
	addwf	lo
	movlw	d'0'
	addwfc	hi
	rcall	do_logoffset_common_write	; lo:hi -> EEPROM
change_logbook_offset2:
	; Clear lastdive:4
	banksel	lastdive_time+0
	clrf	lastdive_time+0
	clrf	lastdive_time+1
	clrf	lastdive_time+2
	clrf	lastdive_time+3
	banksel	common

	; Add more here...
	return	

	global	do_logoffset_common_write
do_logoffset_common_write:
	movff	lo,EEDATA
	write_int_eeprom	0x0D
	movff	hi,EEDATA
	write_int_eeprom	0x0E
	return

	global	do_logoffset_common_read
do_logoffset_common_read:
	clrf	EEADRH
	read_int_eeprom		0x0D
	movff	EEDATA,lo
	read_int_eeprom		0x0E
	movff	EEDATA,hi					; Existing logbook offset into lo:hi
	return


	global		update_battery_registers
update_battery_registers:
	; save battery_gauge:6 into EEPROM 0x07-0x0C
	clrf	EEADRH
	movff	battery_gauge+0,EEDATA
	write_int_eeprom 0x07
	movff	battery_gauge+1,EEDATA
	write_int_eeprom 0x08
	movff	battery_gauge+2,EEDATA
	write_int_eeprom 0x09
	movff	battery_gauge+3,EEDATA
	write_int_eeprom 0x0A
	movff	battery_gauge+4,EEDATA
	write_int_eeprom 0x0B
	movff	battery_gauge+5,EEDATA
	write_int_eeprom 0x0C
	return


    global  vault_decodata_into_eeprom
vault_decodata_into_eeprom:
    ; Vault in EEPROM 512...1023
    ; Write 0xAA at 512 to indicate valid data in vault
    ; Store last time/date
    ; Store 0x700 to 0x780 (pres_tissue_N2 and pres_tissue_He)
    movlw   HIGH .512           ; =2
    movwf   EEADRH
    movlw   0xAA
    movwf   EEDATA
    write_int_eeprom .0
    ; Store date/time
    movff   year,EEDATA
    write_int_eeprom .1
    movff   month,EEDATA
    write_int_eeprom .2
    movff   day,EEDATA
    write_int_eeprom .3
    movff   hours,EEDATA
    write_int_eeprom .4
    movff   mins,EEDATA
    write_int_eeprom .5
    movff   secs,EEDATA
    write_int_eeprom .6

    movff   int_O_CNS_fraction+0,EEDATA
    write_int_eeprom .7
    movff   int_O_CNS_fraction+1,EEDATA
    write_int_eeprom .8
    movff   desaturation_time+0,EEDATA
    write_int_eeprom .9
    movff   desaturation_time+1,EEDATA
    write_int_eeprom .10
    movff   surface_interval+0,EEDATA
    write_int_eeprom .11
    movff   surface_interval+1,EEDATA
    write_int_eeprom .12
    movff   char_O_gradient_factor,EEDATA
    write_int_eeprom .13
    movff   nofly_time+0,EEDATA
    write_int_eeprom .14
    movff   nofly_time+1,EEDATA
    write_int_eeprom .15

    ; Tissue data from 16 to 144
    movlw   .16
    movwf   EEADR
    movlw   .128
    movwf   lo
    lfsr    FSR1,0x700;pres_tissue_N2+0       ; 32*4Byte Float = 128Bytes
vault_decodata_into_eeprom2:
    movff   POSTINC1,EEDATA
    call    write_eeprom                ; EEDATA into EEPROM@EEADR
    incf    EEADR,F
    decfsz  lo,F                        ; All done?
    bra     vault_decodata_into_eeprom2 ; No
    clrf    EEADRH
    return

    global  restore_decodata_from_eeprom
restore_decodata_from_eeprom:
    movlw   LOW  .512           ; =0
    movwf   EEADR
    movlw   HIGH .512           ; =2
    movwf   EEADRH

    ; Restore date/time
    read_int_eeprom .1
    movff   EEDATA,year
    read_int_eeprom .2
    movff   EEDATA,month
    read_int_eeprom .3
    movff   EEDATA,day
    read_int_eeprom .4
    movff   EEDATA,hours
    read_int_eeprom .5
    movff   EEDATA,mins
    read_int_eeprom .6
    movff   EEDATA,secs
    call    rtc_set_rtc

    read_int_eeprom .7
    movff   EEDATA,int_O_CNS_fraction+0
    read_int_eeprom .8
    movff   EEDATA,int_O_CNS_fraction+1
    read_int_eeprom .9
    movff   EEDATA,desaturation_time+0
    read_int_eeprom .10
    movff   EEDATA,desaturation_time+1
    read_int_eeprom .11
    movff   EEDATA,surface_interval+0
    read_int_eeprom .12
    movff   EEDATA,surface_interval+1
    read_int_eeprom .13
    movff   EEDATA,char_O_gradient_factor
    read_int_eeprom .14
    movff   EEDATA,nofly_time+0
    read_int_eeprom .15
    movff   EEDATA,nofly_time+1

    ; Tissue data from 16 to 144
    movlw   .16
    movwf   EEADR
    movlw   .128
    movwf   lo
    lfsr    FSR1,0x700;pres_tissue_N2+0       ; 32*4Byte Float = 128Bytes
restore_decodata_from_eeprom2:
    call    read_eeprom                ; EEPROM@EEADR into EEDATA
    movff   EEDATA,POSTINC1
    incf    EEADR,F
    decfsz  lo,F                        ; All done?
    bra     restore_decodata_from_eeprom2   ; No
    clrf    EEADRH
    return


 END