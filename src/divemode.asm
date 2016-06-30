;=============================================================================
;
;   File divemode.asm
;
;   Divemode
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-15 : [mH] moving from OSTC code

#include    "hwos.inc"                  ; Mandatory header
#include 	"shared_definitions.h"         ; Mailbox from/to p2_deco.c
#include	"tft_outputs.inc"
#include 	"strings.inc"
#include 	"tft.inc"
#include	"eeprom_rs232.inc"
#include	"isr.inc"
#include	"math.inc"
#include	"wait.inc"
#include	"customview.inc"
#include	"start.inc"
#include	"adc_lightsensor.inc"
#include	"ghostwriter.inc"
#include    "i2c.inc"
#include    "calibrate.inc"
#include    "convert.inc"

gui     CODE

	global	diveloop
diveloop:
    banksel common
	call	speed_normal
	call	diveloop_boot			; Boot tasks for all modes

; Startup Tasks for all modes
    call	TFT_boot                ; Initialize TFT (includes clear screen)
	call	TFT_divemode_mask		; Display mask
	call	TFT_temp_divemode		; Displays temperature
    movff   customview_divemode,menupos3    ; Reload last customview
    call    customview_mask         ; Redraw last custom view

	btfsc	FLAG_apnoe_mode
	bsf		realdive					; Set Realdive flag in Apnoe mode

	btfsc	FLAG_apnoe_mode             ; Done for Apnoe or Gauge mode
    bra     diveloop_loop
	btfsc	FLAG_gauge_mode             ; Done for Apnoe or Gauge mode
	bra     diveloop_loop

	call	TFT_active_gas_divemode     ; Display gas/Setpoint
	call	TFT_display_ndl_mask		; display "NDL"

	; +@5 init
    setf        WREG                    ; WAIT marker: display "---"
    movff       WREG,int_O_extra_ascenttime+0
    movff       WREG,int_O_extra_ascenttime+1
	movlw       1
    movwf       apnoe_mins              ; Start compute after next cycle.

;--------------------------------------------------------------------------------------------------------
diveloop_loop:		; The diveloop starts here
	btfss	onesecupdate					
	bra		diveloop_loop3

; tasks any new second...
    bcf     onesecupdate					; one seconds update, clear flag here in case it's set again in ISR before all tasks are done.

	btfsc	FLAG_apnoe_mode					; Only in apnoe mode
	bra		diveloop_loop1b					; One Second Tasks in Apnoe mode

	call	TFT_divemins					; Display (new) divetime!
	call	customview_second				; Do every-second tasks for the custom view area
    call	divemode_check_for_warnings     ; Check for any warnings

; Tasks only for deco modes
	btfsc	show_safety_stop				; Show the safety stop?
	call	TFT_show_safety_stop			; Yes, show/delete if done.
	call	calc_deko_divemode				; calculate decompression and display result (any two seconds)
	bra		diveloop_loop1x					; Common Tasks

diveloop_loop1b:
; Tasks only for Apnoe mode
	rcall	divemode_apnoe_tasks			; 1 sec. Apnoe tasks
    call	customview_second				; Do every-second tasks for the custom view area
 	bra		diveloop_loop1x					; Common Tasks

diveloop_loop1x:
; Common 1sec. tasks for all modes
	rcall	timeout_divemode				; dive finished? This routine sets the required flags
	rcall	set_dive_modes                  ; tests if depth>threshold
	rcall	set_min_temp                    ; store min. temp if required

	btfsc	store_sample					; store new sample?
	call	store_dive_data					; Store profile data

	btfss	divemode						; Dive finished?
	goto	ghostwriter_end_dive    		; Dive finished!

    btfsc   divemode_gaschange              ; Gas switch flag set?
    rcall   gas_switched_common             ; Yes

    btfsc   toggle_gf                       ; =1: Toggle GF/aGF
    rcall   divemodemode_togglegf           ; Toggle aGF/GF

    btfsc   FLAG_ccr_mode                   ; In CCR mode...
    call    check_dive_autosp               ; ...check for Auto-SP

    call    compute_ppo2                    ; compute mv_sensorX and ppo2_sensorX arrays

diveloop_loop3:
	rcall	test_switches_divemode			; Check switches in divemode

    global  diveloop_loop4
diveloop_loop4:                             ; Menu-Exit returns here...
	btfsc	toggle_customview				; Next view?
	call	customview_toggle				; Yes, show next customview (and delete this flag)

	btfsc	pressure_refresh				; new pressure available?
	rcall	update_temp_and_or_depth        ; Yes, display new depth and clear 	"pressure_refresh" flag

	btfsc	oneminupdate					; one minute tasks
	rcall	update_divemode60				; Update clock, etc.

;    btfss   quarter_second_update
;    bra     diveloop_loop4a

    bcf     quarter_second_update
    movlw   .6
    cpfseq  menupos3                    ; in compass view?
    bra     diveloop_loop4a             ; No
    extern  TFT_dive_compass_heading
    call    TFT_dive_compass_heading    ; Yes, update compass heading value
diveloop_loop4a:
    btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    bra     diveloop_loop5
    bra     diveloop_loop6
diveloop_loop5:
    btfss   vusb_in                     ; USB (still) plugged in?
    bcf     enable_screen_dumps         ; No, clear flag
    call    rs232_get_byte
    btfsc   rs232_recieve_overflow
    bra     diveloop_loop6
    movlw   "l"
    cpfseq	RCREG1
    bra     diveloop_loop6
    call    TFT_dump_screen             ; Dump the screen contents
diveloop_loop6:

	bra		diveloop_loop					; Loop the divemode
;--------------------------------------------------------------------------------------------------------


divemode_apnoe_tasks:                       ; 1 sec. Apnoe tasks
	call	TFT_display_apnoe_descent		; Yes, Show descent timer
	call	TFT_max_pressure				; use normal max. depth

	btfsc	divemode2						; Time running?
	bra		divemode_apnoe_tasks2			; New descent, reset data if flag is set

	rcall	apnoe_calc_maxdepth
	call	TFT_display_apnoe_surface
	call	TFT_display_apnoe_last_max		; Show last max. depth
	incf	apnoe_surface_secs,F
	movlw	d'60'
	cpfseq	apnoe_surface_secs
	bra		divemode_apnoe_tasks1
	clrf	apnoe_surface_secs
	incf	apnoe_surface_mins,F

divemode_apnoe_tasks1:	
	bcf		FLAG_active_descent				; Clear flag
	btfsc	divemode2						; Time running?
	return									; Yes, return
	bsf		FLAG_active_descent				; Set Flag
	return

divemode_apnoe_tasks2:
	btfss	FLAG_active_descent				; Are we descending?
	return									; No, We are at the surface
	rcall	apnoe_calc_maxdepth				; Yes!
	call	TFT_apnoe_clear_surface			; Clear Surface timer
	clrf	apnoe_timeout_counter			; Delete timeout
	clrf	apnoe_surface_secs
	clrf	apnoe_surface_mins
	clrf	apnoe_secs
	clrf	apnoe_mins						; Reset Descent time
	movlw	.0
	movff	WREG,max_pressure+0
	movff	WREG,max_pressure+1				; Reset Max. Depth
	bcf		FLAG_active_descent				; Clear flag
	return

	global	apnoe_calc_maxdepth
apnoe_calc_maxdepth:
	movff	apnoe_max_pressure+0,sub_a+0
	movff	apnoe_max_pressure+1,sub_a+1
	movff	max_pressure+0,sub_b+0
	movff	max_pressure+1,sub_b+1
	call	subU16				; sub_c = sub_a - sub_b
								; apnoe_max_pressure<max_pressure -> neg_flag=1
								; max_pressure<=apnoe_max_pressure -> neg_flag=0
	btfss	neg_flag	
	return
								;apnoe_max_pressure<max_pressure
	movff	max_pressure+0,apnoe_max_pressure+0
	movff	max_pressure+1,apnoe_max_pressure+1
	return


calc_deko_divemode:
	btfsc	twosecupdate			; two seconds after the last call
	bra		calc_deko_divemode2		; Yes, calculate and display deco data ("first second")

	bsf		twosecupdate			; No, but next second!
	; Routines used in the "other second"
	call	calc_average_depth          ; calculate average depth
	call	calc_velocity               ; calculate vertical velocity and display if > threshold (every two seconds)
    call	set_reset_safety_stop       ; Set flags for safety stop and/or reset safety stop
	call	TFT_debug_output

	btfsc	FLAG_apnoe_mode             ; Done for Apnoe or Gauge mode
    return
	btfsc	FLAG_gauge_mode             ; Done for Apnoe or Gauge mode
	return

; Calculate CNS
    rcall   set_actual_ppo2             ; Set char_I_actual_ppO2
    clrf    WREG
    movff   WREG,char_I_step_is_1min    ; Make sure to be in 2sec mode.
	call	deco_calc_CNS_fraction		; calculate CNS
	movlb	b'00000001'					; rambank 1 selected
; Check for a gas change
	bra	check_gas_change			; Checks if a better gas should be selected (by user) and return...

    global  set_actual_ppo2
set_actual_ppo2:                        ; calculate ppO2 in 0.01bar (e.g. 150 = 1.50 bar ppO2)
    btfsc   divemode                    ; in divemode
    bra     set_actual_ppo2_dive        ; Yes
    ; No, use simulated ambient pressure for char_I_actual_ppO2
    movff   char_I_bottom_depth,WREG
    mullw   .100
    movlw   LOW(.1000)
    addwf   PRODL,W
    movwf   xA+0
    movlw   HIGH(.1000)
    addwfc  PRODH,W
    movwf   xA+1                        ; P_amb in millibar (1000 = 1.00 bar).
    bra     set_actual_ppo2_common
set_actual_ppo2_dive:
    SAFE_2BYTE_COPY amb_pressure, xA    ; P_amb in millibar (1000 = 1.00 bar).
set_actual_ppo2_common:
 	movlw		d'10'
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; xC=p_amb/10 (100 = 1.00 bar).
	movff		xC+0,xA+0
	movff		xC+1,xA+1
	movff		char_I_O2_ratio,xB+0
	clrf		xB+1
	call		mult16x16				; char_I_O2_ratio * (p_amb/10)
	movff		xC+0,xA+0
	movff		xC+1,xA+1
	movlw		d'100'
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; xC=(char_I_O2_ratio * p_amb/10)/100

; Copy ppO2 for CNS calculation
    tstfsz      xC+1                    ; Is ppO2 > 2.55bar ?
    setf        xC+0                    ; yes: bound to 2.55... better than wrap around.

    movff		xC+0, char_I_actual_ppO2	; copy last ppO2 to buffer register
    btfsc       is_bailout                  ; In Bailout?
    return                                  ; Yes, done.
    ; No Bailout, check for ccr mode
    btfsc		FLAG_ccr_mode               ; If FLAG_ccr_mode=1...
    movff		char_I_const_ppO2, char_I_actual_ppO2	; ...copy last ppO2 to buffer register
    return

check_fallback_clear:
    call    check_sensors                   ; Setups "use_O2_sensorX" flags
    ; Copy use flags to voting logic flags in case we are no longer in fallback
    btfsc   use_O2_sensor1
    bsf     voting_logic_sensor1
    btfsc   use_O2_sensor2
    bsf     voting_logic_sensor2
    btfsc   use_O2_sensor3
    bsf     voting_logic_sensor3
    rcall   divemode_setup_sensor_values    ; Setup sensor values
    goto    check_sensors                   ; Check O2 sensor thresholds for fallback customview_minute

calc_deko_divemode2:
	bcf		twosecupdate

	btfsc	FLAG_apnoe_mode             ; Done for Apnoe or Gauge mode
    return
	btfsc	FLAG_gauge_mode             ; Done for Apnoe or Gauge mode
	return

	extern	deco_setup_dive
	call	deco_setup_dive				;  Pass all parameters to the C code


    btfsc   setpoint_fallback               ; Are we in Fallback?
    rcall   check_fallback_clear            ; Yes, check if we still have fallback condition

    bcf     setpoint_fallback               ; =1: Fallback to SP1 due to external O2 sensor failure

    btfsc   FLAG_ccr_mode                   ; In CCR mode?
    rcall    calc_deko_divemode_sensor      ; External sensor stuff

	SAFE_2BYTE_COPY amb_pressure,int_I_pres_respiration ; C-code needs the ambient pressure
	clrf	WREG
	movff	WREG,char_I_step_is_1min    ; Force 2 second deco mode

    clrf    TMR5L
    clrf    TMR5H                       ; 30,51757813µs/bit in TMR5L:TMR5H
	call	deco_calc_hauptroutine		; calc_tissue
    movlb   .1

    movff   char_O_deco_status,WREG     ; Is a compute cycle finished ?
    iorwf   WREG,F
    btfss   STATUS,Z
    return                              ; Return is status <> 0

    ; Check if deco stops are necessary ?
	movff	char_O_first_deco_depth,wait_temp	; copy ceiling to temp register
	tstfsz	wait_temp							; Ceiling<0m?
	bra		calc_deko_divemode3					; Yes!

	btfsc	decostop_active             ; Already in nodeco mode ?
	call	TFT_display_ndl_mask       	; No, Clear deco data, display nostop time
	bcf		decostop_active             ; clear flag (again)

	; Copy for profile recording
	clrf	decodata+0
	movff	char_O_nullzeit,decodata+1  ; NDL
	
	goto	TFT_display_ndl            ; display no deco limit, and return...

calc_deko_divemode3:
	btfss	decostop_active            ; Already in deco mode ?
	call	TFT_display_deko_mask      ; No, clear nostop time, display decodata
	bsf		decostop_active            ; Set flag (again)

	; Copy for profile recording
	movff	char_O_first_deco_depth,decodata+0	; ceiling
	movff	char_O_first_deco_time,decodata+1	; length of first stop in minues
	call	TFT_display_deko            ; display decodata
	call	TFT_show_TTS_divemode       ; display TTS

    movff   char_I_extra_time,WREG
    tstfsz  WREG                    ; extra time = 0?
    bra     calc_deko_divemode4     ; No, compute it
    return

calc_deko_divemode4:
    ; Check if extra cycles are needed to compute @5 variant:
	decfsz  apnoe_mins,F                ; Reached count-down ?
	return                              ; No: don't compute yet.
	
	movlw   .6
	movff   WREG,char_O_deco_status     ; Stole next cycles for @5 variant.
    
    movlw   .2                          ; Restart countdown.
    movwf   apnoe_mins
 	return                              ; done.

calc_deko_divemode_sensor:                  ; External sensor stuff
    movff   opt_ccr_mode,WREG               ; =0: Fixed SP, =1: Sensor,  =2: Auto SP
    sublw   .1                              ; opt_ccr_mode = 1 (Sensor)?
    bnz     calc_deko_divemode_sensor_done  ; No, return

    rcall   divemode_setup_sensor_values    ; Setup sensor values
    call    check_sensors                   ; Check O2 sensor thresholds for fallback
    movff   sensor_setpoint,char_I_const_ppO2; Copy sensor result

    TSTOSS  opt_sensor_fallback             ; =1: Fallback to SP1 when sensor is lost
    return                                  ; Never fallback

    btfsc   is_bailout                      ; In bailout?
    return                                  ; Never fallback in bailout
    ; Check if we should fallback to SP1
  	btfsc	use_O2_sensor1
    return                                  ; At least one sensor is active, no fallback
	btfsc	use_O2_sensor2
	return                                  ; At least one sensor is active, no fallback
	btfsc	use_O2_sensor3
	return                                  ; At least one sensor is active, no fallback
    ; No sensor in use -> fallback
    movff   char_I_setpoint_cbar+0,char_I_const_ppO2    ; Setup fixed Setpoint (Always fallback to SP1), overwrite sensor result
    bsf     setpoint_fallback               ; =1: Fallback to SP1 due to external O2 sensor failure
calc_deko_divemode_sensor_done:
    return
   
;-----------------------------------------------------------------------------

divemodemode_togglegf:                  ; Toggle aGF/GF
    bcf     toggle_gf                   ; clear flag    
    btg     use_agf                     ; Toggle GF
    call    TFT_gf_mask                 ; Setup Mask
    clrf    WREG
    movff   WREG,char_O_deco_status     ; Restart decoplan computation
    return

    global  divemode_setup_sensor_values
divemode_setup_sensor_values:
    ; sum up sensor values (in xA:2) and active sensors in (xB:2)
    clrf    xB+0
    clrf    xB+1
    clrf    xA+0
    clrf    xA+1
    btfss   use_O2_sensor1                  ; Sensor1 active?
    bra     divemode_setup_sensor_values2   ; No
    btfss   voting_logic_sensor1            ; Sensor within voting logic?
    bra     divemode_setup_sensor_values2   ; No
    movf    o2_ppo2_sensor1,W
    addwf   xA+0
    movlw   .0
    addwfc  xA+1                            ; Add into xA:2
    incf    xB+0,F                          ; Add a sensor
divemode_setup_sensor_values2:
    btfss   use_O2_sensor2                  ; Sensor2 active?
    bra     divemode_setup_sensor_values3   ; No
    btfss   voting_logic_sensor2            ; Sensor within voting logic?
    bra     divemode_setup_sensor_values3   ; No
    movf    o2_ppo2_sensor2,W
    addwf   xA+0
    movlw   .0
    addwfc  xA+1                            ; Add into xA:2
    incf    xB+0,F                          ; Add a sensor
divemode_setup_sensor_values3:
    btfss   use_O2_sensor3                  ; Sensor3 active?
    bra     divemode_setup_sensor_values4   ; No
    btfss   voting_logic_sensor3            ; Sensor within voting logic?
    bra     divemode_setup_sensor_values4   ; No
    movf    o2_ppo2_sensor3,W
    addwf   xA+0
    movlw   .0
    addwfc  xA+1                            ; Add into xA:2
    incf    xB+0,F                          ; Add a sensor
divemode_setup_sensor_values4:
    call    div16x16						; xA/xB=xC with xA+0 as remainder
    movff   xC+0,sensor_setpoint            ; Copy result
    return

calc_velocity:								; called every two seconds
    btfsc   display_velocity                
    bra     calc_velocity1                  ; Always update if already displayed
	btfss	divemode2						
	return                      			; display velocity only if deepter then 1m (Not at the surface after the dive)
calc_velocity1:
    SAFE_2BYTE_COPY amb_pressure, sub_a
	movff	last_pressure_velocity+0,sub_b+0
	movff	last_pressure_velocity+1,sub_b+1
	movff	sub_a+0,last_pressure_velocity+0	; store old value for velocity
	movff	sub_a+1,last_pressure_velocity+1

	call	subU16						; sub_c = amb_pressure - last_pressure

    bcf     neg_flag_velocity
    btfsc   neg_flag
    bsf     neg_flag_velocity

	movff	sub_c+0,xA+0
	movff	sub_c+1,xA+1
	movlw	d'39'						; 77 when called every second....
	movwf	xB+0
	clrf	xB+1
	call	mult16x16					; differential pressure in mbar*77...
	movff	xC+0,divA+0
	movff	xC+1,divA+1
	movlw	d'7'
	movwf	divB+0
	call	div16						; devided by 2^7 equals velocity in m/min

	movlw	d'99'
	cpfsgt	divA+0                      ; limit to 99m/min
	bra		calc_velocity3
	movwf	divA+0						; divA=99

calc_velocity3:
    ; Copy old speeds
    movff   old_velocity+2,old_velocity+3
    movff   old_velocity+1,old_velocity+2
    movff   old_velocity+0,old_velocity+1
    movff   divA+0,old_velocity+0
    
;    movff   old_velocity+3,WREG
;    addwf   divA+0,F                    ; add old speed
;    bcf     STATUS,C
;    rrcf    divA+0,F                    ; /2
;    movff   old_velocity+2,WREG
;    addwf   divA+0,F                    ; add old speed
;    bcf     STATUS,C
;    rrcf    divA+0,F                    ; /2
;    movff   old_velocity+1,WREG
;    addwf   divA+0,F                    ; add old speed
;    bcf     STATUS,C
;    rrcf    divA+0,F                    ; /2
;    movff   old_velocity+0,WREG
;    addwf   divA+0,F                    ; add old speed
;    bcf     STATUS,C
;    rrcf    divA+0,F                    ; /2
	goto	TFT_display_velocity		; With divA+0 = m/min..., and return...


;=============================================================================

set_reset_safety_stop:						; Set flags for safety stop and/or reset safety stop
    TSTOSS  opt_enable_safetystop           ; =1: A safety stop is shown
    bra		delete_safety_stop              ; No, don't show safety stop

	btfsc	decostop_active					; Is a deco stop displayed?
	bra		delete_safety_stop				; Yes, don't show safety stop
	; Below "opt_safety_stop_reset"? Set flag and reset count-down timer
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity		; computes salinity setting into lo:hi [mbar]
	movff	lo,sub_a+0
	movff	hi,sub_a+1
    movff   opt_safety_stop_reset,WREG      ; [cbar]
    mullw   .10                             ; mbar in PRODL:H
    movff   PRODL,sub_b+0
    movff   PRODH,sub_b+1
	call	subU16							;  sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		reset_safety_stop				; Below 10m, reset safety stop

	; Above "opt_safety_stop_end"? Clear flag.
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity		; computes salinity setting into lo:hi [mbar]
	movff	lo,sub_a+0
	movff	hi,sub_a+1
    movff   opt_safety_stop_end,WREG      ; [cbar]
    mullw   .10                             ; mbar in PRODL:H
    movff   PRODL,sub_b+0
    movff   PRODH,sub_b+1
	call	subU16							;  sub_c = sub_a - sub_b
	btfsc	neg_flag
	bra		delete_safety_stop				; Above 3m, remove safety stop

	; Above "opt_safety_stop_start"? Activate safety stop
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity		; computes salinity setting into lo:hi [mbar]
	movff	lo,sub_a+0
	movff	hi,sub_a+1
    movff   opt_safety_stop_start,WREG      ; [cbar]
    mullw   .10                             ; mbar in PRODL:H
    movff   PRODL,sub_b+0
    movff   PRODH,sub_b+1
	call	subU16							;  sub_c = sub_a - sub_b
	btfsc	neg_flag
	bra		acivate_safety_stop				; Above 5m, activate safety stop
	bra		reset_safety_stop2				; Pause safety stop

acivate_safety_stop:
	tstfsz	safety_stop_countdown			; Countdown at zero?
	bsf		show_safety_stop				; No, Set flag!
	return

delete_safety_stop:
	clrf	safety_stop_countdown			; reset timer
	bra		reset_safety_stop2				; Remove safety stop from display

reset_safety_stop:
    movff   opt_safety_stop_length,safety_stop_countdown	; reset timer
reset_safety_stop2:
	bcf		show_safety_stop				; Clear flag
	btfss	safety_stop_active				; Safety stop shown
	return									; No, don't delete it
	bcf		safety_stop_active				; Clear flag
    goto    TFT_clear_safety_stop           ; Clear safety stop, and return...


;=============================================================================

timeout_menuview:
    decfsz  timeout_counter3,F          ; timeout for menuview
    return                              ; No timeout, return
    ; Timeout, clear e.g. "Menu?"
    goto    menuview_toggle_reset       ; "returns"

timeout_divemode_menu:
    decfsz  timeout_counter3,F          ; timeout for divemode menu
    return

    global  timeout_divemode_menu2
timeout_divemode_menu2:                 ; Called from divemenu_tree.asm
    bcf     divemode_menu               ; Timeout! Clear flag
    call    TFT_clear_divemode_menu     ; Clear menu
    call    TFT_active_gas_divemode     ; Redraw gas/setpoint/diluent
    bcf     blinking_better_gas         ; Clear flag to have temperature updated once
    call	TFT_temp_divemode           ; Displays temperature
    call    TFT_draw_gassep_line        ; Gas separator grid in spec mode only

    btfss   decostop_active             ; In deco mode ?
    bra     timeout_divemode_menu_ndl   ; No, show NDL again
    ; Show deco
	call	TFT_display_deko_mask      ; clear nostop time, display decodata
    call    TFT_display_deko
    goto    TFT_show_TTS_divemode; and return...

timeout_divemode_menu_ndl:              ; Show NDL
	call	TFT_display_ndl_mask       	; Clear deco data, display nostop time
    goto    TFT_display_ndl; and return...

timeout_divemode:
    btfsc   divemode_menu               ; Divemode menu active?
    rcall   timeout_divemode_menu       ; Yes, check the timeout for it...

    btfsc   menuview                    ; is a menuview shown?
    rcall   timeout_menuview            ; Yes, check the timeout for it...

	btfss	realdive					; Dive longer then one minute
	return
	
	btfsc	FLAG_apnoe_mode				; In Apnoe mode?
	bra		timeout_divemode2			; Yes, use apnoe_timeout [min] for timeout

    ifndef __DEBUG
    	btfsc	simulatormode_active    ; In Simulator mode?
    	bra		timeout_divemode3       ; Yes, use simulator timeout
    endif
	
	bcf		divemode
	infsnz  timeout_counter,F
    incf    timeout_counter2,F			; timeout is 15bits

	movlw	LOW		divemode_timeout
	movwf	sub_a+0
	movlw	HIGH	divemode_timeout
	movwf	sub_a+1

	movff	timeout_counter, sub_b+0
	movff	timeout_counter2, sub_b+1
	call	subU16						;  sub_c = sub_a - sub_b
	btfss	neg_flag					; Result negative?
	bsf		divemode					; No, set flag
	return

timeout_divemode2:
	incf	timeout_counter,F			; seconds...
	movlw	d'60'
	cpfseq	timeout_counter				; timeout_counter=60?
	return								; No.
; One minute timeout done.
	clrf	timeout_counter
	bcf		divemode
	incf	apnoe_timeout_counter,F
	movlw	apnoe_timeout				; apnoe timeout [min]
	cpfseq	apnoe_timeout_counter
	bsf		divemode
	return

timeout_divemode3:
	bcf		divemode
	incf	timeout_counter,F
	movlw	simulator_timeout   		; simulator timeout
	cpfsgt	timeout_counter
	bsf		divemode
	return

update_temp_and_or_depth:			    ; New sensor data arrived...
	btfsc	temp_changed	
	call	TFT_temp_divemode		    ; Displays temperature

	btfsc	pressure_refresh
	call	TFT_depth					; Displays new depth

	rcall	set_max_depth               ; update max. depth if required
	bcf		pressure_refresh			; until new pressure is available
	return

update_divemode60:                      ; update any minute
	call	get_battery_voltage			; gets battery voltage
	rcall	set_powersafe				; Battery low?
	call	TFT_max_pressure			; Update max. depth
	call	customview_minute			; Do every-minute tasks for the custom view area
	bcf		oneminupdate

    btfss   simulatormode_active        ; in simulator mode?
	return                              ; No
	; Yes, quite dive mode simulation after 21*256s=89min:36s
	movlw	.20
	cpfsgt	total_divetime_seconds+1	; Timeout?
	return                              ; No
    ifdef __DEBUG
    return                              ; No simulator timeout in debug mode
    endif
	bra		divemode_option1            ; Yes, set to 0m and "return"

set_max_depth:
	movff	max_pressure+0,sub_a+0
	movff	max_pressure+1,sub_a+1
    SAFE_2BYTE_COPY rel_pressure, sub_b
	call	subU16                      ; sub_c = sub_a - sub_b
                                        ; max_pressure<rel_pressure -> neg_flag=1
                                        ; rel_pressure<=max_pressure -> neg_flag=0
	btfss	neg_flag	
	return
                                        ; max_pressure<rel_pressure
	movff	sub_b+0,max_pressure+0
	movff	sub_b+1,max_pressure+1
	goto	TFT_max_pressure			; No, use normal max. depth; and return...

set_min_temp:
	movff	minimum_temperature+0,sub_a+0
	movff	minimum_temperature+1,sub_a+1
    SAFE_2BYTE_COPY temperature,sub_b
	call	sub16                       ; sub_c = sub_a - sub_b
                                        ; minimum_temperature<T -> neg_flag=1
                                        ; T<=minimum_temperature -> neg_flag=0
	btfsc	neg_flag	
	return
                                        ; minimum_temperature>=T
	movff	sub_b+0,minimum_temperature+0
	movff	sub_b+1,minimum_temperature+1
	return

	global	set_dive_modes
set_dive_modes:
	btfsc	high_altitude_mode		; In high altitude (Fly) mode?
	bra		set_dive_modes3			; Yes!

set_dive_modes0:
	movlw	LOW		start_dive_threshold
	movwf	sub_a+0					; dive_treshold is in cm
	movlw	HIGH	start_dive_threshold
	movwf	sub_a+1					; dive_treshold is in cm

set_dive_modes1:
    SAFE_2BYTE_COPY rel_pressure, sub_b
	call	subU16					; sub_c = sub_a - sub_b

	btfss	neg_flag	
	bra		set_dive_modes2			; too shallow (rel_pressure<dive_threshold)

	btfsc	realdive				; Dive longer than one minute?
	clrf 	timeout_counter			; Yes, reset timout counter

	bsf		divemode				; (Re-)Set divemode flag
	bsf		divemode2				; displayed divetime is running
	return

set_dive_modes2:
	bcf		divemode2				; Stop time
	btfss	realdive				; dive longer then one minute?
	bcf		divemode				; no -> this was no real dive
	return							; No, return


set_dive_modes3:					; High-altitude mode
	btfsc	realdive				; dive longer then one minute?
	bra		set_dive_modes0			; Yes -> this is a real dive -> Use start_dive_threshold or ascend

	movlw	HIGH	high_altitude_dive_threshold
	movwf	sub_a+1
	movlw	LOW		high_altitude_dive_threshold
	movwf	sub_a+0
	bra		set_dive_modes1

set_powersafe:
    movlw   color_code_battery_low+1; [%]
    cpfslt  batt_percent
	return

	movlw	d'7'					; Type of Alarm (Battery Low)
	movwf	AlarmType				; Copy to Alarm Register
	bsf		event_occured			; Set Event Flag
    movlw   .0
    movff   WREG,opt_brightness     ; Set Brightness to ECO
	return							; return

calc_average_depth:
	btfsc	reset_average_depth		; Reset the Average depth?
	rcall	reset_average1			; Reset the resettable average depth

	; 1. Add new 2xdepth to the Sum of depths registers
    SAFE_2BYTE_COPY rel_pressure, xB	; Buffer...
    bcf     STATUS,C
    rlcf    xB+0,F
    rlcf    xB+1,F                  ; x2

	movf	xB+0,w
	addwf	average_depth_hold+0,F
	movf	xB+1,w
	addwfc	average_depth_hold+1,F
	movlw	d'0'
	addwfc	average_depth_hold+2,F
	addwfc	average_depth_hold+3,F ; Will work up to 9999mbar*60*60*24=863913600mbar

; Do the same for the _total registers (Non-Resettable)
	movf	xB+0,w
	addwf	average_depth_hold_total+0,F
	movf	xB+1,w
	addwfc	average_depth_hold_total+1,F
	movlw	d'0'
	addwfc	average_depth_hold_total+2,F
	addwfc	average_depth_hold_total+3,F ; Will work up to 9999mbar*60*60*24=863913600mbar

	; 2. Compute Average Depth on base of average_divesecs:2
	movff	average_divesecs+0,xB+0
	movff	average_divesecs+1,xB+1		; Copy
	movff	average_depth_hold+0,xC+0
	movff	average_depth_hold+1,xC+1
	movff	average_depth_hold+2,xC+2
	movff	average_depth_hold+3,xC+3

	call	div32x16 	; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	xC+0,avg_rel_pressure+0
	movff	xC+1,avg_rel_pressure+1

    btfss   divemode2                   ; displayed divetime is running?
	return                              ; No (e.g. too shallow)

	; 3. Compute Total Average Depth on base of total_divetime_seconds:2
	movff	total_divetime_seconds+0,xB+0
	movff	total_divetime_seconds+1,xB+1		; Copy
	movff	average_depth_hold_total+0,xC+0
	movff	average_depth_hold_total+1,xC+1
	movff	average_depth_hold_total+2,xC+2
	movff	average_depth_hold_total+3,xC+3
	call	div32x16 	; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	xC+0,avg_rel_pressure_total+0
	movff	xC+1,avg_rel_pressure_total+1
	return

reset_average1:
	clrf	average_depth_hold+0
	clrf	average_depth_hold+1
	clrf	average_depth_hold+2
	clrf	average_depth_hold+3		; Clear average depth register
	movlw	d'2'
	movwf	average_divesecs+0
	clrf	average_divesecs+1
	bcf		reset_average_depth			; Clear flag
	return

test_switches_divemode:		; checks switches in divemode
    btfsc   divemode_menu               ; Divemode menu shown?
    bra     test_switches_divemode_menu ; Yes, use menu processor
	btfsc	switch_left
	goto    menuview_toggle         ; Menu or Simulator tasks; and return...; bra		test_switches_divemode2		; Enter button pressed, check if we need to do something
	btfss	switch_right
	return                              ; No button press
    tstfsz  menupos2                    ; any option shown?
    bra     test_switches_divemode1     ; Yes, do option tasks
	bsf		toggle_customview           ; No, toggle custom view
    return

test_switches_divemode_menu:
	btfsc	switch_left
	bra		test_switches_divemode_menu2    ; Move cursor
	btfsc	switch_right
    bra		test_switches_divemode_menu3    ; Enter submenu or do something
    return                                  ; No button press

test_switches_divemode_menu1:
    clrf    menupos
test_switches_divemode_menu2:
    incf    menupos,F
    incf    menupos4,W                  ; menupos4+1 -> WREG
    cpfslt  menupos                     ; >menupos4 (Set in menu_processor.asm)?
    bra     test_switches_divemode_menu1; > Yes, set to 1
    call    TFT_divemode_menu_cursor    ; Update the cursor
    bcf     switch_left
	movlw	divemode_menu_timeout       ; Reload timeout
	movwf	timeout_counter3            ; timeout for divemode menu
    return

test_switches_divemode_menu3:           ; Enter submenu or do something
    bcf     switch_right
;    decf    menupos,F                   ; menu_processor needs 0-5...
    extern  do_line_menu
    goto    do_line_menu                ; Warning! Trashes STKPTR and returns to diveloop_loop4:

test_switches_divemode1:
	bcf		switch_right
    movlw   divemode_menuview_timeout
    movwf   timeout_counter3            ; Reload timeout
	movff	menupos2,WREG               ; menupos2 holds number of customview/divemode menu function
	dcfsnz	WREG,F
    bra		divemode_option_gaschange	; Switch to the indicated "better gas"
	dcfsnz	WREG,F
    bra		divemode_option0			; Start/Setup Divemode menu
	dcfsnz	WREG,F
	bra		divemode_option1			; Quit Simulation?
	dcfsnz	WREG,F
	bra		divemode_option2			; Descent 1m
	dcfsnz	WREG,F
	bra		divemode_option3			; Ascend 1m
	dcfsnz	WREG,F
	bra		divemode_option4			; Quit Apnoe mode
	dcfsnz	WREG,F
	bra		divemode_option5			; Reset Stopwatch (In Gauge mode)
	dcfsnz	WREG,F
	bra		divemode_option6			; +5mins simulation
	dcfsnz	WREG,F
	bra		divemode_option7			; Store heading
    return

;test_switches_divemode2:
;    goto    menuview_toggle         ; Menu or Simulator tasks; and return...
;    return

gas_switched_common:
    bcf     divemode_gaschange      ; Clear flag

    tstfsz  menupos                 ; menupos=0?
    bra     $+4                     ; No
    return                          ; Yes, do not switch gas (There is no Gas #0 !)

    decf    menupos,W               ; 1-5 -> 0-4
    btfss   FLAG_ccr_mode           ; Choose OC Gases
    rcall   setup_gas_registers     ; With WREG=Gas 0-4
    btfsc   FLAG_ccr_mode           ; Choose CC Diluents
    rcall   setup_dil_registers     ; With WREG=Gas 0-4

    decf    menupos,W               ; 1-5 -> 0-4
    btfsc   is_bailout              ; Choose OC Bailouts (OC Gases)
    rcall   setup_gas_registers     ; With WREG=Gas 0-4

    call	TFT_active_gas_divemode	; Display gas/Setpoint
    clrf    WREG
    movff   WREG,char_O_deco_status ; Restart decoplan computation

    ; Set flags for profile recording
	bsf		event_occured			; Set global event byte
    btfsc   is_bailout              ; Choose OC Bailouts (OC Gases)
    bsf     bailoutgas_event        ; Bailout gas change
    btfss   is_bailout              ; Choose OC Bailouts (OC Gases)
	bsf		stored_gas_changed		; OC gas change
	return

    global  setup_gas_registers
setup_gas_registers:                ; With WREG=Gas 0-4
    lfsr    FSR1,opt_gas_O2_ratio+0
    movff   PLUSW1,char_I_O2_ratio  ; O2 (For ppO2 calculations)
    lfsr    FSR1,opt_gas_He_ratio+0
    movff   PLUSW1,char_I_He_ratio  ; He
    incf    WREG,W                  ; Gas# 1-5
	movff	WREG,char_I_current_gas	; Set gas
	movff	WREG,active_gas			; Set for logbook and display
    banksel char_I_O2_ratio
	movf    char_I_O2_ratio,W       ; Add O2...
    addwf   char_I_He_ratio,W       ; ...and He...
	sublw   .100                    ; ...subtract both from 100
	movwf   char_I_N2_ratio         ; -> N2!
    banksel common
    return

    global  setup_dil_registers
setup_dil_registers:                ; With WREG=dil 0-4
    btfsc   is_bailout
    return                          ; Ignore in bailout
    lfsr    FSR1,opt_dil_O2_ratio+0
    movff   PLUSW1,char_I_O2_ratio  ; O2 (For ppO2 calculations)
    lfsr    FSR1,opt_dil_He_ratio+0
    movff   PLUSW1,char_I_He_ratio  ; He
    incf    WREG,W                  ; Gas# 1-5
	movff	WREG,char_I_current_gas	; Set gas
	movff	WREG,active_gas			; Set for logbook and display
    movff   WREG,active_diluent     ; As a backup when switching back from Bailout to CCR
    banksel char_I_O2_ratio
	movf    char_I_O2_ratio,W       ; Add O2...
    addwf   char_I_He_ratio,W       ; ...and He...
	sublw   .100                    ; ...subtract both from 100
	movwf   char_I_N2_ratio         ; -> N2!
    banksel common
    return

divemode_option_gaschange:          ; Switch to the better gas
    movff   better_gas_number,menupos; 1-5
    bsf     divemode_gaschange      ; Change the gas in the dive mode loop...
    call    menuview_toggle_reset   ; Reset to zero (Zero=no menuview)
    bcf     better_gas_available    ; Clear flag immediately
    return

divemode_option0:                   ; Start/Setup Divemode menu
    call    TFT_clear_divemode_menu ; Clear menu area
    bcf     menuview
    extern  do_main_divemenu
    call    do_main_divemenu
    global  divemode_option0_return
divemode_option0_return:
;    movlw   .1
;    movwf   menupos                 ; Set to first option in divemode menu
    call    TFT_divemode_menu_cursor; Show the cursor
	movlw	divemode_menu_timeout
	movwf	timeout_counter3        ; timeout for divemode menu
    bsf     divemode_menu           ; Set flag
    clrf    menupos2                ; Clear option counter
    bra     diveloop_loop4          ; Goto back to diveloop (Menuprocessor trashes STKPTR!)

divemode_option4:
	movlw	d'58'					; two seconds left
	movwf	timeout_counter
	movlw	apnoe_timeout-1			; apnoe timeout [min]
	movwf	apnoe_timeout_counter
    btfss   simulatormode_active	; in simulator mode?
	return							; No
divemode_option1:						; Quit simulation mode
	banksel	isr_backup
    movlw   low     .1000
    movwf	sim_pressure+0
    movlw   high    .1000
	movwf   sim_pressure+1			; Set to 0m -> End of Dive
	banksel common
    call    menuview_toggle_reset   ; Reset to zero (Zero=no menuview)
	
	btfss	FLAG_apnoe_mode			; In Apnoe mode?
	return							; No
	movlw	d'58'					; two seconds left
	movwf	timeout_counter
	movlw	apnoe_timeout-1			; apnoe timeout [min]
	movwf	apnoe_timeout_counter
	return

divemode_option3:			; minus 1m
	banksel	isr_backup
	movlw	d'100'
	subwf	sim_pressure+0
	movlw	.0
	subwfb	sim_pressure+1
	rcall	divemode_simulator_check_limits
	banksel common
	return

divemode_option2:			; plus 1m
	banksel	isr_backup
	movlw	d'100'
	addwf	sim_pressure+0
	movlw	.0
	addwfc	sim_pressure+1
	rcall	divemode_simulator_check_limits
	banksel common
	return

divemode_option5:
    call    menuview_toggle_reset   ; Reset to zero (Zero=no menuview)
    bsf     reset_average_depth     ; Set Flag
    return

divemode_option6:
    bcf     divemode2                   ; Stop divetime
    movlw   .5
    addwf   divemins+0,F
    movlw   .0
    addwfc  divemins+1,F                ; Add 5 mins
    movlw   LOW     (.5*.60)
    addwf   average_divesecs+0,F
    movlw   HIGH    (.5*.60)
    addwfc  average_divesecs+1,F        ; Add 5*60 seconds
    movlw   LOW     (.5*.60)
    addwf   total_divetime_seconds+0,F
    movlw   HIGH    (.5*.60)
    addwfc  total_divetime_seconds+1,F  ; Add 5*60 seconds
; 1. Add 300xdepth to the Sum of depths registers
    SAFE_2BYTE_COPY rel_pressure, xB	; Buffer...
    movlw   LOW     (.5*.60)
    movwf   xA+0
    movlw   HIGH    (.5*.60)
    movwf   xA+1
    call    mult16x16           		;xA*xB=xC

	movf	xC+0,w
	addwf	average_depth_hold+0,F
	movf	xC+1,w
	addwfc	average_depth_hold+1,F
	movf	xC+2,w
	addwfc	average_depth_hold+2,F
    movf	xC+3,w
	addwfc	average_depth_hold+3,F ; Will work up to 9999mbar*60*60*24=863913600mbar

; Do the same for the _total registers (Non-Resettable)
	movf	xC+0,w
	addwf	average_depth_hold_total+0,F
	movf	xC+1,w
	addwfc	average_depth_hold_total+1,F
	movf	xC+2,w
	addwfc	average_depth_hold_total+2,F
    movf	xC+3,w
	addwfc	average_depth_hold_total+3,F ; Will work up to 9999mbar*60*60*24=863913600mbar

    movlw   .5
    movwf   up                          ; counter
; 1min mode
divemode_option6_2:
	movlw   .1
	movff	WREG,char_I_step_is_1min    ; Force 1min mode
    clrf    TMR5L
    clrf    TMR5H                       ; 30,51757813µs/bit in TMR5L:TMR5H
	call	deco_calc_hauptroutine		; calc_tissue
    movlb   .1
    decfsz  up,F                        ; Done?
    bra     divemode_option6_2          ; Not yet
    bsf     divemode2                   ; continue divetime
    goto    menuview_toggle_reset	; and return...

divemode_option7:
    ; Store heading for compass view
    movff   compass_heading_shown+0,compass_bearing+0
    movff   compass_heading_shown+1,compass_bearing+1
    bsf     compass_bearing_set         ; set flag
    goto    menuview_toggle_reset       ; Done and return...

divemode_simulator_check_limits:
	; Check limits (150m and 0m)
	movlw	LOW		d'16000'            ; Compare to 16bar=16000mbar (150m).
	subwf   sim_pressure+0,W
	movlw	HIGH	d'16000'
	subwfb  sim_pressure+1,W
	bnc     divemode_simulator_check_limits2 ; No-carry = borrow = not deeper

	; Too deep, limit to 150m
	movlw	LOW		d'16000'
	movwf	sim_pressure+0
	movlw	HIGH	d'16000'
	movwf	sim_pressure+1
	return
divemode_simulator_check_limits2:
	movlw	LOW		d'1000'             ; Compare to 1bar == 0m == 1000 mbar.
	subwf   sim_pressure+0,W
	movlw	HIGH	d'1000'
	subwfb  sim_pressure+1,W
	btfsc   STATUS,C                    ; No-carry = borrow = not deeper.
	return                              ; Deeper than 0m == Ok.
	; Too shallow, limit to 0m
	movlw	LOW		d'1000'
	movwf	sim_pressure+0
	movlw	HIGH	d'1000'
	movwf	sim_pressure+1
	return

;=============================================================================
; Compare all enabled gas in list, to see if a better one is available.
;
; Output: better_gas_available, better_gas_number
;
check_gas_change:					; Checks if a better gas should be selected (by user)
	bcf		better_gas_available    ;=1: A better gas is available and a gas change is advised in divemode
	clrf	better_gas_number       ; Clear better gas register

    SAFE_2BYTE_COPY rel_pressure,xA
	movlw	d'100'
	movwf	xB+0
	clrf	xB+1
	call	div16x16				; compute depth in full m -> result in xC+0

    btfss   FLAG_ccr_mode           ; In CCR mode...
    bra     check_gas_change_OC_bail; No, check for OC or bailout
    btfsc   is_bailout              ; Bailout?
    bra     check_gas_change_OC_bail; Yes, check for OC or bailout

    ; Check Diluents
    bsf     ccr_diluent_setup       ; in CCR menus
    movlw   .0
    rcall   check_dil_common        ; With Gas 0-4 in WREG
    movlw   .1
    rcall   check_dil_common        ; With Gas 0-4 in WREG
    movlw   .2
    rcall   check_dil_common        ; With Gas 0-4 in WREG
    movlw   .3
    rcall   check_dil_common        ; With Gas 0-4 in WREG
    movlw   .4
    rcall   check_dil_common        ; With Gas 0-4 in WREG
    bra     check_gas_change_exit

check_gas_change_OC_bail:
    bcf     ccr_diluent_setup       ; in OC menus
    movlw   .0
    rcall   check_gas_common        ; With Gas 0-4 in WREG
    movlw   .1
    rcall   check_gas_common        ; With Gas 0-4 in WREG
    movlw   .2
    rcall   check_gas_common        ; With Gas 0-4 in WREG
    movlw   .3
    rcall   check_gas_common        ; With Gas 0-4 in WREG
    movlw   .4
    rcall   check_gas_common        ; With Gas 0-4 in WREG
;    bra     check_gas_change_exit
check_gas_change_exit:
    btfss	better_gas_available	; Is a better gas available
    bcf     blinking_better_gas     ; No, Clear blinking flag
    btfss	better_gas_available	; Is a better gas available
    clrf    better_gas_number		; No, Clear better_gas_number (For gaslist display)
    goto    TFT_active_gas_divemode ; Display gas/Setpoint and return...

check_gas_common:                   ; With Gas 0-4 in WREG
    btfsc   better_gas_available	; Better Gas already found?
    return                          ; Yes, return
    lfsr    FSR1,opt_gas_type       ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    btfss   PLUSW1,0                ; Test for Bit0 and 1 -> type=3 -> Deco
    return                          ; No
    btfss   PLUSW1,1                ; Test for Bit0 and 1 -> type=3 -> Deco
    return                          ; No
    incf    WREG,W                  ; 1-5
    cpfseq  active_gas				; is this gas currently selected?
    bra     check_gas_common2       ; No
    return                          ; Yes, skip test for active gas
check_gas_common2:
    decf    WREG,W                  ; 0-4
    movwf   hi                      ; Save tested gas 0-4
    lfsr    FSR1,opt_OC_bail_gas_change
    movff   PLUSW1,lo               ; Change depth into lo
	movlw	minimum_change_depth
	cpfsgt	lo  					; Change depth>minimum_change_depth?
	return                          ; No, Change depth not deep enough, skip!
	movf	xC+0,W					; load depth in m into WREG
	cpfsgt	lo  					; gas_change_depth < current depth?
    bra     check_gas_common3       ; No, check if we are within the better_gas_window_pos window
	incf    hi,W                    ; 1-5
	movwf	better_gas_number		; number (1-5) of the "better gas" in divemode, =0: no better gas available
	movlw	better_gas_window_neg
	subwf	lo,W                    ; Change depth-better_gas_window_neg
	cpfslt	xC+0					; current depth<Change depth-better_gas_window_neg?
	bsf		better_gas_available	;=1: A better gas is available and a gas change is advised in divemode
    return

check_gas_common3:
	incf    hi,W                    ; 1-5
	movwf	better_gas_number		; number (1-5) of the "better gas" in divemode, =0: no better gas available
	movlw	better_gas_window_pos
	addwf	lo,W                    ; Change depth+better_gas_window_pos
	cpfsgt	xC+0					; current depth>Change depth+better_gas_window_pos?
    bra     check_gas_common4       ; Ok, now check the better gas ppO2<opt_ppO2_max
    return

check_gas_common4:
    movf    hi,W                    ; 0-4
    lfsr    FSR1,char_I_deco_N2_ratio
    movff   PLUSW1,lo               ; N2 ratio into lo
    lfsr    FSR1,char_I_deco_He_ratio
    movff   PLUSW1,xB+0             ; He ratio into xB+0
    movf    xB+0,W
    addwf   lo,F
    movlw   .101
    bcf     STATUS,C
    subfwb  lo,F                    ; O2 ratio in lo

    SAFE_2BYTE_COPY amb_pressure, xA
	movlw	d'10'
	movwf	xB+0
	clrf	xB+1
	call	div16x16				; xC=p_amb/10
	movff	xC+0,xA+0
	movff	xC+1,xA+1
    movff   lo,xB+0                 ; =O2 ratio
	clrf	xB+1
	call	mult16x16               ; lo * p_amb/10

    ; Check very high ppO2 manually
	tstfsz	xC+2				; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	return                      ; Done.
    ; Check if ppO2>3,30bar
	btfsc   xC+1,7
	return                      ; Done.

;    ; Check for low ppo2
;    movff	xC+0,sub_b+0
;	movff	xC+1,sub_b+1
;    movff   opt_ppO2_min,WREG
;	mullw	d'100'				; opt_ppO2_min*100
;	movff	PRODL,sub_a+0
;	movff	PRODH,sub_a+1
;	call	subU16
;	btfss	neg_flag
;    return                      ; Done (Too low).

;check if we are within our warning thresholds!
	movff	xC+0,sub_b+0
	movff	xC+1,sub_b+1
	movff	opt_ppO2_max,WREG	; PPO2 Max for MOD calculation and color coding in divemode
    addlw   .1                  ; e.g. >1.60
	mullw	d'100'				; opt_ppO2_max*100
	movff	PRODL,sub_a+0
	movff	PRODH,sub_a+1
	call	subU16
	btfss	neg_flag
	bsf		better_gas_available	;=1: A better gas is available and a gas change is advised in divemode
    return                      ; Done.

check_dil_common:                   ; With Dil 0-4 in WREG
    btfsc   better_gas_available	; Better Diluent already found?
    return                          ; Yes, return
    lfsr    FSR1,opt_dil_type       ; 0=Disabled, 1=First, 2=Normal
    tstfsz  PLUSW1                  ; =0?
    bra     check_dil_common1       ; No
    return                          ; Yes, skip inactive diluents for test
check_dil_common1:
    incf    WREG,W                  ; 1-5
    cpfseq  active_gas				; is this diluent currently selected?
    bra     check_dil_common2       ; No
    return                          ; Yes, skip test for active diluent
check_dil_common2:
    decf    WREG,W                  ; 0-4
    movwf   hi                      ; Save tested diluent 0-4
    lfsr    FSR1,char_I_dil_change
    movff   PLUSW1,lo               ; Change depth into lo
	movlw	minimum_change_depth
	cpfsgt	lo  					; Change depth>minimum_change_depth?
	return                          ; No, Change depth not deep enough, skip!
	movf	xC+0,W					; load depth in m into WREG
	cpfsgt	lo  					; gas_change_depth < current depth?
    return                          ; No, check next gas
	incf    hi,W                    ; 1-5
	movwf	better_gas_number		; number (1-5) of the "better gas" in divemode, =0: no better gas available
	movlw	better_gas_window_neg
	subwf	lo,W                    ; Change depth-better_gas_window_neg
	cpfslt	xC+0					; current depth<Change depth-better_gas_window_neg?
	bsf		better_gas_available	;=1: A better gas is available and a gas change is advised in divemode
    return


;=============================================================================
; Check for Auto-SP
;
check_dive_autosp:               ; Check for Auto-SP
    movff   opt_ccr_mode,WREG               ; =0: Fixed SP, =1: Sensor,  =2: Auto SP
    sublw   .2                              ; opt_ccr_mode = 2 (Auto SP)?
    bz      check_dive_autosp2              ; Yes, check
    return                                  ; No, return for Sensor or Fixed mode
check_dive_autosp2:
    SAFE_2BYTE_COPY rel_pressure,xA
	movlw	d'100'
	movwf	xB+0
	clrf	xB+1
	call	div16x16				; compute depth in full m -> result in xC+0
    ; Check SP2
    btfsc   sp2_switched            ;=1: This setpoint has been autoselected already
    bra     check_dive_autosp3      ; Skip check
    movff   char_I_setpoint_change+1,lo ; Get depth in m
    tstfsz  lo                      ; =0?
    bra     $+4                     ; No, continue
    bra     check_dive_autosp3      ; Skip check
    decf    lo,W                    ; -1 -> WREG
    cpfsgt  xC+0                    ; Compare with depth
    bra     check_dive_autosp3      ; lower depth, do not switch
    ; auto switch to SP2
	movff	char_I_setpoint_cbar+1, char_I_const_ppO2	; Use SetPoint
    rcall   xmit_sp_set_flag
    bsf     sp2_switched            ; Set flag
check_dive_autosp3:
    ; Check SP3
    btfsc   sp3_switched            ;=1: This setpoint has been autoselected already
    bra     check_dive_autosp4      ; Skip check
    movff   char_I_setpoint_change+2,lo ; Get depth in m
    tstfsz  lo                      ; =0?
    bra     $+4                     ; No, continue
    bra     check_dive_autosp4      ; Skip check
    decf    lo,W                    ; -1 -> WREG
    cpfsgt  xC+0                    ; Compare with depth
    bra     check_dive_autosp4      ; lower depth, do not switch
    ; auto switch to SP3
	movff	char_I_setpoint_cbar+2, char_I_const_ppO2	; Use SetPoint
    rcall   xmit_sp_set_flag
    bsf     sp3_switched            ; Set flag
check_dive_autosp4:
    ; Check SP4
    btfsc   sp4_switched            ;=1: This setpoint has been autoselected already
    bra     check_dive_autosp5      ; Skip check
    movff   char_I_setpoint_change+3,lo ; Get depth in m
    tstfsz  lo                      ; =0?
    bra     $+4                     ; No, continue
    bra     check_dive_autosp5      ; Skip check
    decf    lo,W                    ; -1 -> WREG
    cpfsgt  xC+0                    ; Compare with depth
    bra     check_dive_autosp5      ; lower depth, do not switch
    ; auto switch to SP4
	movff	char_I_setpoint_cbar+3, char_I_const_ppO2	; Use SetPoint
    rcall   xmit_sp_set_flag
    bsf     sp4_switched            ; Set flag
check_dive_autosp5:
    ; Check SP5
    btfsc   sp5_switched            ;=1: This setpoint has been autoselected already
    bra     check_dive_autosp6      ; Skip check
    movff   char_I_setpoint_change+4,lo ; Get depth in m
    tstfsz  lo                      ; =0?
    bra     $+4                     ; No, continue
    bra     check_dive_autosp6      ; Skip check
    decf    lo,W                    ; -1 -> WREG
    cpfsgt  xC+0                    ; Compare with depth
    bra     check_dive_autosp6      ; lower depth, do not switch
    ; auto switch to SP5
	movff	char_I_setpoint_cbar+4, char_I_const_ppO2	; Use SetPoint
    rcall   xmit_sp_set_flag
    bsf     sp5_switched            ; Set flag
check_dive_autosp6:
    return

xmit_sp_set_flag:
    movff   char_I_const_ppO2,WREG
    call    transmit_setpoint           ; Transmit current setpoint from WREG (in cbar) to external electronics
    bsf     setpoint_changed        ; Set flag (For profile)
    bsf		event_occured			; Set global event byte
    return

;=============================================================================
; Setup everything to enter divemode.
;
dive_boot_oc:
    extern  get_first_gas_to_WREG
    call    get_first_gas_to_WREG           ; Gets first gas (0-4) into WREG
    movff   WREG,char_I_first_gas           ; Copy for compatibility
    rcall   setup_gas_registers             ; With WREG=Gas 0-4
    banksel char_I_first_gas
    incf    char_I_first_gas,F              ; 0-4 -> 1-5
    banksel common
    return

dive_boot_cc:
    bcf     is_bailout                      ; =1: Bailout
    bcf     setpoint_fallback               ; =1: Fallback to SP1 due to external O2 sensor failure
    bcf		blinking_setpoint               ; Reset blinking SP flag
    call    compute_ppo2                    ; compute mv_sensorX and ppo2_sensorX arrays
    bsf     voting_logic_sensor1
    bsf     voting_logic_sensor2
    bsf     voting_logic_sensor3
    call   divemode_setup_sensor_values     ; setup sensor values

    ; Setup first SP for Fixed or Auto mode
    movff   opt_ccr_mode,WREG               ; =0: Fixed SP, =1: Sensor,  =2: Auto SP
    sublw   .1                              ; opt_ccr_mode = 1 (Sensor)?
    movff   char_I_setpoint_cbar+0,char_I_const_ppO2    ; No, setup fixed Setpoint (Always start with SP1)
    movff   char_I_const_ppO2,WREG
    call    transmit_setpoint           ; Transmit current setpoint from WREG (in cbar) to external electronics
    bsf     setpoint_changed                ; Set flag (For profile)
    bcf     sp2_switched                    ; =1: This setpoint has been autoselected already
    bcf     sp3_switched                    ; =1: This setpoint has been autoselected already
    bcf     sp4_switched                    ; =1: This setpoint has been autoselected already
    bcf     sp5_switched                    ; =1: This setpoint has been autoselected already

    extern  get_first_dil_to_WREG
    call    get_first_dil_to_WREG           ; Gets first gas (0-4) into WREG
    movff   WREG,char_I_first_gas           ; Copy for compatibility
    rcall   setup_dil_registers             ; With WREG=Gas 0-4
    banksel char_I_first_gas
    incf    char_I_first_gas,F              ; 0-4 -> 1-5
    banksel common
    goto  calc_deko_divemode_sensor        ; External sensor stuff and return...

diveloop_boot:
	call	restart_set_modes_and_flags

    call    I2C_sleep_accelerometer          ; Stop accelerometer
    call    I2C_sleep_compass                ; Stop compass

	clrf	WREG
	movff	WREG,max_pressure+0				; clear some variables
	movff	WREG,max_pressure+1

    bcf     use_agf                         ; Start with normal GF set
    bcf     divemode_menu                   ; clear divemode menu flag
	movlw	d'1'
	movwf	apnoe_max_pressure+0
	clrf	apnoe_max_pressure+1
;	clrf	apnoe_surface_mins
;	clrf	apnoe_surface_secs		
	clrf	apnoe_mins
	clrf	divemins+0
	clrf	divemins+1
    
; Copy date and time for logbook
    movff   year,start_year
    movff   month,start_month
    movff   day,start_day
    movff   hours,start_hours
    movff   mins,start_mins

    bcf     no_more_divesecs            ; =1: Do no longer show seconds in divemode
	bcf		divemode_menu_active
    clrf    menupos
    clrf    menupos2                    ; Reset to zero (Zero=no premenu or simulator task)

    btfss   FLAG_ccr_mode
    rcall   dive_boot_oc
    btfsc   FLAG_ccr_mode
    rcall   dive_boot_cc

    ; Copy opt_dil_types into backup (For "lost gas" feature)
    movff   opt_dil_type+0,opt_dil_type_backup+0    ; 0=Disabled, 1=First, 2=Normal
    movff   opt_dil_type+1,opt_dil_type_backup+1    ; 0=Disabled, 1=First, 2=Normal
    movff   opt_dil_type+2,opt_dil_type_backup+2    ; 0=Disabled, 1=First, 2=Normal
    movff   opt_dil_type+3,opt_dil_type_backup+3    ; 0=Disabled, 1=First, 2=Normal
    movff   opt_dil_type+4,opt_dil_type_backup+4    ; 0=Disabled, 1=First, 2=Normal
    ; Copy opt_gas_types into backup (For "lost gas" feature)
    movff   opt_gas_type+0,opt_gas_type_backup+0    ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    movff   opt_gas_type+1,opt_gas_type_backup+1    ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    movff   opt_gas_type+2,opt_gas_type_backup+2    ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    movff   opt_gas_type+3,opt_gas_type_backup+3    ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    movff   opt_gas_type+4,opt_gas_type_backup+4    ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    ; Also copy change depths into backup (For "lost gas" feature)
    movff   opt_OC_bail_gas_change+0,opt_OC_bail_gas_change_backup+0 ; Gas change depths OC/Bailout
    movff   opt_OC_bail_gas_change+1,opt_OC_bail_gas_change_backup+1 ; Gas change depths OC/Bailout
    movff   opt_OC_bail_gas_change+2,opt_OC_bail_gas_change_backup+2 ; Gas change depths OC/Bailout
    movff   opt_OC_bail_gas_change+3,opt_OC_bail_gas_change_backup+3 ; Gas change depths OC/Bailout
    movff   opt_OC_bail_gas_change+4,opt_OC_bail_gas_change_backup+4 ; Gas change depths OC/Bailout

	bcf		better_gas_available        ;=1: A better gas is available and a gas change is advised in divemode
	clrf	better_gas_number           ; Clear better gas register

	bcf		show_safety_stop			;=1: Show the safety stop
	clrf	safety_stop_countdown		; Clear count-down

 	clrf	samplesecs
	clrf	apnoe_timeout_counter		; timeout in minutes
	clrf 	timeout_counter				; takes care of the timeout (Low byte)
	clrf 	timeout_counter2			; takes care of the timeout (High byte)
	clrf	AlarmType					; Clear all alarms
	bcf		event_occured				; clear flag
	clrf 	total_divetime_seconds+1
	clrf	average_depth_hold_total+0
	clrf	average_depth_hold_total+1
	clrf	average_depth_hold_total+2
	clrf	average_depth_hold_total+3	; Clear Non-Resettable Average
    rcall	reset_average1				; Reset the resettable average depth
    bcf		decostop_active
	bcf		better_gas_available        ;=1: A better gas is available and a gas change is advised in divemode
	call	ghostwriter_short_header	; Write short header with divenumber into profile memory

    btfsc   simulatormode_active
    bra     diveloop_boot_1
    ; Normal mode = Surface pressure is the pressure 30mn before dive.
	SAFE_2BYTE_COPY last_surfpressure_30min, int_I_pres_surface	;copy surfacepressure to deco routine
	SAFE_2BYTE_COPY last_surfpressure_30min, last_surfpressure	;copy surfacepressure to last_surfpressure for correct depth
    bra     diveloop_boot_2

diveloop_boot_1:
    ; Simulator mode: Surface pressure is 1bar.
    movlw   LOW .1000
	movff	WREG,int_I_pres_surface+0   ; LOW copy surfacepressure to deco routine
    movlw   HIGH .1000
	movff	WREG,int_I_pres_surface+1   ; HIGH copy surfacepressure to deco routine

diveloop_boot_2:
	SAFE_2BYTE_COPY	temperature,minimum_temperature ; Reset Min-Temp registers

; Init profile recording parameters	
	movff	samplingrate,samplesecs_value            ; to avoid EEPROM access in the ISR
	movlw	div_temperature
	movwf	divisor_temperature         ; load divisors for profile storage
	movlw	div_deco
	movwf	divisor_deco				
	movlw	div_gf
	movwf	divisor_gf
	movlw	div_ppo2_sensors
	movwf	divisor_ppo2_sensors
	movlw	div_decoplan
	movwf	divisor_decoplan
	movlw	div_cns
	movwf	divisor_cns
	movlw	div_tank
	movwf	divisor_tank

	btfss	FLAG_apnoe_mode				; In Apnoe mode?
	bra		divemode_boot1
; Overwrite some parameters in Apnoe mode....
	movlw	samplingrate_apnoe
	movwf	samplesecs_value			; to avoid EEPROM access in the ISR
divemode_boot1:
    btfsc   FLAG_ccr_mode               ; =1: CCR mode (Fixed ppO2 or Sensor) active
    bra     divemode_boot2
    ; in OC Mode, disable ppO2 logging
    movlw   .0
    movwf   divisor_ppo2_sensors
divemode_boot2:

	bcf		LEDg
	bcf		LEDr
	bcf		realdive
	btfss	simulatormode_active		; do not disable in simulator mode!					
	call	disable_rs232				; Disable RS232
    btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    call	enable_rs232				; Also sets to speed_normal ...
    ; Reset divetime seconds
    movlw   .2                          ; Start at 2seconds
	movwf   total_divetime_seconds+0
	movwf   divesecs
    movwf	apnoe_secs
	bsf		divemode2                   ; displayed divetime is running (Divetime starts HERE)

	movff	int_O_CNS_fraction+0,CNS_start+0
    movff	int_O_CNS_fraction+1,CNS_start+1        ; Save CNS value at beginning of dive
    movff   char_O_relative_gradient_GF,GF_start    ; Save GF value at  beginning of dive
	return								; Done with divemode boot

divemode_check_for_warnings:
    movlw   .2
    cpfsgt  warning_counter						; only two warnings active?
    bra     divemode_check_for_warnings1        ; Yes, update every second

    btfss   secs,0                      ; Every two seconds...
    return
    btfss   secs,1                      ; Every four seconds...
    return

divemode_check_for_warnings1:
	movf	warning_counter_backup,W
	cpfseq	warning_counter						; warning_counter_backup = warning_counter?
	call	TFT_clear_warning_text              ; No, clear all warnings
	movff	warning_counter,warning_counter_backup	; copy warning_counter

	bcf		warning_active				; Clear flag
	clrf	warning_counter						; Clear counter

    ; Warnings for all modes
    call	check_warn_battery                  ; Check if the battery level should be displayed/warned
    call    check_divetimeout                   ; Not actually a warning. Check and show the divemode timeout

	btfsc	FLAG_apnoe_mode             ; Done for Apnoe or Gauge mode
    bra     divemode_check_for_warnings2
	btfsc	FLAG_gauge_mode             ; Done for Apnoe or Gauge mode
	bra     divemode_check_for_warnings2

    ; Warnings only in deco modes
    btfss   FLAG_ccr_mode                       ; Don't check in CCR mode
	rcall	check_ppO2							; check ppO2 and displays warning, if required
    btfsc   is_bailout                          ; But check in Bailout case...
	rcall	check_ppO2							; check ppO2 and displays warning, if required
	rcall	check_cns_violation					; Check CNS value and display it, if required
	btfsc	decostop_active						; In deco mode?
	rcall	check_and_store_gf_violation		; Yes, Sets warnings, if required
	btfsc	decostop_active						; In deco mode?
    call    TFT_ftts                            ; Show @+x time
    btfsc   use_agf                             ; In aGF mode?
    rcall   warn_agf                            ; Yes, show a warning for it
    btfsc   setpoint_fallback                   ; =1: Fallback to SP1 due to external O2 sensor failure
    rcall   warn_fallback                       ; Show the warning

divemode_check_for_warnings2:
; Display the warning icon?
	btfsc	warning_active				; Any warning active?
	call	TFT_divemode_warning				; Yes
	btfss	warning_active				; Any warning active?
	call	TFT_divemode_warning_clear			; No, clear warning icon

; Setup warning_page number
    incf    warning_page,F
    bcf     STATUS,C
    rlcf    warning_page,W                      ; *2
    cpfsgt  warning_counter                     ; > warning_counter
    clrf    warning_page                        ; No, clear

; Clear 2nd row of warnings if there is nothing to show (on this page)
    btfss   second_row_warning                  ; =1: The second row contains a warning
    call    TFT_clear_warning_text_2nd_row      ; No, clear this row
    return                                      ; Done.

    global  check_warn_battery
check_warn_battery:
    movff   batt_percent,lo
	movlw	battery_show_level+1
	cpfslt	lo
    return                              ; No Display, no warning
    ; Display Battery, but warn?
	incf	warning_counter,F			; increase counter
    call	TFT_update_batt_percent_divemode    ; Show percent

    movff   batt_percent,lo
	movlw	color_code_battery_low+1
	cpfslt	lo                          ;
	return                              ; No warning
	bsf		warning_active		; Set Warning flag
	return

check_divetimeout:
    btfsc		divemode2				
    return                              ; displayed divetime is not running
	incf	warning_counter,F			; increase counter
    goto    TFT_divetimeout             ; Show timeout counter  (and return)


check_ppO2:							    ; check current ppO2 and display warning if required
    SAFE_2BYTE_COPY amb_pressure, xA
	movlw	d'10'
	movwf	xB+0
	clrf	xB+1
	call	div16x16				; xC=p_amb/10

	movff	xC+0,xA+0
	movff	xC+1,xA+1
    movff   char_I_O2_ratio,xB+0    ; =O2 ratio
	clrf	xB+1
	call	mult16x16               ; char_I_O2_ratio * p_amb/10

    ; Check very high ppO2 manually
	tstfsz	xC+2				; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra		check_ppO2_1		; Yes, display Value!
    ; Check if ppO2>3,30bar
	btfsc   xC+1,7
	bra     check_ppO2_1		; Yes!

    ; Check for low ppo2
    movff	xC+0,sub_b+0
	movff	xC+1,sub_b+1
    movff   opt_ppO2_min,WREG
	mullw	d'100'				; opt_ppO2_min*100
	movff	PRODL,sub_a+0
	movff	PRODH,sub_a+1
	call	subU16
	btfsc	neg_flag
	bra     check_ppO2_0        ; Not too low
    ; ppO2 low
    rcall   check_ppo2_display  ; display if not already shown in custom view
	movlw	d'4'				; Type of Alarm (ppO2 low)
	movwf	AlarmType			; Copy to Alarm Register
	bsf		event_occured		; Set Event Flag
	bsf		warning_active		; Set Warning flag
	return						; Done.

check_ppO2_0:
    ; Check if ppO2 should be displayed
    movlw   .0
    TSTOSS  opt_showppo2		; 0=no, 1=always show
    movlw   ppo2_display_high
	mullw	d'100'				; ppo2_display_high*100
	movff	PRODL,sub_a+0
	movff	PRODH,sub_a+1
	call	subU16
	btfss	neg_flag
	return						; No Display, no warning
    ; Display ppO2, but warn?
    rcall   check_ppo2_display  ; display if not already shown in custom view

;check if we are within our warning thresholds!
	movff	xC+0,sub_b+0
	movff	xC+1,sub_b+1
	movff	opt_ppO2_max,WREG	; PPO2 Max for MOD calculation and color coding in divemode
    addlw   .1                  ; e.g. >1.60
	mullw	d'100'				; opt_ppO2_max*100
	movff	PRODL,sub_a+0
	movff	PRODH,sub_a+1
	call	subU16					
	btfss	neg_flag
	return						; Done. Not too high
	movlw	d'5'				; Type of Alarm (ppO2 high)
	movwf	AlarmType			; Copy to Alarm Register
	bsf		event_occured		; Set Event Flag
	bsf		warning_active		; Set Warning flag
	return						; Done.

check_ppO2_1:                   ; ppO2 very high
    rcall   check_ppo2_display  ; display if not already shown in custom view
	movlw	d'5'				; Type of Alarm
	movwf	AlarmType			; Copy to Alarm Register
	bsf		event_occured		; Set Event Flag
	bsf		warning_active		; Set Warning flag
	return						; Done.

check_ppo2_display:
    movlw   .9
    cpfseq  menupos3            ; ppO2 shown in Custom View?
    bra     check_ppO2_a        ; No
    return                      ; Yes, do not show twice (in custom view and in warning area)
check_ppO2_a:
	incf	warning_counter,F	; increase counter
    goto    TFT_display_ppo2	; Show ppO2  (and return)

    global  check_cns_violation
check_cns_violation:
    ; Check if CNS should be displayed
    movff   int_O_CNS_fraction+1,lo		; copy into bank1
    tstfsz  lo                          ; >255% ?
    bra     check_cns_violation2        ; Yes
	movff	int_O_CNS_fraction+0,lo		; copy into bank1

	movlw	cns_warning_high			; cns_warning_high
	subwf	lo,W
	btfsc	STATUS,C
	bsf		warning_active              ; Set Warning flag

	movlw	cns_display_high			; cns_display_high
	subwf	lo,W
	btfss	STATUS,C
	return                              ; No Display, no warning
    ; Display CNS
	incf	warning_counter,F			; increase counter
	goto	TFT_display_cns				; Show CNS  (and return)

check_cns_violation2:
	incf	warning_counter,F			; increase counter
    bsf		warning_active              ; Set Warning flag
	goto    TFT_display_cns				; Show CNS  (and return)

    global  check_and_store_gf_violation
check_and_store_gf_violation:
	movff	char_O_gradient_factor,lo			; gradient factor absolute (Non-GF model)
	movff	char_I_deco_model,hi
	decfsz	hi,F		; jump over next line if char_I_deco_model == 1
	movff	char_O_relative_gradient_GF,lo		; gradient factor relative (GF model)

	movlw	gf_warning_high
	cpfsgt	lo
	bra     check_and_store_gf_violation2   ; No warning
	movlw	d'2'                        ; Type of Alarm
	movwf	AlarmType                   ; Copy to Alarm Register
	bsf		event_occured               ; Set Event Flag
	bsf		warning_active              ; Set Warning flag
check_and_store_gf_violation2:
	movlw	gf_display_high
	cpfsgt	lo
    return                              ; No Display, no warning
    ; Display GF
	incf	warning_counter,F			; increase counter
    goto    TFT_warning_gf              ; Show GF Warning (and return)

warn_agf:
	incf	warning_counter,F			; increase counter
	goto    TFT_warning_agf             ; Show aGF warning  (and return)

warn_fallback:
    incf	warning_counter,F			; increase counter
    bsf		warning_active              ; Set Warning flag
	goto    TFT_warning_fallback        ; Show fallback warning  (and return)


 END