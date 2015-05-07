;=============================================================================
;
;   File surfmode.asm
;
;   Surfacemode
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-07 : [mH] moving from OSTC code

#include    "hwos.inc"                 ; Mandatory header
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include	"start.inc"
#include	"tft.inc"
#include	"tft_outputs.inc"
#include	"isr.inc"
#include	"adc_lightsensor.inc"
#include 	"menu_processor.inc"
#include	"strings.inc"
#include	"sleepmode.inc"
#include 	"wait.inc"                  ; speed_*
#include	"external_flash.inc"
#include	"customview.inc"
#include	"divemode.inc"
#include    "mcp.inc"                   ; RX
#include    "i2c.inc"
#include    "comm.inc"
#include    "eeprom_rs232.inc"
#include    "calibrate.inc"

   	extern   do_main_menu

#DEFINE	menu_pos_row		.215
#DEFINE	menu_pos_column		.1
#DEFINE	view_row            .215
#DEFINE	view_column         .124

gui     CODE


;=============================================================================
; Boot tasks for all modes
	global	surfloop
surfloop:
    call	speed_normal
    bcf     no_sensor_int           ; Normal pressure mode

    bcf     LEDg
    bcf     LEDr

    clrf	CCP1CON					; stop PWM
	bcf		PORTC,2					; Pull PWM output to GND
	call	TFT_boot                ; Initialize TFT (includes clear screen)
	bcf		restore_deco_data

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

    WIN_TOP     .100
    WIN_LEFT    .34
    extern  ostc_logo_block
    movlw   LOW(ostc_logo_block)
    movwf   TBLPTRL
    movlw   HIGH ostc_logo_block;&0xFFFF
    movwf   TBLPTRH
    movlw   UPPER(ostc_logo_block)
    movwf   TBLPTRU
    call    color_image
    call    TFT_Display_FadeIn      ; Show splash
	call	TFT_serial              ; Show serial and firmware version

    ;---- Do any usefull initializes that takes time -------------------------
	call	restart_set_modes_and_flags	; Sets decomode flags
	bcf		pressure_refresh
    call    I2C_init_compass
    call    I2C_init_accelerometer
	clrf	ext_flash_address+0
	clrf	ext_flash_address+1
	clrf	ext_flash_address+2

    btfsc   rechargeable
    call    piezo_config            ; Configure buttons

	clrf	timeout_counter2
	clrf 	timeout_counter3
	bcf		menubit						; clear menu flag
	clrf	last_pressure+0
	clrf	last_pressure+1
    bcf     is_bailout                  ; =1: Bailout
    bcf     ccr_diluent_setup           ; Use OC gases for gaslist routine

	bcf		simulatormode_active		; Quit simulator mode (if active)
	bcf		switch_left
	bcf		switch_right

	;---- Fade to standard surface view --------------------------------------
	; Wait 1 second
	bcf		onesecupdate
	btfss	onesecupdate
	bra		$-2
	; Wait 1 second
	bcf		onesecupdate
	btfss	onesecupdate
	bra		$-2

	call    TFT_Display_FadeOut         ; Go to black screen
	call	TFT_ClearScreen             ; Then change everything
	WIN_TOP		.0
	WIN_LEFT	.0
	WIN_FONT 	FT_SMALL
	bcf     win_invert              ; Reset invert flag

    WIN_COLOR	color_lightblue
	WIN_SMALL	menu_pos_column,menu_pos_row
    STRCPY_TEXT_PRINT  tMenu		;"<Menu"
	WIN_SMALL	view_column,view_row
    STRCPY_TEXT_PRINT  tView        ;"View>"
	call    TFT_standard_color

; Logo
    WIN_TOP     .0
    WIN_LEFT    .70
    movlw   LOW(ostc_logo_block)
    movwf   TBLPTRL
    movlw   HIGH ostc_logo_block;&0xFFFF
    movwf   TBLPTRH
    movlw   UPPER(ostc_logo_block)
    movwf   TBLPTRU
    call    color_image

	call	TFT_clock					; display time
    call    update_surfloop60
	call	get_battery_voltage			; get battery voltage
	call	TFT_update_batt_voltage		; display battery voltage
	call	TFT_update_surf_press		; display surface pressure
	call	TFT_temp_surfmode			; Displays temperature
	call	TFT_display_decotype_surface

    movff   opt_dive_mode,lo            ; 0=OC, 1=CC, 2=Gauge, 3=Apnea
    tstfsz  lo
    bra     surfloop_no_oc              ; Not OC
    call    TFT_show_OC_startgas_surface; Show first gas and "OSTC2-like" active gases
surfloop_no_oc:
    movff   customview_surfmode,menupos3    ; Reload last customview
    call    surf_customview_mask        ; Update #menupos3 view

    call    TFT_Display_FadeIn          ; Display resulting surface screen.

    ;---- Late initialisations -----------------------------------------------    
	movff	last_surfpressure_30min+0,int_I_pres_respiration+0		; copy surface air pressure to deco routine
	movff	last_surfpressure_30min+1,int_I_pres_respiration+1		; 30min old values 
	movff	last_surfpressure_30min+0,int_I_pres_surface+0			; copy surface air pressure to deco routine
	movff	last_surfpressure_30min+1,int_I_pres_surface+1			; 30min old values 
	movff	last_surfpressure_30min+0,last_surfpressure+0			; Use 30min old airpressure 
	movff	last_surfpressure_30min+1,last_surfpressure+1			; Use 30min old airpressure

    ; Startup tasks for all modes
    ; Desaturation time needs:
    ;   int_I_pres_surface
    ;   char_I_desaturation_multiplier
	call	deco_calc_desaturation_time ; calculate desaturation time
	movlb	b'00000001'					; select ram bank 1

    btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    call	enable_rs232				; Also sets to speed_normal ...

surfloop_loop:
	btfss	onesecupdate				; do every second tasks?
	bra		surfloop_loop2				; no, loop

; One Second tasks for all modes
	call	speed_normal
	call	TFT_debug_output
	call	TFT_clock					; update clock
	call	timeout_surfmode			; check timeout 
	call	get_battery_voltage			; get battery voltage
	call	TFT_update_batt_voltage		; display battery voltage
	call	set_dive_modes				; tests if depth>threshold
    btfss   secs,0                      ; Every two seconds...
	call	TFT_temp_surfmode			; Displays temperature
    btfss   secs,0                      ; Every two seconds...
    call    surfmode_check_for_warnings ; ... check for warnings (and display/update) them

	bcf		onesecupdate				; every second tasks done
	
surfloop_loop2:	
; Tasks approx. every 50ms for all modes
	call	test_switches_surfmode		; check switches

; One minute tasks for all modes
	btfsc	oneminupdate				; do every minute tasks
	call	update_surfloop60			; yes, e.g. update time and date

; Mode tasks
	btfsc	divemode					; Divemode active?
	goto	diveloop					; Yes, switch into Divemode!

	btfsc	menubit						; Menu?
	goto	do_main_menu				; Menu!

	btfsc	pressure_refresh			; new pressure available?
	call	TFT_update_surf_press		; display surface pressure
	bcf		pressure_refresh			; until new pressure is available

; Updates every 1/4 second
    btfss   quarter_second_update
    bra     surfloop_loop2a

    bcf     quarter_second_update
    ; Update Sensors

    call    compute_ppo2                ; compute mv_sensorX and ppo2_sensorX arrays
    call    check_sensors               ; Set enable/disable flags
    btfsc   FLAG_ccr_mode               ; In CCR mode...
    call    TFT_surface_sensor          ; ...update sensor data in surface mode

    movlw   .6
    cpfseq  menupos3                    ; in compass view?
    bra     surfloop_loop2a             ; No
    extern  TFT_surface_compass_heading
    call    TFT_surface_compass_heading ; Yes, update compass heading value

surfloop_loop2a:
	btfsc	toggle_customview			; Next view?
	call	surf_customview_toggle      ; Yes, show next customview (and delete this flag)

    btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    bra     surfloop_loop3
    btfsc   vusb_in                     ; USB plugged in?
    call    comm_mode                   ; Start COMM mode
    bra     surfloop_loop4
surfloop_loop3:
    btfss   vusb_in                     ; USB (still) plugged in?
    bcf     enable_screen_dumps         ; No, clear flag
    call    rs232_get_byte
    btfsc   rs232_recieve_overflow
    bra     surfloop_loop4
    movlw   "l"
    cpfseq	RCREG1
    bra     surfloop_loop4
    call    TFT_dump_screen             ; Dump the screen contents
surfloop_loop4:
	btfsc	sleepmode					; Sleepmode active?
	goto	sleeploop					; Yes, switch into sleepmode!

	bra		surfloop_loop				; loop surfacemode

update_surfloop60:
; One minute tasks for all modes
	call	TFT_date					; Update date
	call	calc_deko_surfmode			; calculate desaturation every minute
	bcf		oneminupdate				
	return

    extern  check_cns_violation,check_warn_battery,check_and_store_gf_violation
surfmode_check_for_warnings:
	movf	warning_counter_backup,W
	cpfseq	warning_counter						; warning_counter_backup = warning_counter?
	call	TFT_clear_warning_text              ; No, clear all warnings
	movff	warning_counter,warning_counter_backup	; copy warning_counter

	bcf		warning_active                      ; Clear flag
	clrf	warning_counter						; Clear counter

    ; Warnings for all modes
    call	check_warn_battery                  ; Check if the battery level should be displayed/warned
    rcall   surfmode_check_for_nofly            ; Check if nofly time should be shown
    rcall   surfmode_check_for_desat            ; Check if desat time should be shown
    rcall   surfmode_check_for_interval         ; Check if surface interval should be shown

	btfsc	FLAG_apnoe_mode             ; Done for Apnoe or Gauge mode
    bra     surfmode_check_for_warnings2
	btfsc	FLAG_gauge_mode             ; Done for Apnoe or Gauge mode
	bra     surfmode_check_for_warnings2

    ; Warnings only in deco modes
	call	check_cns_violation					; Check CNS value and display it, if required
    call	check_and_store_gf_violation		; Check GF value and display it, if required

surfmode_check_for_warnings2:
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

surfmode_check_for_interval:
	movf    	surface_interval+0,W            ; Is interval null ?
    iorwf   	surface_interval+1,W
    bnz     	surfmode_check_for_interval2	; No
    return
surfmode_check_for_interval2:
	incf    warning_counter,F			; increase counter
    call    TFT_interval
    return


surfmode_check_for_desat:
	movf    	desaturation_time+0,W           ; Is nofly null ?
    iorwf   	desaturation_time+1,W
    bnz     	surfmode_check_for_desat2		; No
    return
surfmode_check_for_desat2:
	incf	warning_counter,F			; increase counter
    call    TFT_desaturation_time
    return

surfmode_check_for_nofly:
 	movf    nofly_time+0,W              ; Is nofly null ?
    iorwf   nofly_time+1,W
    bnz     surfmode_check_for_nofly2   ; No...
    return
surfmode_check_for_nofly2:
	incf	warning_counter,F			; increase counter
    call    TFT_nofly_time
    return


;=============================================================================
	global	calc_deko_surfmode
calc_deko_surfmode:
    SAFE_2BYTE_COPY amb_pressure,int_I_pres_respiration ; copy surface air pressure to deco routine
	call	deco_calc_wo_deco_step_1_min    ; calculate deco in surface mode
	banksel		common
	return

test_switches_surfmode:		; checks switches in surfacemode
	btfsc	switch_right
	bra		test_switches_surfmode2
	btfsc	switch_left
	bra		test_switches_surfmode3		
	
	; No button press
	return

test_switches_surfmode3:
	bcf		switch_left
	bsf		menubit					; Enter Menu!
	return

test_switches_surfmode2:
	bcf		switch_right
	bsf		toggle_customview
    clrf	timeout_counter2        ; and reset timeout
	return

	global	timeout_surfmode
timeout_surfmode:
	movlw	timeout_surfacemode		; [s] Default timeout
    btfsc   menu_show_sensors2      ; In the "Calibrate" menu?
    movlw   timeout_calibrate_menu  ; [s] CCR Calibrate Menu timeout
    btfsc   menubit                 ; in Menu?
    bra     timeout_testmode        ; No, done.
    ; Must be in surface mode
    btfss   FLAG_ccr_mode           ; =1: CCR mode (Fixed ppO2 or Sensor) active
    bra     timeout_testmode        ; No, not CCR
    movlw   timeout_ccr_surface     ; [s] CCR Surface mode timeout

	global	timeout_testmode
timeout_testmode:
	incf	timeout_counter2,F		; increase timeout counter
	cpfsgt	timeout_counter2		; Compare with timeout_counter2
	return							; return, no timeout
	bsf		sleepmode				; Set Flag
	return							; Return

 END