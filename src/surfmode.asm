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

#include    "ostc3.inc"                 ; Mandatory header
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

#DEFINE	menu_pos_row		.215
#DEFINE	menu_pos_column		.0
#DEFINE	view_row            .215
#DEFINE	view_column         .124

   	extern   do_main_menu

gui     CODE


;=============================================================================
; Boot tasks for all modes
	global	surfloop
surfloop:
    call	speed_normal
    bcf     no_sensor_int           ; Normal pressure mode

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
    extern  ostc3_logo_block
    movlw   LOW(ostc3_logo_block)
    movwf   TBLPTRL
    movlw   HIGH ostc3_logo_block;&0xFFFF
    movwf   TBLPTRH
    movlw   UPPER(ostc3_logo_block)
    movwf   TBLPTRU
    call    color_image
    call    TFT_Display_FadeIn      ; Show splash
	call	TFT_serial              ; Show serial and firmware version

    ;---- Do any usefull initializes that takes time -------------------------
	call	restart_set_modes_and_flags	; Sets decomode flags
;	call	speed_fastest
	bcf		pressure_refresh
    call    I2C_init_compass
    call    I2C_init_accelerometer
	clrf	ext_flash_address+0
	clrf	ext_flash_address+1
	clrf	ext_flash_address+2

	clrf	timeout_counter2
	clrf 	timeout_counter3
	bcf		premenu						; clear premenu flag
	bcf		menubit						; clear menu flag
	clrf	last_pressure+0
	clrf	last_pressure+1
    bcf     is_bailout                  ; =1: Bailout
    bcf     ccr_diluent_setup           ; Use OC gases for gaslist routine

	bcf		simulatormode_active		; Quit simulator mode (if active)
	bcf		switch_left
	bcf		switch_right
    bcf     LEDg
    bcf     LEDr

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
	WIN_INVERT	.0					    ; Init new Wordprocessor

;	call	TFT_user_image				; Show the user image/text
	
    WIN_COLOR	color_lightblue
	WIN_SMALL	menu_pos_column,menu_pos_row
    STRCPY_TEXT_PRINT  tMenu		;"<Menu"
	WIN_SMALL	view_column,view_row
    STRCPY_TEXT_PRINT  tView        ;"View>"
	call    TFT_standard_color

    call    mcp_reset                   ; Setup RX
	call	TFT_clock					; display time
    call    update_surfloop60
	call	get_battery_voltage			; get battery voltage
	call	TFT_update_batt_voltage		; display battery voltage
	call	TFT_update_surf_press		; display surface pressure
	call	TFT_temp_surfmode			; Displays temperature
	call	TFT_display_decotype_surface
    extern  surf_customview_mask
    call    surf_customview_mask        ; Update #menupos3 view

    WIN_TOP     .0
    WIN_LEFT    .70
    movlw   LOW(ostc3_logo_block)
    movwf   TBLPTRL
    movlw   HIGH ostc3_logo_block;&0xFFFF
    movwf   TBLPTRH
    movlw   UPPER(ostc3_logo_block)
    movwf   TBLPTRU
    call    color_image

	btfsc	FLAG_apnoe_mode				; Ignore in Apnoe mode
	bra		surfloop1
	btfsc	FLAG_gauge_mode				; Ignore in Gauge mode
	bra		surfloop1

surfloop1:
    call    TFT_Display_FadeIn          ; Display resulting surface screen.

    ;---- Late initialisations -----------------------------------------------    
	movff	last_surfpressure_30min+0,int_I_pres_respiration+0		; copy surface air pressure to deco routine
	movff	last_surfpressure_30min+1,int_I_pres_respiration+1		; 30min old values 
	movff	last_surfpressure_30min+0,int_I_pres_surface+0			; copy surface air pressure to deco routine
	movff	last_surfpressure_30min+1,int_I_pres_surface+1			; 30min old values 
	movff	last_surfpressure_30min+0,last_surfpressure+0			; Use 30min old airpressure 
	movff	last_surfpressure_30min+1,last_surfpressure+1			; Use 30min old airpressure

    extern  do_demo_divemode
;    goto    do_demo_divemode

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
	call	timeout_premenu				; timeout premenu
	call	set_dive_modes				; tests if depth>threshold
    btfss   secs,0                      ; Every two seconds...
	call	TFT_temp_surfmode			; Displays temperature
    btfss   secs,0                      ; Every two seconds...
    call    surfmode_check_for_warnings ; ... check for warnings (and display/update) them

    btfsc   FLAG_ccr_mode               ; In CCR mode...
    call    TFT_surface_hud             ; ...update HUD data in surface mode

	bcf		onesecupdate				; every second tasks done
	
surfloop_loop2:	
; Tasks approx. every 50ms for all modes
	call	test_switches_surfmode		; check switches
	call	speed_normal

; One minute tasks for all modes
	btfsc	oneminupdate				; do every minute tasks
	call	update_surfloop60			; yes, e.g. update time and date

; Mode tasks
	btfsc	menubit						; Menu?
	goto	do_main_menu				; Menu!

	btfsc	pressure_refresh			; new pressure available?
	call	TFT_update_surf_press		; display surface pressure
	bcf		pressure_refresh			; until new pressure is available

    btfss   quarter_second_update
    bra     surfloop_loop2a
    bcf     quarter_second_update
    movlw   .6
    cpfseq  menupos3                    ; in compass view?
    bra     surfloop_loop2a             ; No
    call    TFT_surface_compass_heading ; Yes, update compass heading value

surfloop_loop2a:
	btfsc	toggle_customview			; Next view?
	call	surf_customview_toggle      ; Yes, show next customview (and delete this flag)

    btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    bra     surfloop_loop3
    btfsc   vusb_in                     ; USB plugged in?
    goto    comm_mode                   ; Start COMM mode
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
	btfsc	divemode					; Divemode active?
	goto	diveloop					; Yes, switch into Divemode!
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
    call    surfmode_check_for_nofly            ; Check if nofly time should be shown
    call    surfmode_check_for_desat            ; Check if desat time should be shown
    call    surfmode_check_for_interval         ; Check if surface interval should be shown

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
    iorwf   	surface_interval+0,W
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

timeout_premenu:
	btfsc	premenu					; is "<Menu" displayed?
	bra		timeout_premenu1		; Yes
	return

timeout_premenu1:
	incf	timeout_counter3,F		; Yes...
	movlw	d'2'
	cpfsgt	timeout_counter3		; ... longer then premenu_timeout
	return							; No!

	bcf		premenu					; Yes, so clear "Menu?" and clear pre_menu bit

	WIN_SMALL	menu_pos_column,menu_pos_row
    WIN_COLOR	color_lightblue
    STRCPY_TEXT_PRINT  tMenu		; "<Menu"
	call	TFT_standard_color
	clrf	timeout_counter3		; Also clear timeout
	bcf		switch_left				; and debounce switches
	bcf		switch_right
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
	btfss	premenu
	bra		test_switches_surfmode4
	bsf		menubit					; Enter Menu!
	return

test_switches_surfmode4:
    WIN_COLOR	color_lightblue
	WIN_SMALL	view_column,view_row
    STRCPY_TEXT_PRINT  tView        ;"View"
	WIN_SMALL	menu_pos_column,menu_pos_row
	call	TFT_standard_color
	WIN_INVERT	.1					; Init new Wordprocessor
	STRCPY_TEXT_PRINT  tMenu		;"<Menu"
	WIN_INVERT	.0					; Init new Wordprocessor
	bsf		premenu
	clrf	timeout_counter2
	return

test_switches_surfmode2:
	bcf		switch_right
	bsf		toggle_customview
    clrf	timeout_counter2        ; and reset timeout
	return

test_switches_surfmode5:
	WIN_SMALL	menu_pos_column,menu_pos_row
    WIN_COLOR	color_lightblue
	STRCPY_TEXT_PRINT  tMenu		;"<Menu"
	call	TFT_standard_color
    bcf		premenu
	clrf	timeout_counter2
	return

	global	timeout_surfmode
timeout_surfmode:
	movlw	timeout_surfacemode		; [s]
	global	timeout_testmode
timeout_testmode:
	incf	timeout_counter2,F		; increase timeout counter
	cpfsgt	timeout_counter2		; Compare with timeout_counter2
	return							; return, no timeout
	bsf		sleepmode				; Set Flag
	return							; Return

 END