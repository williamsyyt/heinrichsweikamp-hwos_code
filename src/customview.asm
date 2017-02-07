;=============================================================================
;
;   File customview.asm
;
;   Customview in Surfacemode and Divemode
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-10 : [mH] moving from OSTC code

;=============================================================================

#include    "hwos.inc"                  ; Mandatory header
#include	"tft_outputs.inc"
#include 	"strings.inc"
#include 	"tft.inc"
#include	"isr.inc"
#include	"wait.inc"
#include	"surfmode.inc"
#include	"convert.inc"
#include	"divemode.inc"
#include    "i2c.inc"

gui     CODE

;=============================================================================
; Do every-second tasks for the custom view area (Dive mode)

	global	customview_second
customview_second:
	movff	menupos3,WREG               ; copy current view (1-...)
	dcfsnz	WREG,F
	goto    TFT_update_ppo2_sensors         ; Update Sensor data ; and return
	dcfsnz	WREG,F
	goto    TFT_update_avr_stopwatch        ; Update average depth and stopwatch; and return
	dcfsnz	WREG,F
	goto    TFT_decoplan                    ; Show decoplan ; and return
	dcfsnz	WREG,F
	goto    TFT_battinfo_tissues_clock       ; Update Battery, Tissues and clock ; and return
	dcfsnz	WREG,F
	goto    TFT_gf_info                     ; Update GF informations ; and return
	dcfsnz	WREG,F
        return  ; Compass updated seperately (Faster) in divemode;
	dcfsnz	WREG,F
	goto    TFT_dyn_gaslist                 ; Update the gaslist ; and return
	dcfsnz	WREG,F
	goto    TFT_hud_voltages                ; Show HUD details	; and return
	dcfsnz	WREG,F
	bra	customview_1sec_view9           ; Make sure to change value in "check_ppo2_display:" when moving around custom views
	dcfsnz	WREG,F
	goto    TFT_sensor_check                ; Show ppO2 of O2 and Diluent ; and return
	dcfsnz	WREG,F
	goto	TFT_ppo2_ead_end_cns            ; Show ppO2, END/EAD and CNS; and return
	dcfsnz	WREG,F
	goto	TFT_pscr_info			; Show ppO2, drop and lung ratio; and return

	; Menupos3=0, do nothing
	return

customview_1sec_view9:                      ; Ceiling
    call    TFT_ceiling                     ; Show Ceiling
    ; ppO2 value
    call    TFT_display_ppo2_val
    ; current GF value
    extern  char_I_deco_model
    TSTOSS  char_I_deco_model               ; 0 = ZH-L16, 1 = ZH-L16-GF
    return                                  ; No GF info for non-GF modes
    goto    TFT_gf_info                     ; Update GF informations ; and return



;=============================================================================
; Do every-minute tasks for the custom view area

	global	customview_minute
customview_minute:
    return
;	movff	menupos3,WREG               ; copy
;	dcfsnz	WREG,F
;	bra		customview_1min_view1
;	dcfsnz	WREG,F
;	bra		customview_1min_view2
;	dcfsnz	WREG,F
;	bra		customview_1min_view3
;	dcfsnz	WREG,F
;	bra		customview_1min_view4
;	dcfsnz	WREG,F
;	bra		customview_1min_view5
;	dcfsnz	WREG,F
;	bra		customview_1min_view6
;	dcfsnz	WREG,F
;	bra		customview_1min_view7
;	dcfsnz	WREG,F
;	bra		customview_1min_view8
;	dcfsnz	WREG,F
;	bra		customview_1min_view9
;	dcfsnz	WREG,F
;	bra		customview_1min_view10
;	dcfsnz	WREG,F
;	bra		customview_1min_view11
;	; Menupos3=0, do nothing
;	return
;
;customview_1min_view1:
;customview_1min_view2:
;customview_1min_view3:
;customview_1min_view4:
;customview_1min_view5:
;customview_1min_view6:
;customview_1min_view7:
;customview_1min_view8:
;customview_1min_view9:
;customview_1min_view10:
;customview_1min_view11:
;	return

    global  surf_customview_toggle
surf_customview_toggle:
	bcf		switch_right
	incf	menupos3,F			            ; Number of customview to show
	movlw	d'7'							; Max number of customsviews in surface mode
	cpfsgt	menupos3			            ; Max reached?
	bra		surf_customview_mask		    ; No, show
    movlw   .1
	movwf   menupos3			            ; Reset to one (Always one custom view visible)

    global  surf_customview_mask
surf_customview_mask:
    WIN_BOX_BLACK    .50,surf_warning1_row-1, .0, surf_decotype_column-.1	; top, bottom, left, right
    ; Prepare title
    WIN_TINY    surf_customview_title_column,surf_customview_title_row
    WIN_COLOR   color_greenish
	movff	menupos3,WREG                   ; Menupos3 holds number of customview function
	dcfsnz	WREG,F
	bra		surf_customview_init_view1      ; OC Gas list
	dcfsnz	WREG,F
	bra		surf_customview_init_view2      ; CC Dil list
	dcfsnz	WREG,F
	bra		surf_customview_init_view3      ; CC SP list
	dcfsnz	WREG,F
	bra		surf_customview_init_view4      ; Custom Text
	dcfsnz	WREG,F
	bra		surf_customview_init_view5      ; Tissue Diagram
    dcfsnz	WREG,F
	bra		surf_customview_init_view6      ; Compass
    dcfsnz	WREG,F
	bra		surf_customview_init_view7      ; Deco settings

    call    I2C_sleep_accelerometer         ; Stop accelerometer
    call    I2C_sleep_compass               ; Stop compass

    movlw   .1
	movwf   menupos3			            ; Reset to one (Always one custom view visible)

surf_customview_init_view1:                 ; View1: OC Gas list
    btfsc   FLAG_gauge_mode
    bra     surf_customview_toggle
    btfsc   FLAG_apnoe_mode
    bra     surf_customview_toggle
    btfsc   FLAG_ccr_mode
    bra     surf_customview_init_view1_bail ; Bailoutversion
    STRCPY_TEXT_PRINT tGaslist              ; Title of customview
    call    TFT_standard_color
    call	TFT_gaslist_surfmode            ; Show gas list
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view1_bail:
    STRCPY_TEXT_PRINT tDiveBailout          ; Title of customview
    call    TFT_standard_color
    call	TFT_gaslist_surfmode            ; Show gas list
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view2:                 ; View2: CC Dil list
    btfss   FLAG_ccr_mode
    bra     surf_customview_toggle
    btfsc   FLAG_gauge_mode
    bra     surf_customview_toggle
    btfsc   FLAG_apnoe_mode
    bra     surf_customview_toggle
    STRCPY_TEXT_PRINT tGaslistCC            ; Title of customview
    call    TFT_standard_color
    call	TFT_dillist_surfmode            ; Show diluent list
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view3:                 ; View3: CC SP list
    btfss   FLAG_ccr_mode
    bra     surf_customview_toggle
    btfsc   FLAG_gauge_mode
    bra     surf_customview_toggle
    btfsc   FLAG_apnoe_mode
    bra     surf_customview_toggle
    STRCPY_TEXT_PRINT tFixedSetpoints       ; Title of customview
    call    TFT_standard_color
    call	TFT_splist_surfmode             ; Show Setpoint list
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view4:                 ; View4: Custom text
    call    TFT_standard_color
    call	TFT_custom_text                 ; Show the custom text
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view5:                 ; View5: Tissue Diagram
    btfsc   FLAG_gauge_mode
    bra     surf_customview_toggle
    btfsc   FLAG_apnoe_mode
    bra     surf_customview_toggle
    call    TFT_standard_color
    call	TFT_surface_tissues             ; Show Tissue diagram
    bra		customview_toggle_exit          ; Done.

surf_customview_init_view6:                 ; View6: Compass
    call    I2C_init_accelerometer          ; Start accelerometer
    call    I2C_init_compass                ; Start compass
    extern  TFT_surface_compass_mask
    call	TFT_surface_compass_mask        ; Show compass
    bra		customview_toggle_exit          ; Done.
surf_customview_init_view7:
    btfsc   FLAG_gauge_mode
    bra     surf_customview_toggle
    btfsc   FLAG_apnoe_mode
    bra     surf_customview_toggle
    extern  deco_setup_dive
    call    deco_setup_dive
    call    TFT_surface_decosettings        ; Show all deco settings
    bra		customview_toggle_exit          ; Done.

    global  menuview_toggle
menuview_toggle:            ; Show Menu or the simulator tasks
    movlw   divemode_menuview_timeout
    movwf   timeout_counter3
    bsf     menuview
	bcf		switch_left
	incf	menupos2,F			            ; Number of options to show
	movlw	d'9'							; Max number of options in divemode
	cpfsgt	menupos2			            ; Max reached?
	bra		menuview_mask		            ; No, show
    global  menuview_toggle_reset
menuview_toggle_reset:                      ; Timeout occured
	clrf	menupos2
    bcf     menuview
menuview_mask:
    WIN_BOX_BLACK   dm_simtext_row, dm_simtext_row+.23, dm_simtext_column, dm_simtext_column+.49 ; top, bottom, left, right
    btfss   FLAG_gauge_mode
    bra     menuview_mask2
    ; Clear some more in gauge mode  -- "Reset Avg." text is longer than normal simtext
    WIN_BOX_BLACK   dm_simtext_row, dm_simtext_row+.23, dm_simtext_column+.50, dm_simtext_column+.70  ; top, bottom, left, right
menuview_mask2:
    call    TFT_draw_gassep_line
    movlw   color_yellow
    call	TFT_set_color
    bsf     win_invert                  ; Set invert flag
    WIN_SMALL   dm_simtext_column,dm_simtext_row
	movff	menupos2,WREG                   ; Menupos2 holds number of menu option to show
	dcfsnz	WREG,F
	bra		menuview_view_gaschange         ; If a better gas is indicated
	dcfsnz	WREG,F
	bra		menuview_view1                  ; "Menu?" (Not in Gauge and Anpnoe)
	dcfsnz	WREG,F
	bra		menuview_view2                  ; "Quit Simulation?" (Sim only)
	dcfsnz	WREG,F
	bra		menuview_view3                  ; "Descent 1m" (Sim only)
	dcfsnz	WREG,F
	bra		menuview_view4                  ; "Ascend 1m" (Sim only)
	dcfsnz	WREG,F
	bra		menuview_view5                  ; "Quit Apnea mode?" (Apnea only)
	dcfsnz	WREG,F
	bra		menuview_view6                  ; "Reset Avr." (Gauge only)
	dcfsnz	WREG,F
	bra		menuview_view7                  ; "Sim:+5mins"  (Sim only)
	dcfsnz	WREG,F
	bra		menuview_view8                  ; "Heading"  (When compass is shown)
menuview_exit:
    call	TFT_standard_color
    bcf     win_invert              ; Reset invert flag
	return                                  ; Menupos2 = 0, Show nothing


menuview_view_gaschange:
    extern  gaslist_strcat_gas_mod
    btfss	better_gas_available            ; =1: A better gas is available
	bra		menuview_toggle 				; No, call next option
    btfsc   divemode_gaschange              ; Skip if the last gas change is not done yet.
    bra		menuview_toggle
    bsf     short_gas_decriptions           ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    movff   better_gas_number,PRODL     	; number (1-5) of the "better gas" in divemode, =0: no better gas available
    decf    PRODL,F
    call    gaslist_strcat_gas_mod          ; Append gas description of gas #PRODL (0-4) to current string
    movlw   .5
    movwf   FSR2L                           ; Point to char 6 (5 chars gas description only)
    STRCAT_PRINT "?"
    bra     menuview_exit                   ; Done.
menuview_view1:
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		menuview_toggle 				; Call next option
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		menuview_toggle 				; Call next option
	STRCPY_TEXT_PRINT tDivePreMenu			; "Menu?"
    bra     menuview_exit                   ; Done.
menuview_view2:
    btfss  	simulatormode_active			; View only for simulator mode
	bra		menuview_toggle 				; Call next option
	STRCPY_TEXT_PRINT tQuitSim				; "Quit Simulation?"
    bra     menuview_exit                   ; Done.
menuview_view3:
    btfss  	simulatormode_active			; View only for simulator mode
	bra		menuview_toggle 				; Call next option
	STRCPY_PRINT "Sim:-1m"
    bra     menuview_exit                   ; Done.
menuview_view4:
    btfss  	simulatormode_active			; View only for simulator mode
	bra		menuview_toggle 				; Call next option
	STRCPY_PRINT "Sim:+1m"
    bra     menuview_exit                   ; Done.
menuview_view5:
	btfss	FLAG_apnoe_mode					; In Apnoe mode?
	bra		menuview_toggle 				; No, call next option
	btfss	FLAG_active_descent				; Are we descending?
	bra		menuview_toggle 				; Yes
; We are at the surface:
	STRCPY_TEXT_PRINT	tQuitSim			; "Quit Apnea mode?"
	bra     menuview_exit                   ; Done.
menuview_view6:
	btfss	FLAG_gauge_mode					; In Gauge mode?
	bra		menuview_toggle 				; No, call next option
	STRCPY_TEXT_PRINT	tDivemenu_ResetAvg  ; "Reset Avg."
	bra     menuview_exit                   ; Done.
menuview_view7:
    btfss  	simulatormode_active			; View only for simulator mode
	bra		menuview_toggle 				; Call next option
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		menuview_toggle 				; Yes, call next option
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		menuview_toggle 				; Yes, call next option
	STRCPY_PRINT	"Sim:+5'"
	bra     menuview_exit                   ; Done.
menuview_view8:
    movlw   .6
    cpfseq  menupos3                        ; in compass view?
	bra		menuview_toggle 				; No, call next option
	STRCPY_TEXT_PRINT	tSetHeading         ; "Heading"
	bra     menuview_exit                   ; Done.


;=============================================================================
; Show next customview (and delete this flag)
	global	customview_toggle
customview_toggle:
	bcf		switch_right
	incf	menupos3,F			            ; Number of customview to show
	movlw	d'12'							; Max number of customsviews in divemode
	cpfsgt	menupos3			            ; Max reached?
	bra		customview_mask		            ; No, show
customview_toggle_reset:					; Timeout occured
	clrf	menupos3			            ; Reset to zero (Zero=no custom view)
    global  customview_mask
customview_mask:	
    ; Clear custom view area in divemode
    WIN_BOX_BLACK    dm_customview_row, dm_customview_bot, dm_customview_column, dm_customview_rgt	; top, bottom, left, right
	WIN_SMALL	dm_customview_column,dm_customview_row
	call	TFT_standard_color
	movff	menupos3,WREG                   ; Menupos3 holds number of customview function
	dcfsnz	WREG,F
	bra		customview_init_view1		; Update Sensor data
	dcfsnz	WREG,F
	bra		customview_init_view2		; average depth and stopwatch
	dcfsnz	WREG,F
	bra		customview_init_view3		; Decoplan
	dcfsnz	WREG,F
	bra		customview_init_view4		; Battery info, Tissues and clock
	dcfsnz	WREG,F
	bra		customview_init_view5           ; GF informations
	dcfsnz	WREG,F
	bra		customview_init_view6           ; Compass
	dcfsnz	WREG,F
	bra		customview_init_view7           ; Dynamic gaslist
	dcfsnz	WREG,F
	bra		customview_init_view8           ; HUD voltages
	dcfsnz	WREG,F
	bra		customview_init_view9           ; ppO2, Ceiling and current GF
	dcfsnz	WREG,F
	bra		customview_init_view10          ; Sensor check
	dcfsnz	WREG,F
	bra		customview_init_view11          ; ppO2, END/EAD and CNS
	dcfsnz	WREG,F
	bra		customview_init_view12          ; PSCR Info

customview_init_nocustomview:
    call    I2C_sleep_accelerometer         ; Stop accelerometer
    call    I2C_sleep_compass               ; Stop compass
	bra		customview_toggle_exit	

customview_init_view1:
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; yes, Call next view...
	btfss	FLAG_ccr_mode					; In CC mode?
	bra		customview_toggle				; no, Call next view...
    movf    hardware_flag,W
    sublw   0x11        ; 2 with BLE
    btfsc   STATUS,Z
    bra		customview_toggle				; no, Call next view...
    movf    hardware_flag,W
    sublw   0x13        ; +
    btfsc   STATUS,Z
    bra		customview_toggle				; no, Call next view...

    bsf     dive_hud1_displayed         ; Set display flag
    bsf     dive_hud2_displayed         ; Set display flag
    bsf     dive_hud3_displayed         ; Set display flag
    call    TFT_hud_mask                ; Setup HUD mask
    call    TFT_update_ppo2_sensors     ; Update Sensor data
	bra		customview_toggle_exit	

customview_init_view2:
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; Yes, Call next view...
    call    TFT_mask_avr_stopwatch     ; Show mask for average depth and stopwatch
    call    TFT_update_avr_stopwatch   ; Update average depth and stopwatch
    bra		customview_toggle_exit

customview_init_view3:
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; Yes, Call next view...
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		customview_toggle				; Yes, Call next view...
    call    TFT_decoplan                    ; Show decoplan
    bra		customview_toggle_exit

customview_init_view4:
    call    TFT_battinfo_tissues_clock_mask  ; Setup Mask
    call    TFT_battinfo_tissues_clock       ; Show Battery info, Tissues and clock
    bra		customview_toggle_exit

customview_init_view5:
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; Yes, Call next view...
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		customview_toggle				; Yes, Call next view...

    extern  char_I_deco_model
    TSTOSS  char_I_deco_model               ; 0 = ZH-L16, 1 = ZH-L16-GF
    bra		customview_toggle				; No GF info for non-GF modes

    call    TFT_gf_mask                     ; Setup Mask
    call    TFT_gf_info                     ; Show GF informations
    bra		customview_toggle_exit

customview_init_view6:                      ; Compass (View 6)
    call    I2C_init_accelerometer          ; Start accelerometer
    call    I2C_init_compass                ; Start compass
    extern  TFT_dive_compass_mask
    call	TFT_dive_compass_mask           ; Show compass
    bra		customview_toggle_exit

customview_init_view7:                      ; Dynamic gaslist (View 7)
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; Yes, Call next view...
	btfsc	FLAG_ccr_mode					; In CC mode?
	bra		customview_toggle				; Yes, Call next view...
    call    TFT_dyn_gaslist                 ; Show the dyn gaslist
    bra		customview_toggle_exit

customview_init_view8:                      ; Sensor millivolts
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		customview_toggle				; Yes, Call next view...
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; yes, Call next view...
	btfss	FLAG_ccr_mode					; In CC mode?
	bra		customview_toggle				; no, Call next view...
    
    movf    hardware_flag,W
    sublw   0x13        ; +
    btfsc   STATUS,Z
    bra		customview_toggle				; no, Call next view...

    movff   opt_ccr_mode,WREG               ; =0: Fixed SP, =1: Sensor,  =2: Auto SP
    sublw   .1                              ; opt_ccr_mode = 1 (Sensor)?
    bnz     customview_toggle				; no, Call next view...
    call    TFT_hud_mask                    ; Setup HUD mask
    call    TFT_hud_voltages                ; Show HUD details
    bra		customview_toggle_exit

customview_init_view9:                      ; ppO2, Ceiling and current GF
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		customview_toggle				; yes, Call next view...
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		customview_toggle				; Yes, Call next view...
    call    TFT_ceiling_mask                ; Setup mask
    call    TFT_ceiling                     ; Show Ceiling

    ; ppO2 value
    call    TFT_mask_ppo2
    call    TFT_display_ppo2_val

    ; current GF value
    extern  char_I_deco_model
    TSTOSS  char_I_deco_model               ; 0 = ZH-L16, 1 = ZH-L16-GF
    bra     customview_toggle_exit          ; No GF info for non-GF modes
    call    TFT_gf_mask_cGF                 ; Setup Mask - current GF only
    call    TFT_gf_info                     ; Show GF informations
    bra		customview_toggle_exit

customview_init_view10:                     ; Sensor check
    btfsc   FLAG_apnoe_mode		    ; In Apnoe mode?
    bra	    customview_toggle		    ; yes, Call next view...
    btfsc   FLAG_gauge_mode		    ; In Gauge mode?
    bra	    customview_toggle		    ; Yes, Call next view...
    btfss   FLAG_ccr_mode		    ; In CC mode?
    bra	    customview_toggle		    ; no, Call next view...

    call    TFT_sensor_check_mask           ; Show ppO2 of O2 and Diluent mask
    call    TFT_sensor_check                ; Show ppO2 of O2 and Diluent
    bra		customview_toggle_exit

customview_init_view11:                     ; ppO2, END/EAD and CNS
    btfsc   FLAG_apnoe_mode		    ; In Apnoe mode?
    bra	    customview_toggle		    ; yes, Call next view...
    btfsc   FLAG_gauge_mode		    ; In Gauge mode?
    bra	    customview_toggle		    ; Yes, Call next view...

    call    TFT_ppo2_ead_end_cns_mask       ; Show ppO2, END/EAD and CNS mask
    call    TFT_ppo2_ead_end_cns            ; Show ppO2, END/EAD and CNS
    bra		customview_toggle_exit

customview_init_view12:		            ; PSCR Info    
    btfss   FLAG_pscr_mode		    ; In PSCR mode?
    bra	    customview_toggle		    ; No, Call next view...
    
    call    TFT_pscr_info_mask		    ; Show ppO2, drop and lung ratio
    call    TFT_pscr_info		    ; Show ppO2, drop and lung ratio
    bra		customview_toggle_exit
    
customview_toggle_exit:
	call	TFT_standard_color
	bcf		toggle_customview			; Clear flag
	return

	global 	customview_show_change_depth
customview_show_change_depth:       ; Put " lom" or " loft" into Postinc2
    PUTC    " "
    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		customview_show_mix_metric
    movf    lo,W
    mullw   .100                    ; convert meters to mbar
    movff   PRODL,lo
    movff   PRODH,hi
    call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
    bsf     leftbind
    output_16						; Change depth in lo:hi
    bcf     leftbind
    STRCAT_TEXT		tFeets
    return
customview_show_mix_metric:
    output_99						; Change depth in lo
    STRCAT_TEXT		tMeters
    return


	global 	customview_show_mix
customview_show_mix:                ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
    tstfsz  hi                      ; He=0?
    bra     customview_show_mix5    ; No, Show a TX
	movlw	.21
	cpfseq	lo								; Air?
	bra		customview_show_mix2	; No
	STRCAT_TEXT		tSelectAir				; Yes, show "Air"
    bra     customview_show_mix4b
customview_show_mix2:
	movlw	.100
	cpfseq	lo								; O2?
	bra		customview_show_mix3	; No
	STRCAT_TEXT		tSelectO2				; Yes, show "O2"
    bra     customview_show_mix4b

customview_show_mix3:
	movlw	.21
	cpfslt	lo								; < Nx21?
	bra		customview_show_mix4    ; No
	STRCAT_TEXT		tGasErr        	; Yes, show "Err"
	output_99						; O2 ratio is still in "lo"
    bra     customview_show_mix4c

customview_show_mix4:
	STRCAT_TEXT		tSelectNx		; Show "Nx"
	output_99						; O2 ratio is still in "lo"
customview_show_mix4b:
    STRCAT  " "
customview_show_mix4c:
    btfsc   divemode                ; In divemode
	return                          ; Yes
    STRCAT  "  "
    return

customview_show_mix5:
    btfsc   divemode
    bra     customview_show_mix6
    STRCAT_TEXT		tSelectTx   	; Show "Tx"
customview_show_mix6:
    output_99						; O2 ratio is still in "lo"
    PUTC    "/"
    movff   hi,lo
    output_99						; He ratio
    return

	END