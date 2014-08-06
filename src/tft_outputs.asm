;=============================================================================
;
;   File tft_outputs.asm
;
;   Startup subroutines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-07 : [mH] moving from OSTC code

#include    "ostc3.inc"                  ; Mandatory header
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include 	"tft.inc"
#include 	"wait.inc"
#include 	"strings.inc"
#include	"convert.inc"
#include 	"varargs.inc"
#include	"math.inc"
#include	"isr.inc"
#include	"eeprom_rs232.inc"
#include	"adc_lightsensor.inc"
#include	"surfmode.inc"
#include	"divemode.inc"
#include    "external_flash.inc"
#include	"ghostwriter.inc"
#include    "customview.inc"
#include    "i2c.inc"
#include    "colorschemes.inc"

	extern	aa_wordprocessor

;=============================================================================

gui    CODE
;=============================================================================

    global   TFT_divemask_color
TFT_divemask_color:
    movlw   color_green
    btfsc   divemode            ; in Divemode?
    rcall   TFT_divemask_color_dive
	bra		TFT_standard_color0

TFT_divemask_color_dive:
    movff   opt_dive_color_scheme,WREG  ; 0-3
    incf    WREG
	dcfsnz	WREG
	bra		TFT_divemask_colordive0  	;0
	dcfsnz	WREG
	bra		TFT_divemask_colordive1  	;1
	dcfsnz	WREG
	bra		TFT_divemask_colordive2  	;2
	dcfsnz	WREG
	bra		TFT_divemask_colordive3  	;3
TFT_divemask_colordive0:
    movlw   color_scheme_divemode_mask1
    return
TFT_divemask_colordive1:
    movlw   color_scheme_divemode_mask2
    return
TFT_divemask_colordive2:
    movlw   color_scheme_divemode_mask3
    return
TFT_divemask_colordive3:
    movlw   color_scheme_divemode_mask4
    return


    global  TFT_attention_color
TFT_attention_color:
    movlw   color_yellow
	bra		TFT_standard_color0

    global  TFT_warnings_color
TFT_warnings_color:
    movlw   color_red           ; TODO
	bra		TFT_standard_color0

    global  TFT_disabled_color
TFT_disabled_color:
    movlw   color_grey          ; Default to OSTC grey (dark blue)
    btfsc   divemode            ; in Divemode?
    rcall   TFT_disabled_color_dive
    bra		TFT_standard_color0
TFT_disabled_color_dive:
    movff   opt_dive_color_scheme,WREG  ; 0-3
    incf    WREG
	dcfsnz	WREG
	bra		TFT_disabled_colordive0  	;0
	dcfsnz	WREG
	bra		TFT_disabled_colordive1  	;1
	dcfsnz	WREG
	bra		TFT_disabled_colordive2  	;2
	dcfsnz	WREG
	bra		TFT_disabled_colordive3  	;3
TFT_disabled_colordive0:
    movlw   color_scheme_divemode_dis1
    return
TFT_disabled_colordive1:
    movlw   color_scheme_divemode_dis2
    return
TFT_disabled_colordive2:
    movlw   color_scheme_divemode_dis3
    return
TFT_disabled_colordive3:
    movlw   color_scheme_divemode_dis4
    return

    global  TFT_standard_color
TFT_standard_color:
    setf    WREG                ; Default white
    btfsc   divemode            ; in Divemode?
    rcall   TFT_standard_color_dive
TFT_standard_color0:
	call	TFT_set_color
	return
TFT_standard_color_dive:
    movff   opt_dive_color_scheme,WREG  ; 0-3
    incf    WREG
	dcfsnz	WREG
	bra		TFT_standard_colordive0  	;0
	dcfsnz	WREG
	bra		TFT_standard_colordive1  	;1
	dcfsnz	WREG
	bra		TFT_standard_colordive2  	;2
	dcfsnz	WREG
	bra		TFT_standard_colordive3  	;3
TFT_standard_colordive0:
    movlw   color_scheme_divemode_std1
    return
TFT_standard_colordive1:
    movlw   color_scheme_divemode_std2
    return
TFT_standard_colordive2:
    movlw   color_scheme_divemode_std3
    return
TFT_standard_colordive3:
    movlw   color_scheme_divemode_std4
    return

TFT_color_code macro color_code_temp
	movlw	color_code_temp
	call	TFT_color_code1
	endm
	
	global	TFT_color_code1
TFT_color_code1:				; Color-codes the output, if required
	dcfsnz	WREG
	bra		TFT_color_code_depth		; depth_warn_mbar [mbar], 16Bit
	dcfsnz	WREG
	bra		TFT_color_code_cns			; color_code_cns_high [%]
	dcfsnz	WREG
	bra		TFT_color_code_gf			; color_code_gf_warn_high [%]
	dcfsnz	WREG
	bra		TFT_color_code_ppo2         ; Color-code the OC ppO2 results [cbar], opt_ppO2_max as threshold
	dcfsnz	WREG
	bra		TFT_color_code_velocity     ; color_code_velocity_warn_high [m/min]
	dcfsnz	WREG
	bra		TFT_color_code_ceiling		; Show warning if current depth>shown ceiling
	dcfsnz	WREG
	bra		TFT_color_code_gaslist		; Color-code current row in Gaslist (%O2 in hi), opt_ppO2_max as threshold
    dcfsnz	WREG
    bra     TFT_color_code_ppo2_hud     ; Color-code the hud ppO2 readings [cbar], opt_ppO2_max as threshold
    dcfsnz	WREG
    bra     TFT_color_code_battery      ; Color-code the battery display

TFT_color_code_gaslist:				; %O2 in hi
; Check very high ppO2 manually
    SAFE_2BYTE_COPY amb_pressure,xA
	movlw		d'10'
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; xC=p_amb/10
	movff		xC+0,xA+0
	movff		xC+1,xA+1
	movff		hi,xB+0
	clrf		xB+1
	call		mult16x16                   ; lo * p_amb/10
; Check if ppO2>6,55bar
	tstfsz		xC+2						; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra			TFT_color_code_warn     	; Yes, warn in warning color
; Check if ppO2>3,30bar
	btfsc		xC+1,7
	bra			TFT_color_code_warn         ; Yes, warn in warning color

; Check for low ppo2
	movff       xC+0,sub_a+0
	movff       xC+1,sub_a+1
    movff       opt_ppO2_min,WREG
	mullw       d'100'                  ; opt_ppO2_min*100
	movff       PRODL,sub_b+0
	movff       PRODH,sub_b+1
	call        subU16
	btfsc       neg_flag
    bra			TFT_color_code_warn     ; too low -> Warning Color!

; Check for high ppo2
	movff		opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
	mullw		d'100'					; opt_ppO2_max*100
	movff		PRODL,sub_b+0
	movff		PRODH,sub_b+1
	call		subU16					;  sub_c = sub_a - sub_b	
	btfss		neg_flag
	bra			TFT_color_code_warn     ; too high -> Warning Color!
	return

TFT_color_code_warn:
	call		TFT_warnings_color
	return

TFT_color_code_ceiling:
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
	movff	hi,xA+1
	movff	lo,xA+0
	movff	char_O_first_deco_depth,lo  ; Ceiling in m
	decf	lo,F	                    ; -1
	movlw	LOW		d'100'
	movwf	xB+0
	clrf	xB+1						; Devide/100 -> xC+0 = Depth in m
	call	div16x16					; xA/xB=xC with xA as remainder 	
	movf	xC+0,W						; Depth in m
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_color_code_warn     	; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_depth:
	movff	hi,hi_temp
	movff	lo,lo_temp
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
	movff	lo,sub_a+0
	movff	hi,sub_a+1
	movlw	LOW		depth_warn_mbar
	movwf	lo
	movlw	HIGH	depth_warn_mbar
	movwf	hi
	movff	lo,sub_b+0
	movff	hi,sub_b+1
	movff	hi_temp,hi
	movff	lo_temp,lo			; Restore hi, lo
	call	subU16			;  sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		TFT_color_code_warn ; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_cns:
    movff   int_O_CNS_fraction+1,lo		; copy into bank1
    tstfsz  lo                          ; >255% ?
    bra     TFT_color_code_warn         ; Yes
	movff	int_O_CNS_fraction+0,lo
	movlw	color_code_cns_high		; CNS Warn [%]
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_color_code_warn		; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_gf:
	movff	char_O_gradient_factor,lo		; gradient factor
	movlw	color_code_gf_warn_high 	; GF Warn [%]
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_color_code_warn         ; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_ppo2:
; Check if ppO2>6,55bar
	tstfsz	xC+2					; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra		TFT_color_code_warn     ; Yes, warn in warning color
; Check if ppO2>3,30bar
	btfsc	xC+1,7
	bra		TFT_color_code_warn     ; Yes, warn in warning color

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		TFT_color_code_warn     ; Set to warning color

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_min,WREG		; PPO2 min for Sensors and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfsc	neg_flag
	bra		TFT_color_code_warn     ; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_velocity:
	btfss	neg_flag                        ; Ignore for descent!
	bra		TFT_color_code_velocity1		; Skip check!
	movff	divA+0,lo
	movlw	color_code_velocity_warn_high	; Velocity warn [m/min]
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_color_code_warn             ; Set to warning color
TFT_color_code_velocity1:
	call	TFT_standard_color
	return

TFT_color_code_ppo2_hud:            ; With ppO2 [cbar] in lo
	movff	opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
    cpfsgt  lo                      ; lo > opt_ppO2_max?
    bra     TFT_color_code_ppo2_hud1; No
    bra     TFT_color_code_warn     ; Yes
TFT_color_code_ppo2_hud1:
	movff	opt_ppO2_min,WREG		; PPO2 min for Sensors and color coding in divemode
    cpfslt  lo                      ; lo < opt_ppO2_min?
    bra     TFT_color_code_ppo2_hud2; No
    bra     TFT_color_code_warn     ; Yes
TFT_color_code_ppo2_hud2:
    call	TFT_standard_color
    return

TFT_color_code_battery:             ; With battery percent in lo
    movlw   color_code_battery_low
    cpfsgt  lo                      ; lo < color_code_battery_low ?
    bra     TFT_color_code_warn     ; No
    call	TFT_standard_color
    return

; ****************************************************************************


    global  TFT_show_color_schemes
TFT_show_color_schemes:         ; update the color schemes
    bsf     divemode            ; put in divemode
    call    TFT_divemask_color
    WIN_TINY  divemode_mask_depth_column,divemode_mask_depth_row+.40
    STRCAT_TEXT_PRINT	tDepth
    WIN_TINY  divemode_mask_maxdepth_column,divemode_mask_maxdepth_row+.40
    STRCAT_TEXT_PRINT	tMaxDepth
    WIN_TINY  divemode_mask_divetime_column,divemode_mask_divetime_row+.40
    STRCAT_TEXT_PRINT	tDivetime

    ; Show some demo screen

    ; Depth demo
    call	TFT_standard_color
	WIN_MEDIUM	depth_column+.3,depth_row+.40
    movlw   LOW     .5172
    movwf   lo
    movlw   HIGH    .5172
    movwf   hi
	bsf		leftbind
	bsf		ignore_digit4
	output_16						; Full meters in Big font
	bcf		leftbind
	STRCAT_PRINT ""					; Display full meters
    WIN_SMALL	depth_dm_column-.15,max_depth_dm_row+.40
    movlw   LOW     .5172
    movwf   lo
    movlw   HIGH    .5172
    movwf   hi
	PUTC    "."
	movlw	d'4'
	movwf	ignore_digits
	bsf		ignore_digit5
	output_16dp	d'0'                ; .1m in SMALL font
	STRCAT_PRINT ""					; Display decimeters
	WIN_FONT 	FT_SMALL

    ; Max. Depth demo
    WIN_MEDIUM	max_depth_column,max_depth_row+.40
	bsf     ignore_digit4			; no 0.1m
    bsf     leftbind
    movlw   LOW     .6349
    movwf   lo
    movlw   HIGH    .6349
    movwf   hi
	output_16
	STRCAT_PRINT ""					; Display full meters
    bcf     leftbind
	; .1m in SMALL font
	WIN_SMALL	max_depth_dm_column,max_depth_dm_row+.40
	PUTC    "."
	movlw	d'4'
	movwf	ignore_digits
	bsf		ignore_digit5
    bsf     leftbind
    movlw   LOW     .6349
    movwf   lo
    movlw   HIGH    .6349
    movwf   hi
	output_16dp	d'0'
	STRCAT_PRINT ""					; Display decimeters
    bcf     leftbind

    ; Divetime demo
    movff   mins,lo
    clrf    hi
	WIN_MEDIUM	divetime_column, divetime_row+.40
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL  divetime_secs_column, divetime_secs_row+.40   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff   secs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font

    bcf     divemode                ; don't stay in divemode
	return

	global	TFT_divemode_mask
TFT_divemode_mask:					; Displays mask in Dive-Mode
    call    TFT_divemask_color
    WIN_TINY  divemode_mask_depth_column,divemode_mask_depth_row
    STRCAT_TEXT_PRINT	tDepth
    WIN_TINY  divemode_mask_maxdepth_column,divemode_mask_maxdepth_row
    STRCAT_TEXT_PRINT	tMaxDepth
    WIN_TINY  divemode_mask_divetime_column,divemode_mask_divetime_row
    STRCAT_TEXT_PRINT	tDivetime
    
    call	TFT_standard_color
	return

	global	TFT_clear_customview_divemode
TFT_clear_customview_divemode:
    WIN_BOX_BLACK    divemode_customview_row, .163, .0, .159	; top, bottom, left, right
	return

	global	TFT_display_velocity
TFT_display_velocity:						; With divA+0 = m/min
	TFT_color_code	warn_velocity	    	; Color-code Output (With divA+0 = m/min)
	WIN_SMALL	velocity_text_column,velocity_text_row

    TSTOSS  opt_units			; 0=Meters, 1=Feets
	bra		TFT_display_velocity_metric
;TFT_display_velocity_imperial:
	movff	divA+0,WREG						; divA+0 = m/min
	mullw	.100							; PRODL:PRODH = mbar/min
	movff	PRODL,lo
	movff	PRODH,hi
	call	convert_mbar_to_feet			; convert value in lo:hi from mbar to feet
	movlw	'-'
	btfsc	neg_flag
	movlw	'+'
	movwf	POSTINC2
	bsf		leftbind
	output_16
	bcf		leftbind
	STRCAT_TEXT_PRINT  tVelImperial			; Unit switch
	call	TFT_standard_color
    return

TFT_display_velocity_metric:
	movff	divA+0,lo						; divA+0 = m/min
	movlw	'-'
	btfsc	neg_flag
	movlw	'+'
	movwf	POSTINC2
	output_99
	STRCAT_TEXT_PRINT  tVelMetric			; Unit switch
	call	TFT_standard_color
    return

	global	TFT_display_velocity_clear
TFT_display_velocity_clear:
	; Clear Text
	WIN_BOX_BLACK   velocity_text_row, velocity_text_row+.22, velocity_text_column, (velocity_text_column+.7*.8)-1	; top, bottom, left, right
	return

    global  TFT_clear_decoarea
TFT_clear_decoarea:
    WIN_BOX_BLACK   decostop_1st_stop_row, .239, decostop_1st_stop_column ,.159	; top, bottom, left, right
	return

    global  TFT_clear_divemode_menu
TFT_clear_divemode_menu:
    WIN_BOX_BLACK   divemode_menu_row, divemode_menu_lower, divemode_menu_left ,divemode_menu_right	; top, bottom, left, right
	return

	global	TFT_display_ndl_mask
TFT_display_ndl_mask:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	rcall	TFT_clear_decoarea			; Clear Dekostop and Dekosum
    call    TFT_divemask_color
   	WIN_STD 	ndl_text_column,ndl_text_row
	STRCPY_TEXT_PRINT  tNDL             ; NDL
	call	TFT_standard_color
	return

	global	TFT_show_TTS_divemode
TFT_show_TTS_divemode:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	call	TFT_standard_color
	movff	int_O_ascenttime+0,lo       ; TTS
	movff	int_O_ascenttime+1,hi       ; on 16bits
	WIN_MEDIUM  tts_value_column,tts_value_row
	output_16_3					;Displays only 0...999
	STRCAT_PRINT "'"
	return

	global	TFT_display_ndl
TFT_display_ndl:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	WIN_MEDIUM	ndl_value_column,ndl_value_row
	call	TFT_standard_color
	movff	char_O_nullzeit,lo		; Get NDL from C-code
	output_8
	STRCAT_PRINT "'"
	return

	global	TFT_divemode_warning
TFT_divemode_warning:
    bsf     dive_warning_displayed              ; =1: The warning sign is shown
    WIN_TOP  	warning_icon_row
	WIN_LEFT 	warning_icon_column
    TFT_WRITE_PROM_IMAGE dive_warning_block 	; Show Warning icon
;    movlw   .3
;    cpfslt  warning_counter                     ; More then two warnings?
;    rcall   TFT_divemode_warning_counter        ; Yes, show the number
	return

;TFT_divemode_warning_counter:
;    WIN_SMALL	warning_icon_column+.8,warning_icon_row+.13
;    call	TFT_warnings_color
;    movff   warning_counter,lo
;    bsf     leftbind
;	output_8
;    bcf     leftbind
;	STRCAT_PRINT ""
;	call	TFT_standard_color
;	return

	global	TFT_divemode_warning_clear
TFT_divemode_warning_clear:
    btfss   dive_warning_displayed              ; =1: The warning sign is shown
    return
    bcf     dive_warning_displayed              ; clear only once
	WIN_BOX_BLACK   warning_icon_row, warning_icon_row+.38, warning_icon_column, warning_icon_column+.21; top, bottom, left, right
	return

	global	TFT_display_deko_mask
TFT_display_deko_mask:
	rcall		TFT_clear_decoarea
   	WIN_STD 	tts_text_column,tts_text_row
    call    TFT_divemask_color
	STRCPY_TEXT_PRINT  tTTS             ; TTS
	call	TFT_standard_color
    bcf		show_safety_stop            ; Clear safety stop flag
    return

TFT_display_deko_output_depth:		; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	TSTOSS	opt_units				; 0=m, 1=ft
	bra		TFT_display_deko_output_metric
;TFT_display_deko_output_imperial:
	movf	lo,W					; lo = m
	mullw	.100					; PRODL:PRODH = mbar
	movff	PRODL,lo
	movff	PRODH,hi
; Convert with 334feet/100m to have 10ft, 20ft, 30ft stops...
	movff	lo,xA+0
	movff	hi,xA+1
	movlw	LOW 	d'334'          ; 334feet/100m
	movwf	xB+0
	movlw	HIGH 	d'334'
	movwf	xB+1
	call	mult16x16			    ; xA*xB=xC (lo:hi * 328)
	movlw	d'50'                   ; round up
	addwf	xC+0,F
	movlw	0
	addwfc	xC+1,F
	addwfc	xC+2,F
	addwfc	xC+3,F
	movlw	d'100'					
	movwf	xB+0
	clrf	xB+1
	call	div32x16  			    ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	xC+0,lo
	movff	xC+1,hi				    ; restore lo and hi with updated value
	bcf		leftbind
	bsf		ignore_digit4			; Only full feet
	output_16
	STRCAT_TEXT	tFeets1
	return

TFT_display_deko_output_metric:
	output_99
	STRCAT_TEXT	tMeters
	PUTC	' '
	return

	global	TFT_display_deko
TFT_display_deko:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	WIN_MEDIUM	decostop_1st_stop_column,decostop_1st_stop_row
	TFT_color_code		warn_ceiling    ; Color-code Output
	movff	char_O_first_deco_depth,lo  ; Ceiling in m
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_first_deco_time,lo   ; length of first stop in min
	output_99
	STRCAT_PRINT "'"
	call	TFT_standard_color
    return

    global  TFT_decoplan
TFT_decoplan:
    call    TFT_divemask_color
    WIN_TINY    decoplan_title_column,decoplan_title_row
    STRCPY_TEXT_PRINT tDiveDecoplan
	call	TFT_standard_color

	movff	char_O_deco_depth+1,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko2			; Yes
	; No, clear output and return
	call	TFT_standard_color
	WIN_SMALL	decostop_4th_stop_column,decostop_4th_stop_row
	STRCPY_PRINT "  ---  "
	WIN_BOX_BLACK   decostop_2nd_stop_row, divemode_simtext_row-1, decostop_2nd_stop_column, decostop_4th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   decostop_5th_stop_row, divemode_simtext_row-1, decostop_5th_stop_column, decostop_6th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   decostop_6th_stop_row, divemode_simtext_row-1, decostop_6th_stop_column, .159	; top, bottom, left, right
    return
TFT_display_deko2:
	WIN_SMALL	decostop_2nd_stop_column,decostop_2nd_stop_row
	movff	char_O_deco_depth+1,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+1,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"
	movff	char_O_deco_depth+2,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko3			; Yes
	; No, clear output and return
	WIN_BOX_BLACK   decostop_3rd_stop_row, divemode_simtext_row-1, decostop_2nd_stop_column, decostop_4th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   decostop_4th_stop_row, divemode_simtext_row-1, decostop_4th_stop_column, .159	; top, bottom, left, right
	return

TFT_display_deko3:
	WIN_SMALL	decostop_3rd_stop_column,decostop_3rd_stop_row
	movff	char_O_deco_depth+2,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+2,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"

	movff	char_O_deco_depth+3,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko4			; Yes
	; No, clear output and return
	WIN_BOX_BLACK   decostop_4th_stop_row, divemode_simtext_row-1, decostop_4th_stop_column, .159 ; top, bottom, left, right
	return								; Done.

TFT_display_deko4:
	WIN_SMALL	decostop_4th_stop_column,decostop_4th_stop_row
	movff	char_O_deco_depth+3,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+3,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"

	movff	char_O_deco_depth+4,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko5			; Yes
	; No, clear output and return
	WIN_BOX_BLACK   decostop_5th_stop_row, divemode_simtext_row-1, decostop_5th_stop_column, decostop_6th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   decostop_6th_stop_row, divemode_simtext_row-1, decostop_6th_stop_column, .159                     ; top, bottom, left, right
	return								; Done.

TFT_display_deko5:
	WIN_SMALL	decostop_5th_stop_column,decostop_5th_stop_row
	movff	char_O_deco_depth+4,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+4,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"
	movff	char_O_deco_depth+5,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko6			; Yes
	; No, clear output and return
	WIN_BOX_BLACK   decostop_6th_stop_row, divemode_simtext_row-1, decostop_6th_stop_column, .159                     ; top, bottom, left, right
	return								; Done.
TFT_display_deko6:
	WIN_SMALL	decostop_6th_stop_column,decostop_6th_stop_row
	movff	char_O_deco_depth+5,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+5,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"
	movff	char_O_deco_depth+6,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko7			; Yes
	; No, clear output and return
	WIN_BOX_BLACK   decostop_7th_stop_row, divemode_simtext_row-1, decostop_7th_stop_column, .159                     ; top, bottom, left, right
	return								; Done.
TFT_display_deko7:
	WIN_SMALL	decostop_7th_stop_column,decostop_7th_stop_row
	movff	char_O_deco_depth+6,lo  	; stop in m
	bcf     lo,7                        ; Clear GAS_SWITCH bit
	rcall	TFT_display_deko_output_depth ; Outputs depth (stored in lo) to POSTINC2 with "m" or w/o (For ft)
	movff	char_O_deco_time+6,lo   	; length of stop in min
	output_99
	STRCAT_PRINT "'"
	return								; Done.

;TFT_display_deko1:
;	movff	char_O_gradient_factor,lo		; gradient factor
;	movlw	gf_display_threshold			; threshold for display
;	cpfslt	lo				; show value?
;	bra		TFT_display_deko2	; Yes
;	; No
;	bra		TFT_display_ndl_mask2	; Clear gradient factor
;

    global  TFT_clear_safety_stop
TFT_clear_safety_stop:
    WIN_BOX_BLACK   safetystop_text_row, ndl_text_row-.4, safetystop_text_column, .159	; top, bottom, left, right
    return

    global  TFT_show_safety_stop
TFT_show_safety_stop:
	tstfsz	safety_stop_countdown			; Countdown at zero?
	bra		TFT_show_safety_stop2			; No, show stop

	bcf		show_safety_stop				; Clear flag

	btfss	safety_stop_active				; Displayed?
    return                                  ; No
	bcf		safety_stop_active				; Clear flag
    btfsc   divemode_menu                   ; Is the dive mode menu shown?
    return                                  ; Yes, return
    rcall	TFT_clear_safety_stop           ; Yes, Clear stop
    return

TFT_show_safety_stop2:
    bsf     safety_stop_active				; Set flag
    decf	safety_stop_countdown,F			; Reduce countdown

    btfsc   divemode_menu                   ; Is the dive mode menu shown?
    return                                  ; Yes, return
    btfsc   menuview
    bra     TFT_show_safety_stop3           ; No room when menuview=1...

    rcall    TFT_divemask_color
    WIN_STD safetystop_text_column,safetystop_text_row
    STRCPY_TEXT_PRINT tDiveSafetyStop
TFT_show_safety_stop3:
	rcall    TFT_attention_color            ; show in yellow
    WIN_MEDIUM	safetystop_column,safetystop_row
	lfsr	FSR2,buffer
	movff	safety_stop_countdown,lo
	clrf	hi
	call	convert_time					; converts hi:lo in seconds to mins (hi) and seconds (lo)
	movf	hi,W
	movff	lo,hi
	movwf	lo								; exchange lo and hi
    bsf     leftbind
	output_8
    bcf     leftbind
	PUTC    ':'
	movff	hi,lo
	output_99x
	STRCAT_PRINT ""
	WIN_FONT 	FT_SMALL
	rcall	TFT_standard_color
	return

    global  TFT_mask_avr_stopwatch             ; Show mask for average depth and stopwatch
TFT_mask_avr_stopwatch:
    ; The mask
    call    TFT_divemask_color
    WIN_TINY  dive_custom_avr_stop_column1,dive_custom_avr_stop_row
    STRCPY_TEXT_PRINT tDiveTotalAvr
    WIN_TINY  dive_custom_avr_stop_column2,dive_custom_avr_stop_row
    STRCPY_TEXT_PRINT tDiveStopwatch
    WIN_TINY  dive_custom_avr_stop_column3,dive_custom_avr_stop_row
    STRCPY_TEXT_PRINT tDiveStopAvr
    call	TFT_standard_color
    return

    global  TFT_dyn_gaslist
TFT_dyn_gaslist:                            ; Show the dynamic gaslist
    ; The mask
    call    TFT_divemask_color
    WIN_TINY  dive_custom_dyn_mask_column,dive_custom_dyn_mask_row
    STRCPY_TEXT_PRINT tGaslist
    call	TFT_standard_color

    WIN_SMALL dive_custom_dyn_mask_column1,dive_custom_dyn_mask_row1
    movlw   .1
    movwf   tft_gaslist_temp+0
    bsf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL dive_custom_dyn_mask_column1,dive_custom_dyn_mask_row2
    incf    tft_gaslist_temp+0,F     ; +1
    movf    tft_gaslist_temp+0,W     ; into W
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL dive_custom_dyn_mask_column2,dive_custom_dyn_mask_row1
    incf    tft_gaslist_temp+0,F     ; +1
    movf    tft_gaslist_temp+0,W     ; into W
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL dive_custom_dyn_mask_column2,dive_custom_dyn_mask_row2
    incf    tft_gaslist_temp+0,F     ; +1
    movf    tft_gaslist_temp+0,W     ; into W
    rcall   TFT_dyn_gaslist_common
    call	TFT_standard_color
    return

TFT_dyn_gaslist_common:
    cpfseq  active_gas  ;1-5
    bra     $+4
    incf    tft_gaslist_temp+0,F     ; +1
    movff   tft_gaslist_temp+0,lo
    movff   tft_gaslist_temp+0,PRODL
    decf    PRODL,F     ;-1 to have 0-4
    bsf     leftbind
    output_8            ; Gas number
    bcf     leftbind
    PUTC    ":"
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    PUTC    " "         ; Clearing space
    movlw   0x00
    movff   WREG,buffer+.11  ; limit to 11 chars
    STRCAT_PRINT ""
    return


    global  TFT_update_avr_stopwatch           ; Update average depth and stopwatch
TFT_update_avr_stopwatch:
    call    TFT_standard_color
    SAFE_2BYTE_COPY  average_divesecs,lo
	call	convert_time			; lo=secs, hi=mins
    WIN_MEDIUM  dive_avr_stop_column2,dive_avr_stop_row
    bsf     leftbind
	movf	hi,W
	movff	lo,hi
	movwf	lo					; exchange lo and hi
	output_8
	PUTC    ':'
	movff	hi,lo
	output_99x
    movlw   .5
    call    TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    clrf    WREG
    movff   WREG,buffer+.5              ; limit to 5 chars
	STRCAT_PRINT ""

    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_update_avr_stopwatch_metric
;TFT_update_avr_stopwatch_imperial
    movff   avr_rel_pressure_total+0,lo
    movff   avr_rel_pressure_total+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    call	convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
    WIN_MEDIUM  dive_avr_stop_column1,dive_avr_stop_row
    bsf     leftbind
    output_16                       ; yxz
    STRCAT_PRINT " "
    ; Stopped average depth
    movff   avr_rel_pressure+0,lo
    movff   avr_rel_pressure+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    call	convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
    WIN_MEDIUM  dive_avr_stop_column3,dive_avr_stop_row
    output_16                       ; yxz
    bcf     leftbind
    STRCAT_PRINT " "
    return

TFT_update_avr_stopwatch_metric:
    ; Non-resettable average depth
    movff   avr_rel_pressure_total+0,lo
    movff   avr_rel_pressure_total+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    WIN_MEDIUM  dive_avr_stop_column1,dive_avr_stop_row
    bsf     ignore_digit5         ; no cm
    output_16dp  .3               ; yxz.a
    STRCAT_PRINT ""
    ; Stopped average depth
    movff   avr_rel_pressure+0,lo
    movff   avr_rel_pressure+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    WIN_MEDIUM  dive_avr_stop_column3,dive_avr_stop_row
    bsf     ignore_digit5         ; no cm
    output_16dp  .3               ; yxz.a
    bcf     leftbind
    bcf     ignore_digit5
    STRCAT_PRINT ""
    return

    global  TFT_ceiling_mask                        ; The ceiling mask
TFT_ceiling_mask:
    call    TFT_divemask_color
    WIN_TINY  dive_ceiling_text_column,dive_ceiling_text_row
    STRCPY_TEXT_PRINT tCeiling
    call	TFT_standard_color
    return

    global  TFT_ceiling                             ; Ceiling
TFT_ceiling:
    call    TFT_standard_color
    WIN_MEDIUM  dive_ceiling_value_column,dive_ceiling_value_row
    movff   int_O_ceiling+0,lo
    movff   int_O_ceiling+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    bsf     leftbind
    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_ceiling_metric
;TFT_ceiling_imperial
    call	convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
    output_16                       ; yxz
    bcf     leftbind
    STRCAT_PRINT " "
    return

TFT_ceiling_metric:
    bsf     ignore_digit5         ; no cm
    output_16dp  .3               ; yxz.a
    bcf     leftbind
    bcf     ignore_digit5
    STRCAT_PRINT " "
    return

    global  TFT_hud_mask                        ; The HUD mask
TFT_hud_mask:
    call    TFT_divemask_color
    WIN_TINY  dive_custom_hud_column1,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask1
    WIN_TINY  dive_custom_hud_column2,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask2
    WIN_TINY  dive_custom_hud_column3,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask3
    call	TFT_standard_color
    return

    global  TFT_hud_voltages
TFT_hud_voltages:                    ; Show HUD details
    WIN_SMALL .5,dive_hud_data_row
    call	TFT_standard_color
    btfss   use_02_sensor1
    call    TFT_warnings_color
    movff   o2_mv_sensor1+0,lo
    movff   o2_mv_sensor1+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    WIN_SMALL .55,dive_hud_data_row
    call	TFT_standard_color
    btfss   use_02_sensor2
    call    TFT_warnings_color
    movff   o2_mv_sensor2+0,lo
    movff   o2_mv_sensor2+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    WIN_SMALL .105,dive_hud_data_row
    call	TFT_standard_color
    btfss   use_02_sensor3
    call    TFT_warnings_color
    movff   o2_mv_sensor3+0,lo
    movff   o2_mv_sensor3+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    call	TFT_standard_color
    return

    global  TFT_update_hud             ; Update HUD data
TFT_update_hud:
    ; show three sensors
    bsf     leftbind
    movff   o2_ppo2_sensor1,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_update_hud1 ; No
    btfss   dive_hud1_displayed         ; Was the sensor shown?
    bra     TFT_update_hud2             ; Yes, skip clear
    bcf     dive_hud1_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dive_hud_data_row, dive_hud_data_row+.30, dive_hud_sensor1_column, dive_hud_sensor2_column	; top, bottom, left, right
	WIN_STD dive_hud_sensor1_column+.7,dive_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud2 ; Skip Sensor 1
TFT_update_hud1:
    WIN_MEDIUM dive_hud_sensor1_column,dive_hud_data_row
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bsf     dive_hud1_displayed         ; Set display flag
TFT_update_hud2:
    movff   o2_ppo2_sensor2,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_update_hud3 ; No
    btfss   dive_hud2_displayed         ; Was the sensor shown?
    bra     TFT_update_hud4             ; Yes, skip clear
    bcf     dive_hud2_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dive_hud_data_row, dive_hud_data_row+.30, dive_hud_sensor2_column, dive_hud_sensor3_column	; top, bottom, left, right
    WIN_STD dive_hud_sensor2_column+.7,dive_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud4 ; Skip Sensor 2
TFT_update_hud3:
    WIN_MEDIUM dive_hud_sensor2_column,dive_hud_data_row
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bsf     dive_hud2_displayed         ; Set display flag
TFT_update_hud4:
    movff   o2_ppo2_sensor3,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_update_hud5 ; No
    btfss   dive_hud3_displayed         ; Was the sensor shown?
    bra     TFT_update_hud6             ; Yes, skip clear
    bcf     dive_hud3_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dive_hud_data_row, dive_hud_data_row+.30, dive_hud_sensor3_column, .159 ; top, bottom, left, right
    WIN_STD dive_hud_sensor3_column+.7,dive_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud6 ; Skip Sensor 3
TFT_update_hud5:
    WIN_MEDIUM dive_hud_sensor3_column,dive_hud_data_row
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bsf     dive_hud3_displayed         ; Set display flag
TFT_update_hud6:
    bcf     leftbind
   	call	TFT_standard_color
    return

    global  TFT_surface_hud             ; Update HUD data in surface mode
TFT_surface_hud:
    ; show three sensors
    bsf     leftbind
    WIN_SMALL surf_hud_sensor1_column,surf_hud_sensor1_row
    movff   o2_ppo2_sensor1,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_surface_hud1 ; No
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_hud2 ; Skip Sensor 1
TFT_surface_hud1:
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_hud2:
    WIN_SMALL surf_hud_sensor2_column,surf_hud_sensor2_row
    movff   o2_ppo2_sensor2,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_surface_hud3 ; No
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_hud4 ; Skip Sensor 2
TFT_surface_hud3:
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_hud4:
    WIN_SMALL surf_hud_sensor3_column,surf_hud_sensor3_row
    movff   o2_ppo2_sensor3,lo
    tstfsz  lo              ; ppO2=0 (No data/failure)?
    bra     TFT_surface_hud5 ; No
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_hud6 ; Skip Sensor 3
TFT_surface_hud5:
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_hud6:
    bcf     leftbind
   	call	TFT_standard_color
    return

    global  TFT_menu_hud
TFT_menu_hud:            ; Yes, update HUD data
    call    TFT_attention_color         ; show in yellow
    bsf     leftbind
    WIN_SMALL   surf_menu_sensor1_column,surf_menu_sensor1_row
    movff   o2_ppo2_sensor1,lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    PUTC    ","
    movff   o2_mv_sensor1+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor1+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV "
    WIN_SMALL   surf_menu_sensor2_column,surf_menu_sensor2_row
    movff   o2_ppo2_sensor2,lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    PUTC    ","
    movff   o2_mv_sensor2+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor2+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV "
    WIN_SMALL   surf_menu_sensor3_column,surf_menu_sensor3_row
    movff   o2_ppo2_sensor3,lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    PUTC    ","
    movff   o2_mv_sensor3+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor3+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV "
    WIN_SMALL   surf_menu_sensor4_column,surf_menu_sensor4_row

    btfss   c3_hardware
    bra     TFT_menu_hud_2  ; always for normal OSTC3
    btfss   s8_digital
    return                  ; Not for analog
TFT_menu_hud_2:
    STRCPY  "Batt:"
    movff   hud_battery_mv+0,lo      ; in mV
    movff   hud_battery_mv+1,hi      ; in mV
    output_16dp  .2         ; x.yyy V
    STRCAT_PRINT "V"
    call    TFT_standard_color
    bcf     leftbind
    return

    global  TFT_menu_hud2
TFT_menu_hud2:            ; Yes, update mV data
    call    TFT_attention_color         ; show in yellow
    bsf     leftbind
    WIN_SMALL   surf_menu_sensor1_column,surf_menu2_sensor1_row
    movff   o2_mv_sensor1+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor1+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV  "
    WIN_SMALL   surf_menu_sensor2_column,surf_menu2_sensor2_row
    movff   o2_mv_sensor2+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor2+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV  "
    WIN_SMALL   surf_menu_sensor3_column,surf_menu2_sensor3_row
    movff   o2_mv_sensor3+0,lo      ; in 0.1mV steps
    movff   o2_mv_sensor3+1,hi      ; in 0.1mV steps
    output_16dp  .4         ; xxx.y mV
    STRCAT_PRINT "mV  "
    call    TFT_standard_color
    bcf     leftbind
    return

    	global	TFT_clock
TFT_clock:
	WIN_SMALL  surf_clock_column,surf_clock_row
TFT_clock2:                         ; called from divemode clock
   	call	TFT_standard_color
	movff	hours,lo
	output_99
	movlw	':'
	btfss	secs,0					; blinking every second
	movlw	' '
	movwf	POSTINC2
	movff	mins,lo
	output_99x
	STRCAT_PRINT ""
	return

	global	TFT_show_time_date_menu
TFT_show_time_date_menu:
    call    speed_fastest
	WIN_SMALL  .15,.30
   	call	TFT_standard_color
	movff	hours,lo
	output_99
	PUTC	':'
	movff	mins,lo
	output_99x
	PUTC	':'
	movff	secs,lo
	output_99x
	STRCAT  " - "
	movff	month,convert_value_temp+0
	movff	day,convert_value_temp+1
	movff	year,convert_value_temp+2
	call	TFT_convert_date		; converts into "DD/MM/YY" or "MM/DD/YY" or "YY/MM/DD" in postinc2	
	STRCAT_PRINT " "
	return
;=============================================================================

    global  TFT_interval
TFT_interval:
	call	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	STRCPY	"Int:"
	movff	surface_interval+0,lo
	movff	surface_interval+1,hi
	call	convert_time			; lo=mins, hi=hours
	movf	hi,W
	movff	lo,hi
	movwf	lo					; exchange lo and hi
	output_99x
	PUTC    ':'
	movff	hi,lo
	output_99x
    movlw   surf_warning_length         ; No, use surface string length
    call    TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
	return

    global  TFT_compass_fast
TFT_compass_fast:
	WIN_TINY	.20,.50
	STRCPY  "X:"
    movff	compass_DX+0,lo
    movff	compass_DX+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT  " Y:"
    movff	compass_DY+0,lo
    movff	compass_DY+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT  " Z:"
    movff	compass_DZ+0,lo
    movff	compass_DZ+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT_PRINT "  "
    return

    global  TFT_show_timeout_testmode
TFT_show_timeout_testmode:              ; With timeout in WREG...
    movwf   hi
    WIN_TINY	.20,.68
    STRCPY  "T:"
    movf    timeout_counter2,W          ; current timeout
    subwf   hi,W                        ; subtract from timeout value
    addlw   .1                          ; +1
    movwf   lo
    bsf     leftbind
    output_8                            ; Display timeout
    bcf     leftbind
    STRCAT_PRINT "s "
    return


    global  TFT_compass_show_gain
TFT_compass_show_gain:       ; Show the current compass gain
    movff   opt_compass_gain,lo    ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
    tstfsz  lo
    return                         ; Do not show unless gain=0
	WIN_TINY	.20,.86
    STRCPY_TEXT  tCompassGain
    movff   opt_compass_gain,lo    ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
    bsf     leftbind
    output_8
    bcf     leftbind
    STRCAT_PRINT "!"
    return

    global  TFT_surface_decosettings    ; Show all deco settings
TFT_surface_decosettings:
   ; Deco Mode
	call	TFT_standard_color
    movff   char_I_deco_model,WREG
    iorwf   WREG
    bnz     TFT_surface_decosettings1

    ; Display ZH-L16 sat/desat model.
    TEXT_SMALL  surf_gaslist_column,surf_gaslist_row,  tZHL16
    WIN_TOP surf_gaslist_row+(surf_gaslist_spacing*.1)
    lfsr    FSR2,buffer
    movff   char_I_desaturation_multiplier,lo
    bsf     leftbind
    output_8
    STRCAT  "%/"
    movff   char_I_saturation_multiplier,lo
    output_8
    STRCAT_PRINT  "%"
    bra     TFT_surface_decosettings2

   ; Display ZH-L16-GF low/high model.
TFT_surface_decosettings1:
    TEXT_SMALL  surf_gaslist_column,surf_gaslist_row,  tZHL16GF
    WIN_TOP surf_gaslist_row+(surf_gaslist_spacing*.1)
    STRCPY_TEXT tGF         ; GF:
    movff   char_I_GF_Low_percentage,lo
    output_99x
    STRCAT  "/"
    movff   char_I_GF_High_percentage,lo
    output_99x
    STRCAT_PRINT  ""
    ;bra     TFT_surface_decosettings2
TFT_surface_decosettings2:
    ; FTTS
    WIN_TOP surf_gaslist_row+(surf_gaslist_spacing*.2)
    STRCPY_TEXT tFTTSMenu
    movff   char_I_extra_time,lo
    bsf     leftbind
    output_8
    STRCAT_TEXT_PRINT   tMinutes

    ; Last Stop
    WIN_TOP surf_gaslist_row+(surf_gaslist_spacing*.3)
    STRCPY_TEXT tLastDecostop
    movff   char_I_depth_last_deco,lo
    output_8
    STRCAT_TEXT_PRINT   tMeters

    ; Salinity
    WIN_TOP surf_gaslist_row+(surf_gaslist_spacing*.4)
    STRCPY_TEXT tDvSalinity
    movff   opt_salinity,lo
    output_8
    bcf     leftbind
    STRCAT_TEXT_PRINT   tPercent
    return                          ; Done.

    global  TFT_surface_compass_mask
TFT_surface_compass_mask:
    WIN_SMALL   surf_compass_mask_column,surf_compass_mask_row
	call	TFT_standard_color
    STRCPY_TEXT_PRINT   tHeading            ; Heading:
    return

    global  TFT_dive_compass_mask
TFT_dive_compass_mask:
    WIN_TINY    dive_compass_mask_column,dive_compass_mask_row
    call    TFT_divemask_color
    STRCPY_TEXT_PRINT   tHeading            ; Heading:
    return


    global  TFT_surface_compass_heading
TFT_surface_compass_heading:
    rcall   compass_heading_common
    btfsc   compass_fast_mode               ; In fast mode?
    bra     TFT_surface_compass_heading2    ; Yes
    ; No, update 1/second max.
    movff   sensor_state_counter,lo
    movlw   .6
    cpfsgt  lo
    return
TFT_surface_compass_heading2:
    WIN_STD   surf_compass_head_column,surf_compass_head_row
	call	TFT_standard_color
TFT_surface_compass_heading_com:     ; Show "000° N"
    movff	compass_heading+0,lo
    movff	compass_heading+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    bsf     leftbind
    output_16dp .2      ; Result is "0.000"
    bcf     leftbind
    ; rearrange figures to "000"
    movff   buffer+2,buffer+0
    movff   buffer+3,buffer+1
    movff   buffer+4,buffer+2
    lfsr	FSR2,buffer+3
    STRCAT  "° "
    rcall   tft_compass_cardinal        ; Add cardinal and ordinal to POSTINC2
    STRCAT_PRINT " "
    return

    global  TFT_dive_compass_heading
TFT_dive_compass_heading:
    rcall   compass_heading_common
    btfsc   compass_fast_mode               ; In fast mode?
    bra     TFT_dive_compass_heading2       ; Yes
    ; No, update 1/second max.
    movff   sensor_state_counter,lo
    movlw   .6
    cpfsgt  lo
    bra     TFT_dive_compass_heading3       ; But update graph always in fast mode
TFT_dive_compass_heading2:
    WIN_STD dive_compass_head_column,dive_compass_head_row
	call	TFT_standard_color
    rcall   TFT_surface_compass_heading_com  ; Show "000° N"
TFT_dive_compass_heading3:
    return              ; No graphical output (yet)

    movff   compass_heading+0,sub_a+0
    movff   compass_heading+1,sub_a+1
    movlw   .45
    movwf   sub_b+0
    clrf    sub_b+1
    call    subU16                      ;  sub_c = sub_a - sub_b (with UNSIGNED values)
    btfss   neg_flag                    ; Result <0?
    bra     TFT_dive_compass_heading_graph1 ; No
    ; Yes
    movlw   LOW     .360
    movwf   sub_a+0
    movlw   HIGH    .360
    movwf   sub_a+1
    movff   sub_c+0,sub_b+0
    movff   sub_c+1,sub_b+1
    call    subU16                      ;  sub_c = sub_a - sub_b (with UNSIGNED values)

TFT_dive_compass_heading_graph1:
    WIN_SMALL dive_compass_head_column+.70,dive_compass_head_row
    movff   sub_c+0,lo
    movff   sub_c+1,hi
	call	TFT_standard_color
    bsf     leftbind
    output_16
    bcf     leftbind
    STRCAT_PRINT "  "

; Draw marks (left border of graphic is in lo)
    movlw   b'00011111'
    andwf   lo,F                        ; Get lowest 5bits of heading
	movlw	d'30'
	cpfslt	lo
	movwf	lo							; Limit to 30
    rlncf   lo,F                        ; x2
; marks parameters
    WIN_BOX_BLACK   dive_compass_graph_row,dive_compass_graph_row+dive_compass_graph_height,.0,.159
    call	TFT_standard_color
    WIN_SMALL .77,dive_compass_graph_row        ; Center of screen
    STRCPY_PRINT "^"
    call    TFT_divemask_color
    movlw   dive_compass_graph_row
    movff   WREG,win_top
    movlw   dive_compass_graph_height
    movff   WREG,win_height
    movlw   dive_compass_graph_width
    movff   WREG,win_width+0
    clrf    win_width+1
; marks draw loop
    movlw   .6
    movwf   hi                  ; amount of marks (max.)
    clrf    lo_temp
TFT_dive_compass_heading_graph2:
    movlw   LOW      .319
    movwf   sub_a+0
    movlw   HIGH     .319
    movwf   sub_a+1
    movff   lo,sub_b+0
    movff   lo_temp,sub_b+1
    call    subU16
    btfsc   neg_flag
    bra     TFT_dive_compass_heading_graph3 ; Abort when negative
    movff   sub_c+0,PRODL
    movff   sub_c+1,PRODH
    call    TFT_box_write_16bit_win_left    ; With column in PRODL:PRODH
    ;---- Define Window ------------------------------------------------------
	movf	win_width,W
	bcf     STATUS,C
	rlcf    WREG
	movwf   win_width+0
	movlw   0
	rlcf    WREG
	movwf   win_width+1
    call    TFT_box_16bit_win_left
    movlw   .56                             ; 60 px. space
    addwf   lo,F
    movlw   .0
    addwfc  lo_temp,F
;    movlw   .160
;    cpfslt  lo
;    bra     TFT_dive_compass_heading_graph3 ; Abort
    decfsz  hi,F
    bra     TFT_dive_compass_heading_graph2
TFT_dive_compass_heading_graph3:
    return

tft_compass_cardinal:
    btfsc  hi,0          ; Heading >255°?
    bra     tft_compass_cardinal2   ; Yes must be W, NW or N
    ; No, Must be W, SW, S, SE, E, NE or N
    movlw   .23
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_N
    movlw   .68
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_NE
    movlw   .113
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_E
    movlw   .158
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_SE
    movlw   .203
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_S
    movlw   .248
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_SW
    bra     tft_compass_cardinal_W

tft_compass_cardinal2:
    movlw   .37
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_W
    movlw   .82
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_NW
    bra     tft_compass_cardinal_N

tft_compass_cardinal_N:
    STRCAT_TEXT     tN
    return
tft_compass_cardinal_NE:
    STRCAT_TEXT     tNE
    return
tft_compass_cardinal_E:
    STRCAT_TEXT     tE
    return
tft_compass_cardinal_SE:
    STRCAT_TEXT     tSE
    return
tft_compass_cardinal_S:
    STRCAT_TEXT     tS
    return
tft_compass_cardinal_SW:
    STRCAT_TEXT     tSW
    return
tft_compass_cardinal_W:
    STRCAT_TEXT     tW
    return
tft_compass_cardinal_NW:
    STRCAT_TEXT     tNW
    return

compass_heading_common:
    extern  compass
    extern  compass_filter
    rcall   TFT_get_compass
    rcall   TFT_get_compass
    rcall   TFT_get_compass
    rcall   TFT_get_compass
    rcall   TFT_get_compass
    rcall   TFT_get_compass
    call    compass                     ; Do compass corrections.
    banksel common
  
    ; More then compass_fast_treshold?
    movff   compass_heading_old+0,sub_a+0
    movff   compass_heading_old+1,sub_a+1
    movff   compass_heading+0,sub_b+0
    movff   compass_heading+1,sub_b+1
    call    sub16
    movff   compass_heading+0,compass_heading_old+0 ; copy new "old"
    movff   compass_heading+1,compass_heading_old+1

    bcf     compass_fast_mode
    movlw   compass_fast_treshold
    cpfslt  sub_c+0                             ; > compass_fast_treshold?
    bsf     compass_fast_mode                   ; Yes!
    return

TFT_get_compass:
	call	speed_normal
    call    I2C_RX_compass              ; Test Compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter Raw compass + accel readings.
    banksel common
    return

	global	TFT_debug_output
TFT_debug_output:
    return
    WIN_TINY   .80,.0
	call	TFT_standard_color
	lfsr	FSR2,buffer
    movff   int_O_ceiling+0,lo
    movff   int_O_ceiling+1,hi
    output_16
	STRCAT_PRINT ""
    return

    global  TFT_divetimeout                     ; Show timeout counter
TFT_divetimeout:
	call	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No

    call	TFT_standard_color
    STRCPY  0x94                        ; "End of dive" icon
    movlw   LOW     divemode_timeout
    movwf   sub_a+0
    movlw   HIGH    divemode_timeout
    movwf   sub_a+1
    movff   timeout_counter,sub_b+0
    movff   timeout_counter2,sub_b+1
    call    subU16  ;  sub_c = sub_a - sub_b (with UNSIGNED values)
	movff	sub_c+0, lo
	movff	sub_c+1, hi
	call	convert_time				; converts hi:lo in minutes to hours (hi) and minutes (lo)
	movf	hi,W
	movff	lo,hi
	movwf	lo							; exchange lo and hi
	output_99x
	PUTC    ':'
	movff	hi,lo
	output_99x
    movlw   warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
	return

	global	TFT_ftts
TFT_ftts:
    movff   char_I_extra_time,lo
    tstfsz  lo
    bra     $+4
    return                              ; char_I_extra_time=0, return.
	incf	warning_counter,F			; increase counter
	call	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    movff   char_I_extra_time,lo
    STRCPY  "@+"
    bsf     leftbind
    output_8
    PUTC    ":"
	movff   int_O_extra_ascenttime+0,lo
    movff   int_O_extra_ascenttime+1,hi
    movf    lo,W
	iorwf   hi,W                    ; extra_ascenttime == 0 ?
	bz      TFT_ftts2   			; No deco
	movf    lo,W                    ; extra_ascenttime == 0xFFFF ?
	andwf   hi,W
	incf    WREG,w
	bz      TFT_ftts2       		; Wait...
	output_16
	bcf         leftbind
    PUTC    "'"
    movlw   warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
	return

TFT_ftts2:
    STRCAT  "---"
	bcf     leftbind
    movlw   warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
    return


;=============================================================================
	
	global	TFT_temp_surfmode
TFT_temp_surfmode:
	WIN_SMALL   surf_temp_column,surf_temp_row
	call	TFT_standard_color

    SAFE_2BYTE_COPY    temperature, lo

    TSTOSS  opt_units   			; 0=°C, 1=°F
	bra		TFT_temp_surfmode_metric

;TFT_temp_surfmode_imperial:
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	call	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit
	lfsr	FSR2,buffer						; Overwrite "-"
	bsf		ignore_digit5		; Full degrees only
	output_16
	STRCAT_PRINT  ""
    call    TFT_divemask_color
	WIN_SMALL   surf_temp_column+4*8,surf_temp_row
	STRCPY_PRINT  "°F"
	return

TFT_temp_surfmode_metric:
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	movlw	d'3'
	movwf	ignore_digits
	bsf		ignore_digit5		; Full degrees only
	output_16

	; read-back the buffer+4
	movff	buffer+4,lo
	movlw	" "					; Space
	cpfseq	lo					; Was it a space (between +1°C and -1°C)?
	bra		TFT_temp_surfmode1	; No.
	movlw	"0"					; Yes, print manual zero
	movff	WREG,buffer+3
	bra		TFT_temp_surfmode2
TFT_temp_surfmode1:
	; Test if output was negative (Flag set in TFT_convert_signed_16bit)
	btfss	neg_flag			; Negative temperature?
    bra		TFT_temp_surfmode3	; No, continue
	; Yes, negative temperature!
	movff	buffer+3,buffer+2	; remove two spaces manually
	movff	buffer+4,buffer+3
TFT_temp_surfmode2:
	movlw	0x00
	movff	WREG,buffer+4
TFT_temp_surfmode3:
	STRCAT_PRINT  ""
    call    TFT_divemask_color
	WIN_SMALL   surf_temp_column+4*8,surf_temp_row
	STRCPY_PRINT  "°C"
	return

;=============================================================================
    global  TFT_divemode_menu_cursor
TFT_divemode_menu_cursor:
    WIN_BOX_BLACK   divemode_menu_item1_row,divemode_menu_item3_row+.24,divemode_menu_item1_column-.8,divemode_menu_item1_column-.1
    WIN_BOX_BLACK   divemode_menu_item4_row,divemode_menu_item6_row+.24,divemode_menu_item4_column-.8,divemode_menu_item4_column-.1
    call	TFT_standard_color

    movlw   divemode_menu_item1_column-.8
    btfsc   menupos,2       ; >3?
    movlw   divemode_menu_item4_column-.8  ; Yes
    movff   WREG,win_leftx2
    
    movff   menupos,lo                      ; Copy menu pos
    movlw   divemode_menu_item6_row
    dcfsnz  lo,F
    movlw   divemode_menu_item1_row
    dcfsnz  lo,F
    movlw   divemode_menu_item2_row
    dcfsnz  lo,F
    movlw   divemode_menu_item3_row
    dcfsnz  lo,F
    movlw   divemode_menu_item4_row
    dcfsnz  lo,F
    movlw   divemode_menu_item5_row
    movff   WREG,win_top
    movlw   FT_SMALL
    movff   WREG,win_font
    STRCPY_PRINT    "\xb7"          ; print cursor
    return

	global	TFT_temp_divemode
TFT_temp_divemode:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	btfsc	blinking_better_gas         ; blinking better Gas?
	return                              ; Yes, no update of temperature now
; temperature
	WIN_SMALL	dive_temp_column,dive_temp_row
	call	TFT_standard_color
    bsf     leftbind

    SAFE_2BYTE_COPY    temperature, lo
    TSTOSS  opt_units   			; 0=°C, 1=°F
	bra		TFT_temp_divemode_metric

;TFT_temp_divemode_imperial:
	call	TFT_convert_signed_16bit        ; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	call	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit
	lfsr	FSR2,buffer						; Overwrite "-" (There won't be less then -18°C underwater...)
	bsf		ignore_digit5		; Full degrees only
	output_16
	STRCAT_TEXT tLogTunitF
TFT_temp_divemode_common:
    bcf     leftbind
    movlw   .4                      ; limit to three chars
    call    TFT_fillup_with_spaces  ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT    ""
	return                          ; Done.

TFT_temp_divemode_metric:
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	movlw	d'3'
	movwf	ignore_digits
	bsf		ignore_digit5		; Full degrees only
	output_16
	STRCAT_TEXT tLogTunitC
    bra     TFT_temp_divemode_common    ; Done.

TFT_active_setpoint2:
	movff	char_I_const_ppO2,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
	clrf	hi
	bsf		leftbind
	output_16dp d'3'
    bcf		leftbind
    STRCAT_TEXT tbar
    TSTOSS  opt_ccr_mode                    ; =0: Fixed SP, =1: Sensor
    bra     $+4
    PUTC    "*"
	STRCAT_PRINT ""
    return

TFT_active_setpoint:         ; Show setpoint
	WIN_STD  active_gas_column,active_gas_row
	call	TFT_standard_color
    btfsc   is_bailout                  ; =1: Bailout
    bra     TFT_active_setpoint_bail    ; Show "Bailout" instead of Setpoint

    rcall   TFT_active_setpoint2        ; show setpoint (Non-Inverted in all cases)

    btfss	setpoint_fallback           ; =1: Fallback to SP1 due to external O2 sensor failure
    bra     TFT_active_setpoint_diluent ; Done.
	btg		blinking_setpoint           ; Toggle blink bit...
	btfss	blinking_setpoint           ; blink now?
	bra     TFT_active_setpoint_diluent ; Done.

	movlw	color_yellow                ; Blink in yellow
    call	TFT_set_color
    WIN_STD_INVERT  active_gas_column,active_gas_row
	movff	char_I_const_ppO2,lo
	clrf	hi
	bsf		leftbind
	output_16dp d'3'
    bcf		leftbind
    STRCAT_TEXT tbar
    TSTOSS  opt_ccr_mode                    ; =0: Fixed SP, =1: Sensor
    bra     $+4
    PUTC    "*"
	STRCAT_PRINT ""
	WIN_INVERT	.0                      ; Init new Wordprocessor

TFT_active_setpoint_diluent:
    call	TFT_standard_color
	WIN_SMALL  active_dil_column,active_dil_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
    rcall   TFT_show_dil_divemode2      ; Show diluent  (Non-Inverted in all cases)

	btfss	better_gas_available        ; =1: A better gas is available and a gas change is advised in divemode
	return					; Done.
	btg		blinking_better_gas         ; Toggle blink bit...
	btfss	blinking_better_gas         ; blink now?
	return                              ; No, Done.

	movlw	color_yellow                ; Blink in yellow
    call	TFT_set_color
    WIN_SMALL_INVERT  active_dil_column,active_dil_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_show_dil_divemode2      ; Show gas
	WIN_INVERT	.0                      ; Init new Wordprocessor
	call	TFT_standard_color
	return                              ; Done.

TFT_show_dil_divemode2:
    call    customview_show_mix         ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
	STRCAT_PRINT ""
	return

TFT_active_setpoint_bail:
    STRCPY_TEXT_PRINT   tDiveBailout        ; Bailout
    bra     TFT_active_setpoint_diluent

	global	TFT_active_gas_divemode
TFT_active_gas_divemode:				; Display gas/Setpoint
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	btfsc	FLAG_apnoe_mode				; Ignore in Apnoe mode
	return
    btfsc   FLAG_ccr_mode               ; in CCR mode
    bra     TFT_active_setpoint         ; Yes, show setpoint

    call    TFT_standard_color
	WIN_STD active_gas_column,active_gas_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_active_gas_divemode2    ; Show gas (Non-Inverted in all cases)
	btfss	better_gas_available        ; =1: A better gas is available and a gas change is advised in divemode
	return					; Done.

	btg		blinking_better_gas         ; Toggle blink bit...
	btfss	blinking_better_gas         ; blink now?
	return                              ; No, Done.
    call    TFT_attention_color         ; blink in yellow
    WIN_STD_INVERT active_gas_column,active_gas_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_active_gas_divemode2    ; Show gas (Non-Inverted in all cases)
	WIN_INVERT	.0                      ; Init new Wordprocessor
	call	TFT_standard_color
	return                              ; Done.

TFT_active_gas_divemode2:
    call    customview_show_mix         ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
	STRCAT_PRINT ""
	return

	global	TFT_display_decotype_surface
TFT_display_decotype_surface:
	WIN_STD  surf_decotype_column,surf_decotype_row
    WIN_COLOR	color_lightblue
    movff   opt_dive_mode,lo        ; 0=OC, 1=CC, 2=Gauge, 3=Apnea
    tstfsz  lo
    bra     TFT_display_decotype_surface2
TFT_display_decotype_surface0:
    STRCAT_TEXT_PRINT	tDvOC	; OC
    bra     TFT_display_decotype_exit
TFT_display_decotype_surface2:
    decfsz  lo,F
    bra     TFT_display_decotype_surface3
    STRCAT_TEXT_PRINT   tDvCC	; CC
    call	TFT_standard_color
	WIN_TINY surf_decotype_column+.18,surf_decotype_row+.12

    TSTOSS  opt_ccr_mode        ; =0: Fixed SP, =1: Sensor
    bra     TFT_display_decotype_cc_fixed
    ; Sensor mode
    STRCPY_TEXT tCCRModeSensor ; Sensor
    bra     TFT_display_decotype_cc_common
TFT_display_decotype_cc_fixed:
    STRCPY_TEXT tCCRModeFixedSP ; Fixed
TFT_display_decotype_cc_common:
    STRCAT_PRINT ""
    bra     TFT_display_decotype_exit
TFT_display_decotype_surface3:
    decfsz  lo,F
    bra     TFT_display_decotype_surface4
TFT_display_decotype_surface3_1:
    STRCAT_TEXT_PRINT	tDvGauge	; Gauge
    bra     TFT_display_decotype_exit
TFT_display_decotype_surface4:
    STRCAT_TEXT_PRINT	tDvApnea	; Apnea
TFT_display_decotype_exit:
    call	TFT_standard_color
    return

    global  TFT_display_decotype_surface1   ; Used from logbook!
TFT_display_decotype_surface1:  ; Used from logbook!
    tstfsz  lo
    bra     TFT_display_decotype_surface1_2
    bra     TFT_display_decotype_surface0   ;OC
TFT_display_decotype_surface1_2:
    decfsz  lo,F
    bra     TFT_display_decotype_surface1_3
    STRCAT_TEXT_PRINT   tDvCC               ; CC (w/o Sensor/Fixed Display)
TFT_display_decotype_surface1_3:
    decfsz  lo,F
    bra     TFT_display_decotype_surface4   ; Apnea
    bra     TFT_display_decotype_surface3_1 ; Gauge

;=============================================================================

    global  TFT_splist_surfmode     ; Show Setpoint list
    extern  gaslist_strcat_setpoint
TFT_splist_surfmode:
    bsf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    ;SP 1
    WIN_SMALL surf_gaslist_column,surf_gaslist_row
    clrf    PRODL
    call    gaslist_strcat_setpoint     ; Show SP#+1 of PRODL#
    STRCAT_PRINT ""
    ;SP 2
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.1)
    movlw   .1
    movwf   PRODL
    call    gaslist_strcat_setpoint     ; Show SP#+1 of PRODL#
    STRCAT_PRINT ""
    ;SP 3
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.2)
    movlw   .2
    movwf   PRODL
    call    gaslist_strcat_setpoint     ; Show SP#+1 of PRODL#
    STRCAT_PRINT ""
    ;SP 4
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.3)
    movlw   .3
    movwf   PRODL
    call    gaslist_strcat_setpoint     ; Show SP#+1 of PRODL#
    STRCAT_PRINT ""
    ;SP 5
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.4)
    movlw   .4
    movwf   PRODL
    call    gaslist_strcat_setpoint     ; Show SP#+1 of PRODL#
    STRCAT_PRINT ""
    bcf     leftbind
    return

	global	TFT_gaslist_surfmode
TFT_gaslist_surfmode:				; Displays Gas List
    bsf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    extern  gaslist_strcat_gas_mod
    ;Gas 1
    WIN_SMALL surf_gaslist_column,surf_gaslist_row
    movlw   .0
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Gas 2
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.1)
    movlw   .1
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Gas 3
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.2)
    movlw   .2
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Gas 4
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.3)
    movlw   .3
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Gas 5
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.4)
    movlw   .4
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    bcf     leftbind
    return

	global	TFT_dillist_surfmode
TFT_dillist_surfmode:				; Displays Diluent List
    bsf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    ;Dil 1
    WIN_SMALL surf_gaslist_column,surf_gaslist_row
    movlw   .5
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Dil 2
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.1)
    movlw   .6
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Dil 3
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.2)
    movlw   .7
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Dil 4
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.3)
    movlw   .8
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    ;Dil 5
    WIN_SMALL surf_gaslist_column,surf_gaslist_row+(surf_gaslist_spacing*.4)
    movlw   .9
    movwf   PRODL
    call    gaslist_strcat_gas_mod  ;Append gas description of gas #PRODL (0-4) to current string
    STRCAT_PRINT ""
    bcf     leftbind
    return

	global	TFT_depth
TFT_depth:
    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]

    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_depth_metric
;TFT_depth_imperial
	WIN_LARGE	depth_feet_column,depth_feet_row
	TFT_color_code	warn_depth			; Color-code the output

    clrf    sub_a+1                     ; Display 0ft if lower then 30cm
	movlw	d'30'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag				; Depth lower then 0.4m?
	bra		depth_less_0.3mtr_feet	; Yes, Show 0ft manually

	call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
	bsf		leftbind
	output_16						; feet in Big font
    bcf		leftbind
    movlw   .3                      ; limit to three chars
    call    TFT_fillup_with_spaces  ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""					; Display feet
    return

depth_less_0.3mtr_feet:
	STRCAT_PRINT "0  "				; manual zero
	return

TFT_depth_metric:
	WIN_LARGE	depth_column,depth_row
	TFT_color_code	warn_depth			; Color-code the output

	movlw	.039
	cpfslt	hi
    bra		depth_greater_99_84mtr

	btfsc	depth_greater_100m			; Was depth>100m during last call
	rcall	TFT_clear_depth             ; Yes, clear depth area
	bcf		depth_greater_100m			; Do this once only...

	movlw	.039
	cpfslt	hi
    bra		depth_greater_99_84mtr

	movlw	HIGH	d'1000'
	movwf	sub_a+1
	movlw	LOW		d'1000'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	incf	sub_b+0,F
	movlw	d'0'
	addwfc	sub_b+1,F				; Add 1mbar offset
	call	sub16					; sub_c = sub_a - sub_b
    movlw   ' '
	btfss	neg_flag				; Depth lower then 10m?
    movwf   POSTINC2                ; Yes, add extra space

	clrf    sub_a+1
	movlw	d'99'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag				; Depth lower then 1m?
	bra		tft_depth2				; Yes, display manual Zero

	bsf		leftbind
	bsf		ignore_digit4
	output_16						; Full meters in Big font
	bcf		leftbind
	bra		tft_depth3

tft_depth2:
	STRCAT	"0"

tft_depth3:
	STRCAT_PRINT ""					; Display full meters

	; .1m in MEDIUM font
	WIN_MEDIUM	depth_dm_column,depth_dm_row
	TFT_color_code	warn_depth			; Color-code the output

    SAFE_2BYTE_COPY rel_pressure, lo
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]

	PUTC    "."
	movlw	HIGH	d'30'			; Display 0.0m if lower then 30cm
	movwf	sub_a+1
	movlw	LOW		d'30'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag				; Depth lower then 0.3m?
	bra		depth_less_0.3mtr		; Yes, Show ".0" manually

	movlw	d'4'
	movwf	ignore_digits
	bsf		ignore_digit5
	output_16dp	d'0'
	STRCAT_PRINT ""					; Display decimeters
	WIN_FONT 	FT_SMALL
	return

depth_less_0.3mtr:
	STRCAT_PRINT "0"				; Display 0.0m manually
	WIN_FONT 	FT_SMALL
	return

depth_greater_99_84mtr:			; Display only in full meters
	btfss	depth_greater_100m		; Is depth>100m already?
	rcall	TFT_clear_depth			; No, clear depth area and set flag
	; Depth is already in hi:lo
	; Show depth in Full meters
	; That means ignore figure 4 and 5
	lfsr    FSR2,buffer
	bsf		ignore_digit4
	bsf		leftbind
	output_16
	bcf		leftbind
    STRCAT_PRINT ""					; Display full meters only
	WIN_FONT 	FT_SMALL
	return

TFT_clear_depth:            			; No, clear depth area and set flag
    WIN_BOX_BLACK   depth_row, .77,.0, max_depth_column-.1    ;top, bottom, left, right
	bsf		depth_greater_100m			; Set Flag
	return

;=============================================================================

;	global	TFT_user_image
;TFT_user_image:
;    ;---- Display user image -------------------------------------------------
;    ; Compute address in external EEPROM
;    movff   opt_skin,WREG
;    mullw   0x50
;    movff   PRODL,ext_flash_address+1
;    movf    PRODH,W
;    iorlw   0x30
;    movwf   ext_flash_address+2
;
;    ; First pixel at @+4:
;    movlw   4
;    movwf   ext_flash_address+0
;
;    ; Read first pixel
;	call	ext_flash_read_block_start
;;	movff   SSP2BUF,skin_color+1        ; TFT format: HIGH is first...
;	movwf	SSP2BUF						; Write to buffer to initiate new read
;	btfss	SSP2STAT, BF                ; Next byte ready ?
;	bra		$-2                         ; NO: wait...
;;   	movff   SSP2BUF,skin_color+0
;	call    ext_flash_read_block_stop
;
;   ; Make a frame of the retrieved skin color.
;    setf    win_color1
;    setf    win_color2
;	WIN_FRAME_COLOR16 user_image_upper-.1, user_image_upper+.100,user_image_left-.1, user_image_left+.50
;
;    WIN_LEFT    user_image_left+.25
;    WIN_TOP     user_image_upper+.50
;
;    ; Display skin icon
;    clrf        ext_flash_address+0
;    call        TFT_write_flash_image_addr
;
;    ;---- Print custom text string
;    WIN_LEFT    user_image_left+.50+.5
;    WIN_TOP     user_image_upper+.0
;
;    ; ---- STRNCPY : String copy from RAM
;   ; lfsr        FSR0, opt_name          ; Source
;    lfsr        FSR1, .13               ; Len max
;    lfsr        FSR2, buffer            ; destination
;TFT_user_image_1:
;    movf        POSTINC0,W              ; Get byte
;    bz          TFT_user_image_2        ; End if NULL
;    movwf       POSTINC2                ; NO: copy
;    decfsz      FSR1L                   ; Max len reached ?
;    bra         TFT_user_image_1        ; NO: loop
;TFT_user_image_2:
;    clrf        POSTINC2                ; Mark end of string
;
;    goto        aa_wordprocessor        ; and print


    global  TFT_custom_text
TFT_custom_text:            ; Show the custom text
    lfsr        FSR0, opt_name          ; Source
    WIN_SMALL   surf_customtext_column,surf_customtext_row1 ; First row
    rcall       TFT_custom_text_2       ; Show up to 12 chars and print
    incfsz      lo,F                    ; Was lo=255?
    return                              ; No, all done.
    lfsr        FSR0, opt_name+.12      ; Source
    WIN_SMALL   surf_customtext_column,surf_customtext_row2 ; Second row
    rcall       TFT_custom_text_2       ; Show up to 12 chars and print
    incfsz      lo,F                    ; Was lo=255?
    return                              ; No, all done.
    lfsr        FSR0, opt_name+.24      ; Source
    WIN_SMALL   surf_customtext_column,surf_customtext_row3 ; Third row
    rcall       TFT_custom_text_2       ; Show up to 12 chars and print
    incfsz      lo,F                    ; Was lo=255?
    return                              ; No, all done.
    lfsr        FSR0, opt_name+.36      ; Source
    WIN_SMALL   surf_customtext_column,surf_customtext_row4 ; Forth row
    rcall       TFT_custom_text_2       ; Show up to 12 chars and print
    incfsz      lo,F                    ; Was lo=255?
    return                              ; No, all done.
    lfsr        FSR0, opt_name+.48      ; Source
    WIN_SMALL   surf_customtext_column,surf_customtext_row5 ; Fifth row
    rcall       TFT_custom_text_2       ; Show up to 12 chars and print
    return                              ; Done.

TFT_custom_text_2:
    lfsr        FSR2, buffer            ; destination
    movlw       .12
    movwf       lo                      ; length/line
TFT_custom_text_3:
    movf        POSTINC0,W              ; Get byte
    bz          TFT_custom_text_4       ; End if NULL
    movwf       POSTINC2                ; NO: copy
    decfsz      lo,F                    ; Max len reached ?
    bra         TFT_custom_text_3       ; NO: loop
    setf        lo                      ; lo=255 -> more to come
TFT_custom_text_4:
    clrf        POSTINC2                ; Mark end of string
    goto        aa_wordprocessor        ; print and return


;=============================================================================
	global	TFT_update_surf_press
TFT_update_surf_press:
    WIN_SMALL   surf_press_column,surf_press_row
	call	TFT_standard_color
    SAFE_2BYTE_COPY amb_pressure, lo
	movff	lo,sub_a+0
	movff	hi,sub_a+1
	movff	last_surfpressure_30min+0,sub_b+0
	movff	last_surfpressure_30min+1,sub_b+1
	call	subU16					; sub_c = sub_a - sub_b
	btfsc	neg_flag				; Pressure lower?
	rcall	update_surf_press2		; Yes, test threshold

	tstfsz	sub_c+1					; >255mbar difference?
	bra		update_surf_press_common; Yes, display!
	movlw	d'10'					; 10mbar noise suppression
	subwf	sub_c+0,W
	btfsc	STATUS,C
	bra		update_surf_press_common; Yes, display!
    SAFE_2BYTE_COPY last_surfpressure_30min, lo	; Overwrite with stable value...

update_surf_press_common:
	output_16
	; Show only 4 figures
	movff	buffer+1,buffer+0
	movff	buffer+2,buffer+1
	movff	buffer+3,buffer+2
	movff	buffer+4,buffer+3
	movlw	0x00
	movff	WREG,buffer+4
	STRCAT_PRINT  ""
    call    TFT_divemask_color
	WIN_SMALL   surf_press_column+4*8,surf_press_row
	STRCPY_PRINT  "mbar"
	return

update_surf_press2:
	movff	lo,sub_b+0
	movff	hi,sub_b+1
	movff	last_surfpressure_30min+0,sub_a+0
	movff	last_surfpressure_30min+1,sub_a+1
	call	subU16					; sub_c = sub_a - sub_b
	return

;=============================================================================

	global	TFT_update_batt_voltage
TFT_update_batt_voltage:
    movff   batt_percent,lo         ; Get battery percent
    TFT_color_code		warn_battery; Color-code battery percent
	WIN_TINY batt_percent_column,batt_percent_row
	bsf		leftbind
	output_8
	bcf		leftbind
	STRCAT	"% "
	movlw	0x00
	movff	WREG,buffer+4			; Only "xxx%"
    STRCAT_PRINT	""
	call	TFT_standard_color
	WIN_TINY batt_voltage_column,batt_voltage_row
	movff	batt_voltage+0,lo
	movff	batt_voltage+1,hi
	bsf		leftbind
	output_16dp	.2
	bcf		leftbind
	PUTC	'V'
	movff	buffer+5,buffer+3
	movlw	0x00
	movff	WREG,buffer+4			; Only "x.yV"
    STRCAT_PRINT	""
	return

;update_battery_debug:
;	call	TFT_standard_color
;	WIN_TINY .70,.0
;	movff	battery_gauge+5,xC+3
;	movff	battery_gauge+4,xC+2
;	movff	battery_gauge+3,xC+1
;	movff	battery_gauge+2,xC+0
;	; battery_gauge:6 is nAs
;	; devide through 65536
;	; devide through 152
;	; Result is 0.01Ah in xC+1:xC+0
;	movlw	LOW		.152
;	movwf	xB+0
;	movlw	HIGH	.152
;	movwf	xB+1
;	call	div32x16	  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
;	bsf		leftbind
;	movff	xC+0,lo
;	movff	xC+1,hi
;	output_16
;	STRCAT_PRINT	"x.01Ah"
;	WIN_FONT	FT_SMALL
;	bcf		leftbind
;	return
		
;=============================================================================

	global	TFT_convert_signed_16bit
TFT_convert_signed_16bit:
	bcf			neg_flag				; Positive temperature
   	btfss   	hi,7                    ; Negative temperature ?
    return								; No, return
; Yes, negative temperature!
	bsf			neg_flag				; Negative temperature
	PUTC		'-'                     ; Display "-"
    comf    	hi                      ; Then, 16bit sign changes.
    negf    	lo
    btfsc   	STATUS,C
    incf    	hi
	return								; and return

;=============================================================================

	global	TFT_convert_date
TFT_convert_date:	; converts into "DD/MM/YY" or "MM/DD/YY" or "YY/MM/DD" in postinc2
	movff	opt_dateformat,WREG		; =0:MMDDYY, =1:DDMMYY, =2:YYMMDD
	movwf	EEDATA					; used as temp here
	tstfsz	EEDATA
	bra		TFT_convert_date1
; EEDATA was 0
; Use MMDDYY
	movff	convert_value_temp+0,lo			;month
	bsf		leftbind
	output_99x
	PUTC    '.'
	movff	convert_value_temp+1,lo			;day
	bra 	TFT_convert_date1_common		;year

TFT_convert_date1:	; Read date format (0=MMDDYY, 1=DDMMYY, 2=YYMMDD)
	decfsz	EEDATA,F
	bra		TFT_convert_date2				; EEDATA was 2
; EEDATA was 1
; Use DDMMYY
	movff	convert_value_temp+1,lo			;day
	bsf		leftbind
	output_99x
	PUTC    '.'
	movff	convert_value_temp+0,lo			;month

TFT_convert_date1_common:
	bsf		leftbind
	output_99x
	PUTC    '.'
	movff	convert_value_temp+2,lo			;year
	output_99x
	bcf		leftbind
	return

TFT_convert_date2:
; Use YYMMDD
	movff	convert_value_temp+2,lo			;year
	bsf		leftbind
	output_99x
    PUTC    '.'
	movff	convert_value_temp+0,lo			;month
	output_99x
    PUTC    '.'
	movff	convert_value_temp+1,lo			;day
	output_99x
	bcf		leftbind
	return

;=============================================================================

	global	TFT_convert_date_short
TFT_convert_date_short:	; converts into "DD/MM" or "MM/DD" or "MM/DD" in postinc2
	movff	opt_dateformat,WREG		; =0:MMDDYY, =1:DDMMYY, =2:YYMMDD
	movwf	EEDATA					; used as temp here
	tstfsz	EEDATA
	bra		TFT_convert_date_short1
; EEDATA was 0
; Use MMDDYY
TFT_convert_date_short_common:
	movff	convert_value_temp+0,lo			;month
	bsf		leftbind
	output_99x
    PUTC    '.'
	movff	convert_value_temp+1,lo			;day
	output_99x
	bcf		leftbind
	return

TFT_convert_date_short1:
	decfsz	EEDATA,F
	bra		TFT_convert_date_short_common	; EEDATA was 2 -> Use YYMMDD
; EEDATA was 1
; Use DDMMYY
	movff	convert_value_temp+1,lo			;day
	bsf		leftbind
	output_99x
    PUTC    '.'
	movff	convert_value_temp+0,lo			;month
	output_99x
	bcf		leftbind
	return

;=============================================================================

	global	TFT_date
TFT_date:
    WIN_SMALL  surf_date_column,surf_date_row				; Init new Wordprocessor
	call	TFT_standard_color
	movff	month,convert_value_temp+0
	movff	day,convert_value_temp+1
	movff	year,convert_value_temp+2
	call	TFT_convert_date		; converts into "DD/MM/YY" or "MM/DD/YY" or "YY/MM/DD" in postinc2	
	STRCAT_PRINT ""
	return

;=============================================================================

	global	TFT_max_pressure
TFT_max_pressure:
	btfsc	FLAG_apnoe_mode						; different display in apnoe mode
	bra		TFT_max_pressure_apnoe
TFT_max_pressure2:
    SAFE_2BYTE_COPY max_pressure, lo
TFT_max_pressure3:
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_max_pressure2_metric
;TFT_max_pressure2_imperial
	call	convert_mbar_to_feet              	; convert value in lo:hi from mbar to feet
	WIN_MEDIUM	max_depth_feet_column,max_depth_feet_row
	call	TFT_standard_color
	output_16_3
	STRCAT_PRINT ""
	return

TFT_max_pressure2_metric:
    WIN_MEDIUM	max_depth_column,max_depth_row

	movlw	.039
	cpfslt	hi
    bra		max_depth_greater_99_84mtr

	btfsc	max_depth_greater_100m			; Was depth>100m during last call
	rcall	TFT_clear_max_depth             ; Yes, clear depth area
	bcf		max_depth_greater_100m			; Do this once only...

	movlw	.039
	cpfslt	hi
    bra		max_depth_greater_99_84mtr

	movlw	HIGH	d'1000'
	movwf	sub_a+1
	movlw	LOW		d'1000'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	incf	sub_b+0,F
	movlw	d'0'
	addwfc	sub_b+1,F				; Add 1mbar offset
	call	sub16					; sub_c = sub_a - sub_b
    movlw   ' '
	btfss	neg_flag				; Depth lower then 10m?
    movwf   POSTINC2                ; Yes, add extra space

	clrf    sub_a+1
	movlw	d'99'
	movwf	sub_a+0
	movff	hi,sub_b+1
	movff	lo,sub_b+0
	call	subU16					; sub_c = sub_a - sub_b
	btfss	neg_flag				; Depth lower then 1m?
	bra		tft_max_depth2          ; Yes, display manual Zero

	bsf     ignore_digit4			; no 0.1m
    bsf     leftbind
	output_16
	bra		tft_max_depth3

tft_max_depth2:
	WIN_MEDIUM	max_depth_column,max_depth_row
	STRCAT	"0"

tft_max_depth3:
	call	TFT_standard_color
	STRCAT_PRINT ""					; Display full meters
    bcf     leftbind

	; .1m in SMALL font
	WIN_SMALL	max_depth_dm_column,max_depth_dm_row

    SAFE_2BYTE_COPY max_pressure, lo
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]

	PUTC    "."

	movlw	d'4'
	movwf	ignore_digits
	bsf		ignore_digit5
    bsf     leftbind
	output_16dp	d'0'
	STRCAT_PRINT ""					; Display decimeters
    bcf     leftbind
	return

max_depth_greater_99_84mtr:             ; Display only in full meters
	btfss	max_depth_greater_100m		; Is max depth>100m already?
	rcall	TFT_clear_max_depth			; No, clear max depth area and set flag
	; Max. Depth is already in hi:lo
	; Show max. depth in Full meters
	; That means ignore figure 4 and 5
	lfsr    FSR2,buffer
	bsf		ignore_digit4
	bsf		leftbind
	output_16
	bcf		leftbind
    STRCAT_PRINT ""					; Display full meters only
	WIN_FONT 	FT_SMALL
	return

TFT_clear_max_depth:            			; No, clear max. depth area and set flag
    WIN_BOX_BLACK   max_depth_row,.49,max_depth_column, max_depth_dm_column+.13    ;top, bottom, left, right
	bsf		max_depth_greater_100m			; Set Flag
	return


TFT_max_pressure_apnoe:
	btfss	FLAG_active_descent				; Are we descending?			
	bra		TFT_max_pressure2				; Yes, show normal max.
	SAFE_2BYTE_COPY apnoe_max_pressure, lo
	bra		TFT_max_pressure3				; Show apnoe_max_pressure as max. depth

	global	TFT_display_apnoe_last_max
TFT_display_apnoe_last_max:
    call    TFT_divemask_color
    WIN_TINY    last_max_apnoe_text_column,last_max_apnoe_text_row
    STRCPY_TEXT_PRINT   tApnoeMax

	call	TFT_standard_color
	SAFE_2BYTE_COPY max_pressure, lo
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_display_apnoe_last_m_metric
;TFT_display_apnoe_last_max_imperial
	call	convert_mbar_to_feet              	; convert value in lo:hi from mbar to feet
	WIN_MEDIUM	apnoe_last_max_depth_column,apnoe_last_max_depth_row
	output_16
	STRCAT_PRINT ""
	return

TFT_display_apnoe_last_m_metric:
	WIN_MEDIUM	apnoe_last_max_depth_column,apnoe_last_max_depth_row
	bsf		ignore_digit5		; do not display 1cm depth
	output_16dp	d'3'
	STRCAT_PRINT ""
	return

;=============================================================================
	global	TFT_divemins
TFT_divemins:
	movff	divemins+0,lo
	movff	divemins+1,hi
    bcf		leftbind

	btfsc	no_more_divesecs		; Ignore seconds?
	bra     TFT_divemins2           ; Show minutes only

	movlw	.99
	cpfsgt	lo                      ; bigger then 99?
	bra		TFT_divemins1           ; No show mins:secs
	; Yes, remove second display for the rest of the dive and clear seconds
	bsf		no_more_divesecs        ; Set flag
	; Clear rest of seconds
	WIN_BOX_BLACK   divetime_row, warning1_row,divetime_column,.159 ;top, bottom, left, right
    bra     TFT_divemins2           ; Show minutes only

TFT_divemins1:
	WIN_MEDIUM	divetime_column, divetime_row
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font

	WIN_SMALL  divetime_secs_column, divetime_secs_row   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff   divesecs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font
	return

TFT_divemins2:
	WIN_MEDIUM	divetime_minsonly_column, divetime_row
	output_16
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font
    return

;=============================================================================
	global	TFT_display_apnoe_surface
TFT_display_apnoe_surface:
    call    TFT_divemask_color
    WIN_TINY    surface_apnoe_text_column,surface_apnoe_text_row
    STRCPY_TEXT_PRINT   tApnoeSurface

	call	TFT_standard_color
	WIN_MEDIUM	surface_time_apnoe_column, surface_time_apnoe_row
	movff	apnoe_surface_mins,lo
	output_8
    PUTC    ':'
	movff	apnoe_surface_secs,lo
	output_99x
	STRCAT_PRINT ""
	return

	global	TFT_apnoe_clear_surface
TFT_apnoe_clear_surface:
	; Clear Surface timer....
	WIN_BOX_BLACK   surface_apnoe_text_row, .239, surface_apnoe_text_column, .159                 ;top, bottom, left, right
	return

	global	TFT_display_apnoe_descent
TFT_display_apnoe_descent:		; Descent divetime
	movff	apnoe_mins,lo
    clrf    hi
	WIN_MEDIUM	divetime_column, divetime_row
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL  divetime_secs_column, divetime_secs_row   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff	apnoe_secs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font

    call    TFT_divemask_color
    WIN_TINY    total_apnoe_text_column,total_apnoe_text_row
    STRCPY_TEXT_PRINT   tApnoeTotal
	call	TFT_standard_color
	movff	divemins,lo
    clrf    hi
	WIN_MEDIUM	apnoe_total_divetime_column, apnoe_total_divetime_row
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL  apnoe_total_divetime_secs_column, apnoe_total_divetime_secs_row   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff	divesecs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font
	return
	
;=============================================================================
; Writes ostc3 #Serial and Firmware version in splash screen
;
	global	TFT_serial
TFT_serial:		
    WIN_TINY	.0,.239-.14
    STRCPY  "OSTC3 #"                    ; Won't translate that...
    rcall   TFT_cat_serial
    
    STRCAT  " v"
    rcall   TFT_cat_firmware

    ifdef __DEBUG
        movlw   color_grey              ; Write header in blue when
        call    TFT_set_color           ; compiled in DEBUG mode...
        STRCAT_PRINT "DEBUG"    
    else
        WIN_COLOR   color_greenish
        STRCAT_PRINT ""
        call	TFT_standard_color

        movlw	softwareversion_beta    ; =1: Beta, =0: Release
        decfsz	WREG,F
        return                          ; Release version -> Return
        
        call	TFT_warnings_color
        WIN_LEFT    .160-4*9/2          ; Right pad.
        STRCPY_TEXT_PRINT tBeta
    endif
    call	TFT_standard_color
	return

	

;=============================================================================
; For the Information menu: append firmware x.yy version.
    global info_menu_firmware
    extern  tFirmware
info_menu_firmware:
    lfsr    FSR1,tFirmware
    call    strcat_text
    global  TFT_cat_firmware
TFT_cat_firmware:
    movlw	softwareversion_x
    movwf	lo
    bsf		leftbind
    output_8
    PUTC    '.'
    movlw	softwareversion_y
    movwf	lo
    output_99x
    bcf		leftbind
    return

;-----------------------------------------------------------------------------
; For the Information menu: append serial number ostc3#42.
    global  info_menu_serial
    extern  tSerial
info_menu_serial:
    lfsr    FSR1,tSerial
    call    strcat_text
    global  TFT_cat_serial
TFT_cat_serial:
    clrf	EEADRH
    clrf	EEADR                       ; Get Serial number LOW
    call	read_eeprom                 ; read byte
    movff	EEDATA,lo
    incf	EEADR,F                     ; Get Serial number HIGH
    call	read_eeprom                 ; read byte
    movff	EEDATA,hi

    bsf		leftbind
    output_16
    bcf		leftbind
    return

;-----------------------------------------------------------------------------
; For the Information menu: Append total dives
    global  info_menu_total_dives
    extern  tTotalDives
info_menu_total_dives:
    lfsr    FSR1,tTotalDives
    call    strcat_text
TFT_cat_total_dives:
	read_int_eeprom	.2
	movff	EEDATA,lo
	read_int_eeprom	.3
	movff	EEDATA,hi
	bsf		leftbind
	output_16
    bcf		leftbind
    return

; For the Information menu: Append battery voltage
    global  info_menu_battery_volts
    extern  tBatteryV
info_menu_battery_volts:
    lfsr    FSR1,tBatteryV
    call    strcat_text
    movff   batt_voltage+1,hi
    movff   batt_voltage+0,lo
	bsf		leftbind
	output_16dp .2      ; x.xxx
    bcf		leftbind
    PUTC    "V"
    return

;-----------------------------------------------------------------------------
; ppO2 menu
	global	divesets_ppo2_max
    extern  tPPO2Max
    extern  tbar
divesets_ppo2_max:
    lfsr    FSR1,tPPO2Max
    call    strcat_text
	movff	opt_ppO2_max,lo
    movlw   ppo2_warning_high
divesets_ppo2_common:
    movwf   up                  ; Save default value
	clrf	hi
	bsf		leftbind
	output_16dp d'3'
    bcf		leftbind
    lfsr    FSR1,tbar
    call    strcat_text

    movf    up,W                ; Default value
    cpfseq  lo                  ; Current value
    bra     divesets_ppo2_common2 ; Not default, add *
    return                      ; Default, Done.
divesets_ppo2_common2:
    PUTC    "*"
    return                      ; Done.

	global	divesets_ppo2_min
    extern  tPPO2Min
divesets_ppo2_min:
    lfsr    FSR1,tPPO2Min
    call    strcat_text
	movff	opt_ppO2_min,lo
    movlw   ppo2_warning_low
    bra     divesets_ppo2_common

;=============================================================================

    global  TFT_clear_warning_text
TFT_clear_warning_text:
    btfss   divemode                            ; in divemode?
    bra     TFT_clear_warning_text2             ; No, setup for surface mode
    WIN_BOX_BLACK   warning1_row, divemode_customview_row-3, warning1_column, warning_icon_column-3	;top, bottom, left, right
    return
TFT_clear_warning_text2:
    WIN_BOX_BLACK   surf_warning1_row, surf_warning2_row+.24, surf_warning1_column, surf_warning1_column+.76     ;top, bottom, left, right
    return

    global  TFT_clear_warning_text_2nd_row
TFT_clear_warning_text_2nd_row:
    btfss   divemode                            ; in divemode?
    bra     TFT_clear_warning_text_2nd_2        ; No, setup for surface mode
    WIN_BOX_BLACK   warning2_row, divemode_customview_row-3, warning2_column, warning_icon_column-3	;top, bottom, left, right
    return
TFT_clear_warning_text_2nd_2:
    WIN_BOX_BLACK   surf_warning2_row, surf_warning2_row+.24, surf_warning2_column, surf_warning2_column+.76     ;top, bottom, left, right
    return

    global  TFT_fillup_with_spaces
TFT_fillup_with_spaces:         ; Fillup FSR2 with spaces (Total string length in #WREG)
    movwf   lo                  ; save max. string length into lo
    movf    FSR2L,W             ; Get current string length
    subwf   lo,F                ; lo-WREG
    btfsc   STATUS,N            ; longer then #lo already?
    return                      ; Yes, done.
    tstfsz  lo                  ; Zero?
    bra     TFT_fillup_with_spaces2 ; No.
    return                      ; Yes, done.
TFT_fillup_with_spaces2:
    PUTC    " "                 ; Add one space
    decfsz  lo,F                ; All done?
    bra     TFT_fillup_with_spaces2 ; No, loop
    return                      ; Done.

	global	TFT_desaturation_time
TFT_desaturation_time:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	STRCPY	"Desat:"
	movff		desaturation_time+0,lo			; divide by 60...
	movff		desaturation_time+1,hi
	call		convert_time				; converts hi:lo in minutes to hours (hi) and minutes (lo)
	bsf			leftbind
	movf		lo,W
	movff		hi,lo
	movwf		hi							; exchange lo and hi...
	output_8								; Hours
	PUTC        ':'
	movff		hi,lo						; Minutes
	output_99x
	bcf		leftbind
    movlw   surf_warning_length             ; Only use surface string length
    rcall   TFT_fillup_with_spaces          ; Fillup FSR2 with spaces (Total string length in #WREG)
    movlw   .0
    movff   WREG,buffer+11
	STRCAT_PRINT	""
	return

	global	TFT_nofly_time
TFT_nofly_time:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	STRCPY	"NoFly:"
	movff		nofly_time+0,lo			; divide by 60...
	movff		nofly_time+1,hi
	call		convert_time			; converts hi:lo in minutes to hours (hi) and minutes (lo)
	bsf			leftbind
	movf		lo,W
	movff		hi,lo
	movwf		hi							; exchange lo and hi...
	output_8							; Hours
	PUTC        ':'
	movff		hi,lo					; Minutes
	output_99x
	bcf		leftbind
    movlw   surf_warning_length         ; Only use surface string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    movlw   .0
    movff   WREG,buffer+11
	STRCAT_PRINT	""
	return

    global  TFT_warning_agf
TFT_warning_agf:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call	TFT_warnings_color
	STRCPY_TEXT tDiveaGF_active         ; "aGF!"
    movlw   warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
    return

    global  TFT_warning_fallback
TFT_warning_fallback:                ; Show fallback warning
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call	TFT_warnings_color
	STRCPY_TEXT tDiveFallback           ; "Fallback!"
    movlw   warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
    return

    global  TFT_warning_gf
TFT_warning_gf:                         ;GF
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	TFT_color_code		warn_gf		; Color-code Output
	STRCPY  "GF:"
	movff	char_O_gradient_factor,lo		; gradient factor
    bsf     leftbind
	output_8
    PUTC    "%"
    movlw   warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT  ""
    bcf     leftbind
	call	TFT_standard_color
	return

TFT_warning_set_window:                 ; Sets the row and column for the current warning
    ; ignore warning (now)?
    decf    warning_counter,W           ; -1
    bcf     STATUS,C
    rrcf    WREG,W                      ; (warning_counter-1)/2
    cpfseq  warning_page
    retlw   .255                        ; WREG <> 0 -> Warning window not defined

    call	TFT_standard_color

    btfss   divemode                    ; in divemode?
    bra     TFT_warning_set_window3     ; No, setup for surface mode

    btfss   warning_counter,0           ; Toggle with each warning
	bra		TFT_warning_set_window2
	WIN_SMALL	warning1_column,warning1_row
    bcf     second_row_warning          ; =1: The second row contains a warning
	retlw   .0                          ; WREG=0 -> Warning window defined
TFT_warning_set_window2:
	WIN_SMALL	warning2_column,warning2_row
    bsf     second_row_warning          ; =1: The second row contains a warning
	retlw   .0                          ; WREG=0 -> Warning window defined

TFT_warning_set_window3:
    btfss   warning_counter,0           ; Toggle with each warning
	bra		TFT_warning_set_window4
    WIN_SMALL	surf_warning1_column,surf_warning1_row
    bcf     second_row_warning          ; =1: The second row contains a warning
	retlw   .0                          ; WREG=0 -> Warning window defined
TFT_warning_set_window4:
	WIN_SMALL	surf_warning2_column,surf_warning2_row
    bsf     second_row_warning          ; =1: The second row contains a warning
	retlw   .0                          ; WREG=0 -> Warning window defined


	global	TFT_update_batt_percent_divemode
TFT_update_batt_percent_divemode:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    movff   batt_percent,lo         ; Get battery percent
    TFT_color_code		warn_battery; Color-code battery percent
    STRCPY  "Batt:"
	bsf		leftbind
	output_8
	bcf		leftbind
    PUTC    "%"
    movlw   warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT	""
	call	TFT_standard_color
	return


    global  TFT_gf_mask                         ; Setup Mask
TFT_gf_mask:
    ; The mask
    call    TFT_divemask_color
    WIN_TINY  dive_gf_column1,dive_gf_text_row
    STRCPY_TEXT_PRINT tGFactors
    WIN_TINY  dive_gf_column2,dive_gf_text_row
    STRCPY_TEXT_PRINT taGFactors
    WIN_TINY  dive_gf_column3,dive_gf_text_row
    STRCPY_TEXT_PRINT tGFInfo

    ; Show GF (Static)
    call    TFT_disabled_color
    btfss   use_agf
    call	TFT_standard_color

    WIN_STD   dive_gf_column,dive_gf_row
    bsf     leftbind
    movff   opt_GF_low,lo
    output_8
    PUTC    "/"
    movff   opt_GF_high,lo
    output_8
    STRCAT_PRINT   ""
    ; Show aGF (Static)
    call	TFT_standard_color
    TSTOSS  opt_enable_aGF              ; =1: aGF can be selected underwater
    bra     TFT_gf_mask2                ; Show "---" instead

    btfss   use_agf
    call    TFT_disabled_color

    WIN_STD   dive_agf_column,dive_agf_row
    movff   opt_aGF_low,lo
    output_8
    PUTC    "/"
    movff   opt_aGF_high,lo
    output_8
    STRCAT_PRINT   ""
    bcf     leftbind
    call	TFT_standard_color
    return

TFT_gf_mask2:
    WIN_STD   dive_agf_column+.10,dive_agf_row
    STRCPY_PRINT   "---"
    bcf     leftbind
    return

    global  TFT_gf_info                         ; Show GF informations
TFT_gf_info:
    ; Show current GF
	movff	char_O_gradient_factor,lo			; gradient factor absolute (Non-GF model)
	movff	char_I_deco_model,hi
	decfsz	hi,F		; jump over next line if char_I_deco_model == 1
	movff	char_O_relative_gradient_GF,lo		; gradient factor relative (GF model)
    WIN_STD   dive_currentgf_column,dive_currentgf_row
    output_8
    STRCAT_PRINT   "%"
    return

    global  TFT_ead_end_tissues_clock_mask      ; Setup Mask
TFT_ead_end_tissues_clock_mask:
    ; The mask
    call    TFT_divemask_color
	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	bra		TFT_ead_end_tissues_clock_mask2 ; Yes
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	bra		TFT_ead_end_tissues_clock_mask2 ; Yes
    ; Put three columns at HUD positions
    WIN_TINY  dive_custom_hud_column2,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveEAD_END
    WIN_TINY  dive_custom_hud_column3,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveTissues
TFT_ead_end_tissues_clock_mask2:            ; Show only clock
    WIN_TINY  dive_custom_hud_column1,dive_custom_hud_row
    STRCPY_TEXT_PRINT tDiveClock
    call	TFT_standard_color
    return

    global  TFT_ead_end_tissues_clock           ; Show EAD/END, Tissues and clock
TFT_ead_end_tissues_clock:
    ; Update clock and date
    WIN_SMALL   dive_clock_column,dive_clock_row
    call    TFT_clock2                          ; print clock

	btfsc	FLAG_apnoe_mode					; In Apnoe mode?
	return                                  ; Yes, done.
	btfsc	FLAG_gauge_mode					; In Gauge mode?
	return                                  ; Yes, done.

;    WIN_SMALL   dive_endtime_column,dive_endtime_row
;
;    btfss	decostop_active             ; Already in nodeco mode ?
;	bra     TFT_ead_end_tissues_clock2  ; No, overwrite with some spaces
;
;	STRCPY  0x94					; "End of dive" icon
;    movff	hours,WREG
;    mullw   .60
;    movf    mins,W
;    addwf   PRODL
;    movlw   .0
;    addwfc  PRODH
;	movff	PRODL, lo
;	movff	PRODH, hi
;
;    ; Add TTS
;    movff	int_O_ascenttime+0,WREG     ; TTS
;    addwf   lo,F
;	movff	int_O_ascenttime+1,WREG     ; TTS is 16bits
;    addwfc  hi,F
;
;	call	convert_time				; converts hi:lo in minutes to hours (hi) and minutes (lo)
;	movf	hi,W
;	movff	lo,hi
;	movwf	lo							; exchange lo and hi
;	output_99x
;	PUTC    ':'
;	movff	hi,lo
;	output_99x
;	STRCAT_PRINT ""
;    bra     TFT_ead_end_tissues_clock3
;
;TFT_ead_end_tissues_clock2:
;    STRCPY_PRINT "      "
;TFT_ead_end_tissues_clock3:

    ; Show END/EAD
    WIN_SMALL   dive_ead_column,dive_ead_row
    STRCPY_TEXT tEAD                            ; EAD:
    movff   char_O_EAD,lo
    rcall   TFT_end_ead_common                  ; print "lo m" (or ft) and limit to 8 chars
    WIN_SMALL   dive_end_column,dive_end_row
    STRCPY_TEXT tEND                            ; END:
    movff   char_O_END,lo
    rcall   TFT_end_ead_common                  ; print "lo m" (or ft) and limit to 8 chars

    ; Show tissue diagram
    call    TFT_divemask_color
    WIN_TINY    dive_tissue_N2_column,dive_tissue_N2_row
    STRCPY_TEXT_PRINT   tN2
    WIN_TINY    dive_tissue_He_column,dive_tissue_He_row
    STRCPY_TEXT_PRINT   tHe
 	call    deco_calc_desaturation_time         ; calculate desaturation time (and char_O_tissue_N2_saturation and char_O_tissue_He_saturation)
	movlb	b'00000001'                         ; select ram bank 1
    rcall   DISP_tissue_saturation_graph        ; Show char_O_tissue_N2_saturation and char_O_tissue_He_saturation
    return

TFT_end_ead_common:           ; print "lo m" (or ft) and limit to 8 chars
    bsf     leftbind
    TSTOSS  opt_units			; 0=Meters, 1=Feets
	bra		TFT_end_ead_common_metric
;TFT_end_ead_common_imperial:
    ; With lo in m
    movf    lo,W
	mullw	.100							; PRODL:PRODH = mbar/min
	movff	PRODL,lo
	movff	PRODH,hi
	call	convert_mbar_to_feet			; convert value in lo:hi from mbar to feet
    output_16_3
    STRCAT_TEXT     tFeets
    clrf    WREG
    movff   WREG,buffer+.8                  ; limit string length to 8
    bra     TFT_end_ead_common_exit
TFT_end_ead_common_metric:
    output_8
    STRCAT_TEXT     tMeters
TFT_end_ead_common_exit:
    bcf     leftbind
    movlw   .8
    rcall   TFT_fillup_with_spaces          ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT    ""
    return

    global  TFT_surface_tissues
TFT_surface_tissues:             ; Show Tissue diagram in surface mode
    WIN_SMALL    surf_tissue_N2_column,surf_tissue_N2_row
    STRCPY_TEXT_PRINT   tN2
    WIN_SMALL    surf_tissue_He_column,surf_tissue_He_row
    STRCPY_TEXT_PRINT   tHe

 	call    deco_calc_desaturation_time         ; calculate desaturation time (and char_O_tissue_N2_saturation and char_O_tissue_He_saturation)
	movlb	b'00000001'                         ; select ram bank 1

    movlw       color_deepblue
	call		TFT_set_color				; Make this configurable?
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.29,.29
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.37,.37
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.45,.45
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.53,.53
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.61,.61
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.69,.69
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.77,.77
    WIN_FRAME_COLOR16 surf_tissue_diagram_top+.23,surf_tissue_diagram_bottom-.4,.85,.85
    WIN_FRAME_STD   surf_tissue_diagram_top, surf_tissue_diagram_bottom, surf_tissue_diagram_left, surf_tissue_diagram_right    ; outer frame

	movlw	.1
	movff	WREG,win_height             ; row bottom (0-239)
    movlw   surf_tissue_diagram_left+.4      ; Surface mode
	movff	WREG,win_leftx2             ; column left (0-159)
	movlw	surf_tissue_diagram_right-surf_tissue_diagram_left-4  ; Width
	movff   WREG,win_width

    ;---- Draw N2 Tissues
	lfsr	FSR2, char_O_tissue_N2_saturation
	movlw	d'16'
	movwf	wait_temp                   ; 16 tissues
	clrf	waitms_temp                 ; Row offset
surf_tissue_saturation_graph_N2:
    movlw   surf_tissue_diagram_top+.23 ; surface mode
	addwf	waitms_temp,W
	movff	WREG,win_top                ; row top (0-239)
    rcall   surf_tissue_saturation_loop    ; Show one tissue
	decfsz	wait_temp,F
	bra		surf_tissue_saturation_graph_N2

    ;---- Draw He Tissues ----------------------------------------------------
	lfsr	FSR2, char_O_tissue_He_saturation
	movlw	d'16'
	movwf	wait_temp                   ; 16 tissues
	clrf	waitms_temp                 ; Row offset
surf_tissue_saturation_graph_He:
    movlw   surf_tissue_diagram_top+.23+.56    ; surface mode
	addwf	waitms_temp,W
	movff	WREG,win_top                ; row top (0-239)
    rcall   surf_tissue_saturation_loop    ; Show one tissue
	decfsz	wait_temp,F
	bra		surf_tissue_saturation_graph_He
    return

surf_tissue_saturation_loop:
    call	TFT_standard_color
    movlw   .2                          ; row spacing
	addwf   waitms_temp,F
	movf	POSTINC2,W                  ; Get tissue load
    bcf		STATUS,C
	rrcf	WREG                        ; And divide by 2
	movwf   temp1
    movlw   .20
    subwf   temp1,F                     ; Subtract some offset
	movff   win_width,WREG              ; Max width.
	cpfslt	temp1                       ; skip if WREG < win_width
	movwf	temp1
	movff   temp1,win_bargraph
	call	TFT_box
    return

;=============================================================================
; Draw saturation graph, is surface mode or in dive mode.
DISP_tissue_saturation_graph:
    ;---- Draw Frame
    call	TFT_standard_color
    WIN_FRAME_COLOR16   tissue_diagram_top, tissue_diagram_bottom, tissue_diagram_left, .159    ; outer frame

	movlw	.1
	movff	WREG,win_height             ; row bottom (0-239)
    movlw   tissue_diagram_left+.3      ; divemode
	movff	WREG,win_leftx2             ; column left (0-159)
	movlw	.159-tissue_diagram_left-4  ; Width
	movff   WREG,win_width

    ;---- Draw N2 Tissues
	lfsr	FSR2, char_O_tissue_N2_saturation
	movlw	d'16'
	movwf	wait_temp                   ; 16 tissues
	clrf	waitms_temp                 ; Row offset
tissue_saturation_graph_N2:
    movlw   tissue_diagram_top+3        ; divemode
	addwf	waitms_temp,W
	movff	WREG,win_top                ; row top (0-239)
    rcall   tissue_saturation_graph_loop    ; Show one tissue
	decfsz	wait_temp,F
	bra		tissue_saturation_graph_N2

    ;---- Draw He Tissues ----------------------------------------------------
	lfsr	FSR2, char_O_tissue_He_saturation
	movlw	d'16'
	movwf	wait_temp                   ; 16 tissues
	clrf	waitms_temp                 ; Row offset
tissue_saturation_graph_He:
    movlw   tissue_diagram_top+3+.22    ; divemode
	addwf	waitms_temp,W
	movff	WREG,win_top                ; row top (0-239)

    rcall   tissue_saturation_graph_loop    ; Show one tissue

	decfsz	wait_temp,F
	bra		tissue_saturation_graph_He
	return

tissue_saturation_graph_loop:
    call	TFT_standard_color
	incf	waitms_temp,F
	movf	POSTINC2,W
	bcf		STATUS,C
	rrcf	WREG
    bcf		STATUS,C
	rrcf	WREG                        ; And divide by 4
	movwf   temp1
    movlw   .12
    subwf   temp1,F                     ; Subtract some offset
	movff   win_width,WREG              ; Max width.
	cpfslt	temp1                       ; skip if WREG < win_width
	movwf	temp1
	movff   temp1,win_bargraph
	call	TFT_box
    return


	global	TFT_display_cns
TFT_display_cns:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	TFT_color_code		warn_cns		; Color-code CNS output
	STRCPY_TEXT tCNS2                   ; CNS:
	movff	int_O_CNS_fraction+0,lo
    movff	int_O_CNS_fraction+1,hi
	bsf		leftbind
	output_16_3					;Displays only 0...999
	bcf		leftbind
    PUTC    "%"
    movlw   warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces          ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
	call	TFT_standard_color
	return

	global	TFT_display_ppo2
TFT_display_ppo2:                       ; Show ppO2 (ppO2 stored in xC, in mbar!)
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
	TFT_color_code		warn_ppo2		; Color-code output (ppO2 stored in xC)
    STRCPY  "ppO2:"
; Check very high ppO2 manually
	tstfsz	xC+2                        ; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra		TFT_show_ppO2_3             ; Yes, display fixed Value!
	movff	xC+0,lo
	movff	xC+1,hi
	bsf		ignore_digit4
	output_16dp	d'1'
TFT_show_ppO2_2:
    movlw   warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
	return

TFT_show_ppO2_3:
    STRCAT  ">6.6"
	bra		TFT_show_ppO2_2


	global	TFT_LogOffset_Logtitle
TFT_LogOffset_Logtitle:
	STRCPY_TEXT tLogOffset
	PUTC	":"
	call	do_logoffset_common_read	; Offset into lo:hi
	bsf		leftbind
	output_16
	bcf		leftbind
	PUTC	" "
	return			; No "_PRINT" here...
	

	global	adjust_depth_with_salinity
adjust_depth_with_salinity:			; computes salinity setting into lo:hi [mbar]
	btfsc	simulatormode_active	; Do apply salinity in Simulatormode
	return

    global  adjust_depth_with_salinity_log
    movff   opt_salinity,WREG       ; 0-5%
adjust_depth_with_salinity_log: ; computes salinity setting (FROM WREG!) into lo:hi [mbar]
	addlw	d'100'                  ; 1.00kg/l
	movwf	wait_temp
	
	movlw	d'105'					; 105% ?
	cpfslt	wait_temp				; Salinity higher limit
	return							; Out of limit, do not adjust lo:hi
	movlw	d'99'					; 99% ?
	cpfsgt	wait_temp				; Salinity lower limit
	return							; Out of limit, do not adjust lo:hi

	movff	lo,xA+0
	movff	hi,xA+1

	movlw	d'102'					; 0,98bar/10m
	movwf	xB+0
	clrf	xB+1
	call	mult16x16				;xA*xB=xC (lo:hi * 100)
	movff	wait_temp,xB+0			; Salinity
	clrf	xB+1
	call	div32x16  				; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	movff	xC+0,lo
	movff	xC+1,hi					; restore lo and hi with updated value
	return

	global	convert_mbar_to_feet   	; convert value in lo:hi from mbar to feet
convert_mbar_to_feet:              	; convert value in lo:hi from mbar to feet
	movff	lo,xA+0
	movff	hi,xA+1

	movlw	LOW 	d'328'          ; 328feet/100m
	movwf	xB+0
	movlw	HIGH 	d'328'
	movwf	xB+1

	call	mult16x16			    ; xA*xB=xC (lo:hi * 328)

	movlw	d'50'                   ; round up
	addwf	xC+0,F
	movlw	0
	addwfc	xC+1,F
	addwfc	xC+2,F
	addwfc	xC+3,F

    movlw   LOW  .10000
    movwf   xB+0
    movlw   HIGH .10000
    movwf   xB+1

	call	div32x16  			    ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder

	movff	xC+0,lo
	movff	xC+1,hi				    ; restore lo and hi with updated value
	return

	global	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit
convert_celsius_to_fahrenheit:		; convert value in lo:hi from celsius to fahrenheit
	; Does it work with signed temperature? mH
	movff	lo,xA+0
	movff	hi,xA+1

	movlw	d'18'                   ; 1C = 1.8F
	movwf	xB+0
	clrf	xB+1

	call	mult16x16               ;xA*xB=xC (lo:hi * 18)

	movlw	d'10'
	movwf	xB+0
	clrf	xB+1

	call	div32x16                ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder

    movlw	LOW d'320'              ; 0C = 32F
	addwf	xC+0,F
	movlw	HIGH d'320'
	addwfc	xC+1,F

	movff	xC+0,lo
	movff	xC+1,hi                 ; restore lo and hi with updated value
	return

	END