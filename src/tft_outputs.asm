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

#include    "hwos.inc"                  ; Mandatory header
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
#include    "calibrate.inc"

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
	bra			TFT_warnings_color       	; Yes, warn in warning color
; Check if ppO2>3,30bar
	btfsc		xC+1,7
	bra			TFT_warnings_color          ; Yes, warn in warning color

; Check for low ppo2
	movff       xC+0,sub_a+0
	movff       xC+1,sub_a+1
    movff       opt_ppO2_min,WREG
	mullw       d'100'                  ; opt_ppO2_min*100
	movff       PRODL,sub_b+0
	movff       PRODH,sub_b+1
	call        subU16
	btfsc       neg_flag
    bra			TFT_warnings_color      ; too low -> Warning Color!

; Check for high ppo2
	movff		opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
	mullw		d'100'					; opt_ppO2_max*100
	movff		PRODL,sub_b+0
	movff		PRODH,sub_b+1
	call		subU16					;  sub_c = sub_a - sub_b	
	btfss		neg_flag
	bra			TFT_warnings_color      ; too high -> Warning Color!
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
	bra		TFT_warnings_color     	; Set to warning color
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

    TSTOSS  opt_modwarning			; 0=standard, 1=blink
	bra		TFT_color_code_depth_std
;TFT_color_code_depth_blink:
	call	subU16			;  sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		TFT_color_code_depth_warn ; Set to warning color
    call    TFT_color_code_ppo2_depth  ; check depth against MOD
    return

TFT_color_code_depth_std:
	call	subU16			   ; sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		TFT_warnings_color ; Set to warning color
    call    TFT_standard_color
	return

TFT_color_code_ppo2_depth:
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

; Check if ppO2>6,55bar
	tstfsz	xC+2					; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	;bra		TFT_color_code_warn     ; Yes, warn in warning color
    bra     TFT_color_code_depth_warn
; Check if ppO2>3,30bar
	btfsc	xC+1,7
    bra     TFT_color_code_depth_warn

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfss	neg_flag
    bra     TFT_color_code_depth_warn

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_min,WREG		; PPO2 min for Sensors and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfsc	neg_flag
    bra     TFT_color_code_depth_warn
    if dm_offset != 0
        call	TFT_standard_color
    else
        movlw   color_green
    	call	TFT_set_color
    endif
    bcf     blinking_depth_warning      ; reset warning
    return

TFT_color_code_depth_warn:
   	bsf		blinking_depth_warning         ; Set warning
    bra     TFT_warnings_color ; Set to warning color

TFT_color_code_cns:
    movff   int_O_CNS_fraction+1,lo		; copy into bank1
    tstfsz  lo                          ; >255% ?
    bra     TFT_warnings_color         ; Yes
	movff	int_O_CNS_fraction+0,lo
	movlw	color_code_cns_high		; CNS Warn [%]
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_warnings_color		; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_gf:
	movff	char_O_gradient_factor,lo		; gradient factor
	movlw	color_code_gf_warn_high 	; GF Warn [%]
	subwf	lo,W
	btfsc	STATUS,C
	bra		TFT_warnings_color         ; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_ppo2:
; Check if ppO2>6,55bar
	tstfsz	xC+2					; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra		TFT_warnings_color     ; Yes, warn in warning color
; Check if ppO2>3,30bar
	btfsc	xC+1,7
	bra		TFT_warnings_color     ; Yes, warn in warning color

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfss	neg_flag
	bra		TFT_warnings_color     ; Set to warning color

	movff	xC+0,sub_a+0
	movff	xC+1,sub_a+1
	movff	opt_ppO2_min,WREG		; PPO2 min for Sensors and color coding in divemode
	mullw	d'100'
	movff	PRODL,sub_b+0
	movff	PRODH,sub_b+1
	call	subU16			  		; sub_c = sub_a - sub_b
	btfsc	neg_flag
	bra		TFT_warnings_color     ; Set to warning color
	call	TFT_standard_color
	return

TFT_color_code_ppo2_hud:            ; With ppO2 [cbar] in lo
	movff	opt_ppO2_max,WREG		; PPO2 Max for MOD calculation and color coding in divemode
    cpfsgt  lo                      ; lo > opt_ppO2_max?
    bra     TFT_color_code_ppo2_hud1; No
    bra     TFT_warnings_color     ; Yes
TFT_color_code_ppo2_hud1:
	movff	opt_ppO2_min,WREG		; PPO2 min for Sensors and color coding in divemode
    cpfslt  lo                      ; lo < opt_ppO2_min?
    bra     TFT_color_code_ppo2_hud2; No
    bra     TFT_warnings_color     ; Yes
TFT_color_code_ppo2_hud2:
    call	TFT_standard_color
    return

TFT_color_code_battery:             ; With battery percent in lo
    movlw   color_code_battery_low
    cpfsgt  lo                      ; lo < color_code_battery_low ?
    bra     TFT_warnings_color     ; No
    call	TFT_standard_color
    return

; ****************************************************************************

    global  TFT_show_OC_startgas_surface
TFT_show_OC_startgas_surface:           ; Show first gas and "OSTC2-like" active gases
    ; Show first gas
    WIN_SMALL surf_decotype_column+.1,surf_decotype_row+.30
    extern  get_first_gas_to_WREG,gaslist_strcat_gas
    call    get_first_gas_to_WREG       ; Gets first gas (0-4) into WREG
    movwf   PRODL
    call    gaslist_strcat_gas          ; Input: PRODL : gas number (0..4), Output: Text appended into buffer pointed by FSR2.
    STRCAT_PRINT ""
    ; Show boxes
    WIN_TOP		surf_decotype_row+.30+.25
	WIN_LEFT	surf_decotype_boxes_left1+.1
    rcall    TFT_disabled_color
    movff   opt_gas_type+0,hi          ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    tstfsz  hi
    rcall    TFT_standard_color
    STRCPY_PRINT    "1"
    decfsz  hi,F                        ; Type = 1 (First)?
    bra     DISP_active_gas_surfmode3   ; No, skip box
	WIN_FRAME_STD   surf_decotype_boxes_top, surf_decotype_boxes_bottom, surf_decotype_boxes_left1, surf_decotype_boxes_left1+.8    ;top, bottom, left, right
DISP_active_gas_surfmode3:
    rcall    TFT_disabled_color
    movff   opt_gas_type+1,hi         ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    tstfsz  hi
    rcall    TFT_standard_color
	WIN_LEFT	surf_decotype_boxes_left2+.1
    STRCPY_PRINT    "2"
    decfsz  hi,F                        ; Type = 1 (First)?
    bra     DISP_active_gas_surfmode4   ; No, skip box
	WIN_FRAME_STD   surf_decotype_boxes_top, surf_decotype_boxes_bottom, surf_decotype_boxes_left2, surf_decotype_boxes_left2+.8    ;top, bottom, left, right
DISP_active_gas_surfmode4:
    rcall    TFT_disabled_color
    movff   opt_gas_type+2,hi         ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    tstfsz  hi
    rcall    TFT_standard_color
	WIN_LEFT	surf_decotype_boxes_left3+.1
    STRCPY_PRINT    "3"
    decfsz  hi,F                        ; Type = 1 (First)?
    bra     DISP_active_gas_surfmode5       ; No, skip box
    WIN_FRAME_STD   surf_decotype_boxes_top, surf_decotype_boxes_bottom, surf_decotype_boxes_left3, surf_decotype_boxes_left3+.8    ;top, bottom, left, right
DISP_active_gas_surfmode5:
    rcall    TFT_disabled_color
    movff   opt_gas_type+3,hi         ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    tstfsz  hi
    rcall    TFT_standard_color
	WIN_LEFT	surf_decotype_boxes_left4+.1
    STRCPY_PRINT    "4"
    decfsz  hi,F                        ; Type = 1 (First)?
    bra     DISP_active_gas_surfmode6       ; No, skip box
    WIN_FRAME_STD   surf_decotype_boxes_top, surf_decotype_boxes_bottom, surf_decotype_boxes_left4, surf_decotype_boxes_left4+.8    ;top, bottom, left, right
DISP_active_gas_surfmode6:
    call    TFT_disabled_color
    movff   opt_gas_type+4,hi         ; 0=Disabled, 1=First, 2=Travel, 3=Deco
    tstfsz  hi
    rcall    TFT_standard_color
	WIN_LEFT	surf_decotype_boxes_left5+.1
    STRCPY_PRINT    "5"
    rcall    TFT_standard_color          ; Reset color
    decfsz  hi,F                        ; Type = 1 (First)?
    return                                  ; no, Done.
    WIN_FRAME_STD   surf_decotype_boxes_top, surf_decotype_boxes_bottom, surf_decotype_boxes_left5, surf_decotype_boxes_left5+.8    ;top, bottom, left, right
    return                                  ; Done.

    global  TFT_show_color_schemes
TFT_show_color_schemes:         ; update the color schemes
    bsf     divemode            ; put in divemode
    call    TFT_divemask_color
    WIN_TINY  .12,.40
    STRCAT_TEXT_PRINT	tDepth
    WIN_TINY  .62,.40
    STRCAT_TEXT_PRINT	tMaxDepth
    WIN_TINY  .122,.40
    STRCAT_TEXT_PRINT	tDivetime

    ; Show some demo screen

    ; Depth demo
    call	TFT_standard_color
	WIN_MEDIUM	.3,.54
    movlw   LOW     .5172
    movwf   lo
    movlw   HIGH    .5172
    movwf   hi
	bsf		leftbind
	bsf		ignore_digit4
	output_16						; Full meters in Big font
	bcf		leftbind
	STRCAT_PRINT ""					; Display full meters
    WIN_SMALL	.25,.66
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
    WIN_MEDIUM	.64,.54
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
	WIN_SMALL	.87,.66
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
	WIN_MEDIUM	.103, .54
	output_16_3                     ; limit to 999 and display only (0-999)
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL  .139, .66   		; left position for two sec figures
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
    if dm_offset != 0
        call    TFT_divemask_color
        WIN_TINY            dm_mask_depth_column,dm_mask_depth_row
        STRCAT_TEXT_PRINT   tDepth
        WIN_TINY            dm_mask_maxdepth_column,dm_mask_maxdepth_row
        STRCAT_TEXT_PRINT   tMaxDepth
        WIN_TINY            dm_mask_divetime_column,dm_mask_divetime_row
        STRCAT_TEXT_PRINT   tDivetime
    endif

    if dm_offset == 0
        movlw       color_dark_red
        call        TFT_set_color
        WIN_FRAME_COLOR16 dm_velobar_top, dm_velobar_bot, dm_velobar_lft, dm_velobar_rgt ;top, bottom, left, right
        WIN_FRAME_COLOR16 dm_sep_1_2_row, dm_sep_1_2_row, .0, .159 ;top, bottom, left, right
        WIN_FRAME_COLOR16 dm_sep_2_3_row, dm_sep_2_3_row, .0, .159 ;top, bottom, left, right
        WIN_FRAME_COLOR16 dm_warning_row-.1, dm_warning_row-.1, dm_warning_column, .159 ;top, bottom, left, right
        call    TFT_draw_gassep_line
    endif

    call	TFT_standard_color
    return

	global	TFT_clear_customview_divemode
TFT_clear_customview_divemode:
    WIN_BOX_BLACK    dm_customview_row, dm_customview_bot, dm_customview_column, dm_customview_rgt	; top, bottom, left, right
	return

    global	TFT_draw_gassep_line
TFT_draw_gassep_line:
    btfsc	FLAG_apnoe_mode				; Ignore in Apnoe mode
    return
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
    if dm_offset == 0
        movlw       color_dark_red
        call        TFT_set_color
        WIN_FRAME_COLOR16   dm_gassep_row, dm_gassep_bot, dm_gassep_column, dm_gassep_column
    endif
    call	TFT_standard_color
    return

;=========================================================================

	global	TFT_display_velocity
TFT_display_velocity:						; With divA+0 = m/min
    ; Input is:
    ;   neg_flag_velocity: ascend=1, descend=0
    ;   divA+0:            rate in m/min
    ; init flags used to store warning/attention
    bcf     velocity_warn
    bcf     velocity_attn
    ; check if old/new ascend logic is used
    TSTOSS  opt_vsitextv2       			; 0=standard, 1=dynamic
    bra     TFT_velocity_std                ; static ascend rate limit
    ; initialize the multiplier/offset values, also required for the
    ; below-the-treshold bar
    movlw   .7
    movwf   xC+0
    movlw   .6
    movwf   xC+1
    movlw   .1
    movwf   xC+2
    movlw   .0
    movwf   xC+3
    ; check if velocity is below the treshold level
    bcf     STATUS,C
	movlw	velocity_display_threshold_2
	subwf	divA+0,W
	btfss	STATUS,C
	bra		TFT_velocity_ntr                ; ascend/descend rare is below limit
	bsf		display_velocity

    ; use a depth-dependent ascent rate warning
    ; depth(ft):     <20 >20 >40 >60 >75 >88 >101 >115 >128 >144 >164
    ; speed(ft/min):  23  26  29  33  36  43   49   56   59   62   66
    ; depth(m):      <=6  >6 >12 >18 >23 >27  >31  >35  >39  >44  >50
    ; speed(m/min):    7   8   9  10  11  13   15   17   18   19   20 (warning)
	; speed(m/min):    5   6   7   8   8  10   12   13   14   15   15 (attention)
    ;
    ; use different multipliers and offsets for the different ascend limits for
    ;   a smoother bar
    ; w-multip         7   6   5   5   4   3    3    2    2    2    2
    ; a-multip         6   5   4   3   3   3    2    2    2    2    2
    ; w-offset         1   2   5   0   6  11    5   16   14   12   10
    ; a-offset         0   0   2   6   6   0    6    4    2    0    0

    ; check if descending: no warning color if descending
    call	TFT_standard_color
	btfss	neg_flag_velocity                   ; Ignore for descent!
	bra		TFT_velocity_disp
    ; get the actual depth
    SAFE_2BYTE_COPY rel_pressure, lo			; get the actual depth
	call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
	call 	convert_mbar_to_feet				; get depth in feet
	; store current depth (in feet) into sub_a
	movff	lo,sub_a+0
	movff	hi,sub_a+1
	; xA will be used to store the warning/attention limits passed to the verification
	clrf	xA+0
	clrf	xA+1

;TFT_display_velocity_asc_164:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'164'
	movwf	sub_b+0
	movlw	.20                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .15                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .2
    movwf   xC+0
    movlw   .2
    movwf   xC+1
    movlw   .10
    movwf   xC+2
    movlw   .0
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_144:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'144'
	movwf	sub_b+0
	movlw	.19                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .15                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .2
    movwf   xC+0
    movlw   .2
    movwf   xC+1
    movlw   .12
    movwf   xC+2
    movlw   .0
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_128:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'128'
	movwf	sub_b+0
	movlw	.18                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .14                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .2
    movwf   xC+0
    movlw   .2
    movwf   xC+1
    movlw   .14
    movwf   xC+2
    movlw   .2
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_115:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'115'
	movwf	sub_b+0
	movlw	.17                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .13                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .2
    movwf   xC+0
    movlw   .2
    movwf   xC+1
    movlw   .16
    movwf   xC+2
    movlw   .4
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_101:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'101'
	movwf	sub_b+0
	movlw	.15                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .12                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .3
    movwf   xC+0
    movlw   .2
    movwf   xC+1
    movlw   .5
    movwf   xC+2
    movlw   .6
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_88:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'88'
	movwf	sub_b+0
	movlw	.13                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .10                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .3
    movwf   xC+0
    movlw   .3
    movwf   xC+1
    movlw   .11
    movwf   xC+2
    movlw   .0
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_75:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'75'
	movwf	sub_b+0
	movlw	.11                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .8                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .4
    movwf   xC+0
    movlw   .3
    movwf   xC+1
    movlw   .6
    movwf   xC+2
    movlw   .6
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_60:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'60'
	movwf	sub_b+0
	movlw	.10                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .8                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .5
    movwf   xC+0
    movlw   .3
    movwf   xC+1
    movlw   .0
    movwf   xC+2
    movlw   .6
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_40:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'40'
	movwf	sub_b+0
	movlw	.9                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .7                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .5
    movwf   xC+0
    movlw   .4
    movwf   xC+1
    movlw   .5
    movwf   xC+2
    movlw   .2
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_20:
	; store segment limit into sub_b
	clrf	sub_b+1
	movlw	LOW 	d'20'
	movwf	sub_b+0
	movlw	.8                 	; store the warn limit to xA+0
	movwf	xA+0
    movlw   .6                     ; store the attn limit to xA+1
    movwf   xA+1
    ; graphical position helpers
    movlw   .6
    movwf   xC+0
    movlw   .5
    movwf   xC+1
    movlw   .2
    movwf   xC+2
    movlw   .0
    movwf   xC+3
	; check if current depth > segment limit
	call	subU16								;  sub_c = sub_a - sub_b;  depth - sLimit
	btfss	neg_flag							;  depth lower than segment limit? sLimit>depth?
    bra     TFT_velocity_check		            ;  no, depth>=sLimit, do the check for this segment

;TFT_display_velocity_asc_n6:
	; no more steps, check the smallest rate
	; store the warn limit to xA : <20ft=23; <6m=7
	movlw	.7
	movwf	xA+0
    movlw   .5
    movwf   xA+1
    ; graphical position helpers
    movlw   .7
    movwf   xC+0
    movlw   .6
    movwf   xC+1
    movlw   .1
    movwf   xC+2
    movlw   .0
    movwf   xC+3
    ;bra     TFT_velocity_check                  ;  depth < 20ft / 6m

TFT_velocity_check:
	; move current ascent rate to lo
	clrf    hi
	movff	divA+0,lo
	; Velocity warn [m/min] - we receive it from xA+0
    bcf     STATUS,C
    movff	xA+0,WREG
	; compare the values
	subwf	lo,W                            ; subtract W from lo,
	btfsc	STATUS,C                        ; Check if C (carry flag) is set. Cleared if the larger number is subtracted from smaller one
	bra		TFT_velocity_warn    ; Skip if no carry flag otherwise set to warning color
    ; not eq or gt warning trashold, lets check if it reach the attention level
	; Velocity attn [m/min] - we receive it from xA+1
    bcf     STATUS,C
    movff	xA+1,WREG
	; compare the values
	subwf	lo,W                            ; subtract W from lo,
	btfsc	STATUS,C                        ; Check if C (carry flag) is set. Cleared if the larger number is subtracted from smaller one
	bra		TFT_velocity_attn    ; Skip if no carry flag otherwise set to warning color
    ;bra     TFT_velocity_def

TFT_velocity_def:
	call	TFT_standard_color
    bra     TFT_velocity_disp

TFT_velocity_warn:
	call	TFT_warnings_color             ; Set to warning color
    bsf     win_invert
    bsf     velocity_warn
    bra     TFT_velocity_disp

TFT_velocity_attn:
	call	TFT_attention_color            ; Set to attention color
    bsf     velocity_attn
    bra     TFT_velocity_disp

TFT_velocity_std:
    ; initialize the multiplier/offset values for the graphical bar
    movlw   .5
    movwf   xC+0
    movlw   .3
    movwf   xC+1
    movlw   .0
    movwf   xC+2
    movlw   .6
    movwf   xC+3

	bcf     STATUS,C
    movlw	velocity_display_threshold_1	; lowest threshold for display vertical velocity
	subwf	divA+0,W
	btfss	STATUS,C
	bra		TFT_velocity_ntr                ; under treshold, clear text and display VSIbar
	bsf		display_velocity

	call	TFT_standard_color
	btfss	neg_flag_velocity       ; Ignore for descent!
	bra		TFT_velocity_disp		; Skip check!
    bcf     STATUS,C
	movff	divA+0,lo
	movlw	color_code_velocity_warn_high	; Velocity warn [m/min]
	subwf	lo,W
	btfsc	STATUS,C
    bra     TFT_velocity_std_warn
    bcf     STATUS,C
	movff	divA+0,lo
	movlw	color_code_velocity_attn_high	; Velocity attn [m/min]
	subwf	lo,W
	btfsc	STATUS,C
    bra     TFT_velocity_std_attn
    bra     TFT_velocity_disp

TFT_velocity_std_warn:
	call	TFT_warnings_color             ; Set to warning color
    bsf     velocity_warn
    bra     TFT_velocity_disp

TFT_velocity_std_attn:
	call	TFT_attention_color            ; Set to attention color
    bsf     velocity_attn
    ;bra     TFT_velocity_disp

TFT_velocity_disp:
    WIN_SMALL	dm_velocity_text_column, dm_velocity_text_row
    TSTOSS  opt_units			; 0=Meters, 1=Feets
	bra		TFT_velocity_metric
;TFT_velocity_imperial:
	movff	divA+0,WREG						; divA+0 = m/min
	mullw	.100							; PRODL:PRODH = mbar/min
	movff	PRODL,lo
	movff	PRODH,hi
	call	convert_mbar_to_feet			; convert value in lo:hi from mbar to feet
	movlw	'-'
	btfsc	neg_flag_velocity
	movlw	'+'
	movwf	POSTINC2
	bsf		leftbind
	output_16
	bcf		leftbind
	STRCAT_TEXT_PRINT  tVelImperial			; Unit switch
    bcf     win_invert
    bcf     neg_flag
    call    TFT_velocity_VSIbar
	call	TFT_standard_color
    return

TFT_velocity_metric:
	movff	divA+0,lo						; divA+0 = m/min
	movlw	'-'
	btfsc	neg_flag_velocity
	movlw	'+'
	movwf	POSTINC2
	output_99
	STRCAT_TEXT_PRINT  tVelMetric			; Unit switch
    bcf     win_invert
    bcf     neg_flag
    call    TFT_velocity_VSIbar
	call	TFT_standard_color
    return

TFT_velocity_VSIbar:
    ; use another logic when descending
    btfss   neg_flag_velocity
    bra     TFT_velocity_VSIbar_desc
    call    TFT_velocity_VSIbar_desc_clr

    TSTOSS  opt_vsigraph			; 0=skip, 1=draw
    return

    btfsc   velocity_warn
    bra     TFT_velocity_VSIbar_warn
    ; if all ok or attention, use attn's values
    movff   xC+1,sub_b+0    ; multiplier
    movff   xC+3,sub_b+1    ; offset
    bra     TFT_velocity_VSIbar_com

TFT_velocity_VSIbar_warn:
    ; save multiplier and offset out from the xC
    movff   xC+0,sub_b+0    ; multiplier
    movff   xC+2,sub_b+1    ; offset
    ;bra     TFT_velocity_VSIbar_com

TFT_velocity_VSIbar_com:
    clrf    divB

    movlw   .0
    cpfsgt  divA+0
    bra     TFT_velocity_VSIbar_clr

    ; multiply
    movff   divA+0,xA+0
    clrf    xA+1
    movff   sub_b+0,xB+0
    clrf    xB+1
    call    mult16x16               ; xA*xB=xC
    movlw   .1
    cpfslt  xC+3
    bra     TFT_velocity_VSIbar_max
    movlw   .1
    cpfslt  xC+2
    bra     TFT_velocity_VSIbar_max
    movlw   .1
    cpfslt  xC+1
    bra     TFT_velocity_VSIbar_max
    movlw   .60
    cpfslt  xC+0
    bra     TFT_velocity_VSIbar_max
    ; add offset
    bcf     STATUS,C
    movff   sub_b+1,WREG
    addwf   xC+0,1
    btfsc   STATUS,C
    bra     TFT_velocity_VSIbar_max
    ; check if out-of-range
    movff   xC+0,divB
    movlw   .60
    cpfsgt  divB
    bra     TFT_velocity_VSIbar_draw

TFT_velocity_VSIbar_max:
    movlw   .60
    movff   WREG,divB

TFT_velocity_VSIbar_draw:
    ; calculate top&height for the bar and mask
    ; 1. Bar:  top=(bar_top+60-divB); height=divB
    movlw   dm_velobar_top+.1
    movff   WREG,sub_a+0              ; !!!!!!  bar position must fit into lo !!
    movlw   .60
    addwf   sub_a+0,1
    clrf    sub_a+1
    movff   divB,sub_b+0
    clrf    sub_b+1
    call    subU16

    movlw       color_white
    WIN_BOX_COLOR dm_velobar_top+.60, dm_velobar_top+.63, dm_velobar_lft+.1, dm_velobar_rgt-.1 ;top, bottom, left, right
   
    movff   sub_c+0,win_top
    movff   divB,win_height
    movlw   dm_velobar_width-.2
    movff   WREG,win_width
    movff   WREG,win_bargraph
    movlw   dm_velobar_lft+.2
    movff   WREG,win_leftx2
    movlw   color_green
    call    TFT_set_color
    btfsc   velocity_attn
    call    TFT_attention_color
    btfsc   velocity_warn
    call    TFT_warnings_color
    call    TFT_box

    ;clear the rest
    movlw   .60
    cpfslt  divB
    return  ; divB !< 60 - the graph uses the full bar, no need to clear

    ; 2. Mask: top=bar_top; height=60-divB
    movlw   .60
    movff   WREG,sub_a+0
    clrf    sub_a+1
    movff   divB,sub_b+0
    clrf    sub_b+1
    call    subU16              ; sub_c = sub_a - sub_b

    movlw   dm_velobar_top+.1
    movff   WREG,win_top
    movff   sub_c+0,win_height
    movlw   dm_velobar_width
    movff   WREG,win_width
    movff   WREG,win_bargraph
    movlw   dm_velobar_lft+.1
    movff   WREG,win_leftx2
    movlw   color_black
    call    TFT_set_color
    call    TFT_box
    return

TFT_velocity_VSIbar_desc:
    ; clear the ascend part of the bar
    call    TFT_velocity_VSIbar_clr

    TSTOSS  opt_vsigraph			; 0=skip, 1=draw
    return

    ; divA+0=0 is descend, clear everything if it's actually zero
    movlw   .0
    cpfsgt  divA+0
    bra     TFT_velocity_VSIbar_desc_clr

    clrf    divB
    ; Desc uses a single multiplier/offset value: *1 / +3
    movlw   .1
    movff   WREG,sub_b+0    ; multiplier
    movlw   .3
    movff   WREG,sub_b+1    ; offset
    ; multiply
    movff   divA+0,xA+0
    clrf    xA+1
    movff   sub_b+0,xB+0
    clrf    xB+1
    call    mult16x16               ; xA*xB=xC
    movlw   .1
    cpfslt  xC+3
    bra     TFT_velocity_VSIbar_desc_max
    movlw   .1
    cpfslt  xC+2
    bra     TFT_velocity_VSIbar_desc_max
    movlw   .1
    cpfslt  xC+1
    bra     TFT_velocity_VSIbar_desc_max
    movlw   .22
    cpfslt  xC+1
    bra     TFT_velocity_VSIbar_desc_max
    ; add offset
    bcf     STATUS,C
    movff   sub_b+1,WREG
    addwf   xC+0,1
    btfsc   STATUS,C
    bra     TFT_velocity_VSIbar_desc_max
    ; check if out-of-range
    movff   xC+0,divB
    movlw   .22
    cpfsgt  divB
    bra     TFT_velocity_VSIbar_desc_draw

TFT_velocity_VSIbar_desc_max:
    movlw   .22
    movff   WREG,divB

TFT_velocity_VSIbar_desc_draw:
    ; calculate top&height for the bar and mask
    ; 1. Bar:  top=(bar_top+63); height=divB
    movlw   dm_velobar_top+.1
    movff   WREG,sub_a+0
    movlw   .62
    addwf   sub_a+0,1

    movlw       color_white
    WIN_BOX_COLOR dm_velobar_top+.60, dm_velobar_top+.63, dm_velobar_lft+.1, dm_velobar_rgt-.1 ;top, bottom, left, right

    movff   sub_a+0,win_top
    movff   divB,win_height
    movlw   dm_velobar_width-.2
    movff   WREG,win_width
    movff   WREG,win_bargraph
    movlw   dm_velobar_lft+.2
    movff   WREG,win_leftx2
    movlw   color_green
    call    TFT_set_color
    call    TFT_box

    ;clear the rest
    movlw   .22
    cpfslt  divB
    return  ; divB !< 22 - the graph uses the full bar, no need to clear

    ; 2. Mask: top=(bar_top+63+divB); height=(23-divB)
    movlw   .24
    movff   WREG,sub_a+0
    clrf    sub_a+1
    movff   divB,sub_b+0
    clrf    sub_b+1
    call    subU16              ; sub_c = sub_a - sub_b

    movlw   dm_velobar_top
    movff   WREG,sub_a+0
    movlw   .61
    addwf   sub_a+0,1
    movff   divB,WREG
    addwf   sub_a+0,1

    movff   sub_a+0,win_top
    movff   sub_c+0,win_height
    movlw   dm_velobar_width
    movff   WREG,win_width
    movff   WREG,win_bargraph
    movlw   dm_velobar_lft+.1
    movff   WREG,win_leftx2
    movlw   color_black
    call    TFT_set_color
    call    TFT_box
    return

TFT_velocity_VSIbar_clr: ; clears the ascend part of the bar
    WIN_BOX_BLACK   dm_velobar_top+.1,dm_velobar_top+.63,dm_velobar_lft+.1,dm_velobar_rgt-.1
    if dm_offset == 0
        movlw       color_dark_red
        WIN_BOX_COLOR dm_velobar_top+.60, dm_velobar_top+.63, dm_velobar_lft+.1, dm_velobar_rgt-.1 ;top, bottom, left, right
    endif
    return

TFT_velocity_VSIbar_desc_clr: ; clears the descend part of the bar
    WIN_BOX_BLACK   dm_velobar_top+.61,dm_velobar_bot-.1,dm_velobar_lft+.1,dm_velobar_rgt-.1
    if dm_offset == 0
        movlw       color_dark_red
        WIN_BOX_COLOR dm_velobar_top+.60, dm_velobar_top+.63, dm_velobar_lft+.1, dm_velobar_rgt-.1 ;top, bottom, left, right
    endif
    return

TFT_velocity_ntr:   ; velocity under treshold
    call    TFT_velocity_clear
    ; use another logic when descending
    btfss   neg_flag_velocity
    bra     TFT_velocity_VSIbar_desc
    bra     TFT_velocity_VSIbar

	global	TFT_velocity_clear
TFT_velocity_clear:
	btfss	display_velocity			; Velocity was not displayed, do not delete
	return
	bcf		display_velocity			; Velocity was displayed, delete velocity now
	; Clear Text
	WIN_BOX_BLACK   dm_velocity_text_row, dm_velocity_text_bot, dm_velocity_text_column, dm_velocity_text_rgt	; top, bottom, left, right
	return

;=========================================================================

    global  TFT_clear_decoarea
TFT_clear_decoarea:
    WIN_BOX_BLACK   dm_decostop_1st_stop_row, .239, dm_decostop_1st_stop_column, .159	; top, bottom, left, right
	return

    global  TFT_clear_divemode_menu
TFT_clear_divemode_menu:
    if dm_offset != 0
        WIN_BOX_BLACK   dm_menu_row,   dm_menu_lower, dm_menu_left,  dm_menu_right	; top, bottom, left, right
    else
        WIN_BOX_BLACK   dm_3rdrow_top, dm_3rdrow_bot, dm_3rdrow_lft, dm_3rdrow_rgt	; top, bottom, left, right
    endif
	return

	global	TFT_display_ndl_mask
TFT_display_ndl_mask:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	rcall	TFT_clear_decoarea			; Clear Dekostop and Dekosum
    if dm_offset != 0
        call    TFT_divemask_color
    else
        call    TFT_attention_color
    endif
   	WIN_STD 	dm_ndl_text_column, dm_ndl_text_row
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
	WIN_MEDIUM  dm_tts_value_column, dm_tts_value_row
	output_16_3					;Displays only 0...999
	STRCAT_PRINT "'"
	return

	global	TFT_display_ndl
TFT_display_ndl:
    btfsc   divemode_menu               ; Is the dive mode menu shown?
    return                              ; Yes, return
	WIN_MEDIUM	dm_ndl_value_column, dm_ndl_value_row
	call	TFT_standard_color
	movff	char_O_nullzeit,lo		; Get NDL from C-code
	output_8
	STRCAT_PRINT "'"
	return

	global	TFT_divemode_warning
TFT_divemode_warning:
    bsf     dive_warning_displayed              ; =1: The warning sign is shown
    WIN_TOP  	dm_warning_icon_row
	WIN_LEFT 	dm_warning_icon_column
    TFT_WRITE_PROM_IMAGE dive_warning2_block 	; Show Warning icon
    return

	global	TFT_divemode_warning_clear
TFT_divemode_warning_clear:
    btfss   dive_warning_displayed              ; =1: The warning sign is shown
    return
    bcf     dive_warning_displayed              ; clear only once
	WIN_BOX_BLACK   dm_warning_icon_row, dm_warning_icon_bot, dm_warning_icon_column, dm_warning_icon_rgt  ; top, bottom, left, right
	return

	global	TFT_display_deko_mask
TFT_display_deko_mask:
	rcall		TFT_clear_decoarea
   	WIN_STD 	dm_tts_text_column, dm_tts_text_row
    if dm_offset != 0
        call    TFT_divemask_color
    else
        call    TFT_attention_color
    endif
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
	WIN_MEDIUM	dm_decostop_1st_stop_column, dm_decostop_1st_stop_row
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
    WIN_TINY    dm_custom_decoplan_title_column, dm_custom_decoplan_title_row
    STRCPY_TEXT_PRINT tDiveDecoplan
	call	TFT_standard_color

	movff	char_O_deco_depth+1,lo
	tstfsz	lo							; Show another stop?
	bra		TFT_display_deko2			; Yes
	; No, clear output and return
	call	TFT_standard_color
	WIN_SMALL	dm_cust_dstop_4th_stop_column,dm_cust_dstop_4th_stop_row
	STRCPY_PRINT "  ---  "
	WIN_BOX_BLACK   dm_cust_dstop_2nd_stop_row, dm_customview_bot, dm_cust_dstop_2nd_stop_column, dm_cust_dstop_4th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   dm_cust_dstop_5th_stop_row, dm_customview_bot, dm_cust_dstop_5th_stop_column, dm_cust_dstop_6th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   dm_cust_dstop_6th_stop_row, dm_customview_bot, dm_cust_dstop_6th_stop_column, .159	; top, bottom, left, right
    return
TFT_display_deko2:
	WIN_SMALL	dm_cust_dstop_2nd_stop_column, dm_cust_dstop_2nd_stop_row
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
	WIN_BOX_BLACK   dm_cust_dstop_3rd_stop_row, dm_customview_bot, dm_cust_dstop_2nd_stop_column, dm_cust_dstop_4th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   dm_cust_dstop_4th_stop_row, dm_customview_bot, dm_cust_dstop_4th_stop_column, .159	; top, bottom, left, right
	return

TFT_display_deko3:
	WIN_SMALL	dm_cust_dstop_3rd_stop_column, dm_cust_dstop_3rd_stop_row
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
	WIN_BOX_BLACK   dm_cust_dstop_4th_stop_row, dm_customview_bot, dm_cust_dstop_4th_stop_column, .159  ; top, bottom, left, right
	return								; Done.

TFT_display_deko4:
	WIN_SMALL	dm_cust_dstop_4th_stop_column, dm_cust_dstop_4th_stop_row
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
	WIN_BOX_BLACK   dm_cust_dstop_5th_stop_row, dm_customview_bot, dm_cust_dstop_5th_stop_column, dm_cust_dstop_6th_stop_column	; top, bottom, left, right
	WIN_BOX_BLACK   dm_cust_dstop_6th_stop_row, dm_customview_bot, dm_cust_dstop_6th_stop_column, .159                     ; top, bottom, left, right
	return								; Done.

TFT_display_deko5:
	WIN_SMALL	dm_cust_dstop_5th_stop_column, dm_cust_dstop_5th_stop_row
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
	WIN_BOX_BLACK   dm_cust_dstop_6th_stop_row, dm_customview_bot, dm_cust_dstop_6th_stop_column, .159        ; top, bottom, left, right
	return								; Done.
TFT_display_deko6:
	WIN_SMALL	dm_cust_dstop_6th_stop_column, dm_cust_dstop_6th_stop_row
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
	WIN_BOX_BLACK   dm_cust_dstop_7th_stop_row, dm_customview_bot, dm_cust_dstop_7th_stop_column, .159     ; top, bottom, left, right
	return								; Done.
TFT_display_deko7:
	WIN_SMALL	dm_cust_dstop_7th_stop_column, dm_cust_dstop_7th_stop_row
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
    WIN_BOX_BLACK   dm_safetystop_row, dm_safetystop_bot, dm_safetystop_text_column, .159	; top, bottom, left, right
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
    ;btfsc   menuview
    ;bra     TFT_show_safety_stop3           ; No room when menuview=1...
    if dm_offset != 0
        call    TFT_divemask_color
    else
        call    TFT_standard_color
    endif
    WIN_STD  dm_safetystop_text_column, dm_safetystop_text_row
    STRCPY_TEXT_PRINT tDiveSafetyStop
TFT_show_safety_stop3:
	call    TFT_attention_color            ; show in yellow
    WIN_MEDIUM	dm_safetystop_column, dm_safetystop_row
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
	call	TFT_standard_color
	return

    global  TFT_mask_avr_stopwatch             ; Show mask for average depth and stopwatch
TFT_mask_avr_stopwatch:
    ; The mask
    call    TFT_divemask_color
    WIN_TINY          dm_custom_avr_stop_title_column1,dm_custom_avr_stop_title_row
    STRCPY_TEXT_PRINT tDiveTotalAvr
    WIN_TINY          dm_custom_avr_stop_title_column2,dm_custom_avr_stop_title_row
    STRCPY_TEXT_PRINT tDiveStopwatch
    WIN_TINY          dm_custom_avr_stop_title_column3,dm_custom_avr_stop_title_row
    STRCPY_TEXT_PRINT tDiveStopAvr
    call	TFT_standard_color
    return

    global  TFT_dyn_gaslist
TFT_dyn_gaslist:                            ; Show the dynamic gaslist
    ; The mask
    call    TFT_divemask_color
    WIN_TINY    dm_custom_dyn_gas_mask_column,dm_custom_dyn_gas_mask_row
    STRCPY_TEXT_PRINT tGaslist
;    call	TFT_standard_color

    WIN_SMALL   dm_custom_dyn_gas_column1,dm_custom_dyn_gas_row1
    movlw   .1
    movwf   uart1_temp
    bsf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL   dm_custom_dyn_gas_column1,dm_custom_dyn_gas_row2
    incf    uart1_temp,F     ; +1
    movf    uart1_temp,W     ; into W
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL   dm_custom_dyn_gas_column2,dm_custom_dyn_gas_row1
    incf    uart1_temp,F     ; +1
    movf    uart1_temp,W     ; into W
    rcall   TFT_dyn_gaslist_common
    WIN_SMALL   dm_custom_dyn_gas_column2,dm_custom_dyn_gas_row2
    incf    uart1_temp,F     ; +1
    movf    uart1_temp,W     ; into W
    rcall   TFT_dyn_gaslist_common
    call	TFT_standard_color
    return

TFT_dyn_gaslist_common:
    cpfseq  active_gas  ;1-5
    bra     TFT_dyn_gaslist_common2
    incf    uart1_temp,F     ; +1
TFT_dyn_gaslist_common2:
    movff   uart1_temp,lo    ; gas number 1-5
    movff   uart1_temp,PRODL
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
    WIN_MEDIUM  dm_custom_avr_stop_column2,dm_custom_avr_stop_row
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
    WIN_MEDIUM  dm_custom_avr_stop_column1,dm_custom_avr_stop_row
    bsf     leftbind
    output_16                       ; yxz
    STRCAT_PRINT " "
    ; Stopped average depth
    movff   avr_rel_pressure+0,lo
    movff   avr_rel_pressure+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    call	convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
    WIN_MEDIUM  dm_custom_avr_stop_column3,dm_custom_avr_stop_row
    output_16                       ; yxz
    bcf     leftbind
    STRCAT_PRINT " "
    return

TFT_update_avr_stopwatch_metric:
    ; Non-resettable average depth
    movff   avr_rel_pressure_total+0,lo
    movff   avr_rel_pressure_total+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    WIN_MEDIUM  dm_custom_avr_stop_column1,dm_custom_avr_stop_row
    bsf     ignore_digit5         ; no cm
    output_16dp  .3               ; yxz.a
    STRCAT_PRINT " "
    ; Stopped average depth
    movff   avr_rel_pressure+0,lo
    movff   avr_rel_pressure+1,hi
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    WIN_MEDIUM  dm_custom_avr_stop_column3,dm_custom_avr_stop_row
    bsf     ignore_digit5         ; no cm
    output_16dp  .3               ; yxz.a
    bcf     leftbind
    bcf     ignore_digit5
    STRCAT_PRINT " "
    return

    global  TFT_ceiling_mask                        ; The ceiling mask
TFT_ceiling_mask:
    call    TFT_divemask_color
    WIN_TINY  dm_custom_ceiling_text_column,dm_custom_ceiling_text_row
    STRCPY_TEXT_PRINT tCeiling
    call	TFT_standard_color
    return

    global  TFT_ceiling                             ; Ceiling
TFT_ceiling:
    call    TFT_standard_color
    WIN_MEDIUM  dm_custom_ceiling_value_column,dm_custom_ceiling_value_row
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
    WIN_TINY          dm_custom_hud_column1,dm_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask1
    WIN_TINY          dm_custom_hud_column2,dm_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask2
    WIN_TINY          dm_custom_hud_column3,dm_custom_hud_row
    STRCPY_TEXT_PRINT tDiveHudMask3
    call	TFT_standard_color
    return

    global  TFT_hud_voltages
TFT_hud_voltages:                    ; Show HUD details
    WIN_SMALL dm_custom_hud_sensor1_column,dm_custom_hud_data_row
    call	TFT_standard_color
    btfss   use_O2_sensor1
    call    TFT_warnings_color
    movff   o2_mv_sensor1+0,lo
    movff   o2_mv_sensor1+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    WIN_SMALL dm_custom_hud_sensor2_column,dm_custom_hud_data_row
    call	TFT_standard_color
    btfss   use_O2_sensor2
    call    TFT_warnings_color
    movff   o2_mv_sensor2+0,lo
    movff   o2_mv_sensor2+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    WIN_SMALL dm_custom_hud_sensor3_column,dm_custom_hud_data_row
    call	TFT_standard_color
    btfss   use_O2_sensor3
    call    TFT_warnings_color
    movff   o2_mv_sensor3+0,lo
    movff   o2_mv_sensor3+1,hi
    bsf     leftbind
    output_16dp  .4         ; x.xx
    bcf     leftbind
    STRCAT_PRINT "mV  "
    call	TFT_standard_color
    return

    global  TFT_update_ppo2_sensors         ; Update Sensor data
TFT_update_ppo2_sensors:
    ; show three sensors
    bsf     leftbind
    btfsc   use_O2_sensor1      ; Use Sensor 1?
    bra     TFT_update_hud1     ; Yes
    btfss   dive_hud1_displayed         ; Was the sensor shown?
    bra     TFT_update_hud2             ; Yes, skip clear
    bcf     dive_hud1_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dm_custom_hud_data_row, dm_customview_bot, dm_custom_hud_sensor1_column, dm_custom_hud_sensor2_column	; top, bottom, left, right
	WIN_STD         dm_custom_hud_sensor1_column+.7, dm_custom_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud2 ; Skip Sensor 1
TFT_update_hud1:
    WIN_MEDIUM dm_custom_hud_sensor1_column,dm_custom_hud_data_row
    movff   o2_ppo2_sensor1,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    btfss   voting_logic_sensor1        ; Sensor within voting logic?
    bsf     win_invert                  ; No, invert output...
    btfss   voting_logic_sensor1
    call    TFT_warnings_color          ; ... and draw in red
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bcf     win_invert
    bsf     dive_hud1_displayed         ; Set display flag
TFT_update_hud2:
    btfsc   use_O2_sensor2      ; Use Sensor 2?
    bra     TFT_update_hud3     ; Yes
    btfss   dive_hud2_displayed         ; Was the sensor shown?
    bra     TFT_update_hud4             ; Yes, skip clear
    bcf     dive_hud2_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dm_custom_hud_data_row, dm_customview_bot, dm_custom_hud_sensor2_column, dm_custom_hud_sensor3_column	; top, bottom, left, right
    WIN_STD         dm_custom_hud_sensor2_column+.7, dm_custom_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud4 ; Skip Sensor 2
TFT_update_hud3:
    WIN_MEDIUM dm_custom_hud_sensor2_column,dm_custom_hud_data_row
    movff   o2_ppo2_sensor2,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    btfss   voting_logic_sensor2        ; Sensor within voting logic?
    bsf     win_invert                  ; No, invert output...
    btfss   voting_logic_sensor2
    call    TFT_warnings_color          ; ... and draw in red
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bcf     win_invert
    bsf     dive_hud2_displayed         ; Set display flag
TFT_update_hud4:
    btfsc   use_O2_sensor3      ; Use Sensor 3?
    bra     TFT_update_hud5     ; Yes
    btfss   dive_hud3_displayed         ; Was the sensor shown?
    bra     TFT_update_hud6             ; Yes, skip clear
    bcf     dive_hud3_displayed         ; No, clear display flag
    WIN_BOX_BLACK   dm_custom_hud_data_row, dm_customview_bot, dm_custom_hud_sensor3_column, .159 ; top, bottom, left, right
    WIN_STD         dm_custom_hud_sensor3_column+.7, dm_custom_hud_data_row+.5
   	call	TFT_standard_color
    STRCPY_PRINT "---"
    bra     TFT_update_hud6 ; Skip Sensor 3
TFT_update_hud5:
    WIN_MEDIUM dm_custom_hud_sensor3_column,dm_custom_hud_data_row
    movff   o2_ppo2_sensor3,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    btfss   voting_logic_sensor3        ; Sensor within voting logic?
    bsf     win_invert                  ; No, invert output...
    btfss   voting_logic_sensor3
    call    TFT_warnings_color          ; ... and draw in red
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
    bcf     win_invert
    bsf     dive_hud3_displayed         ; Set display flag
TFT_update_hud6:
    bcf     leftbind
   	call	TFT_standard_color
    return

    global  TFT_surface_sensor             ; Update Sensor data in surface mode
TFT_surface_sensor:
    movf    hardware_flag,W
    sublw   0x11        ; 2 with BLE
    btfsc   STATUS,Z
    return              ; Ignore for 0x11

    ; show three sensors
    bsf     leftbind
    WIN_SMALL surf_hud_sensor1_column,surf_hud_sensor1_row
    btfsc   use_O2_sensor1      ; Use Sensor 1?
    bra     TFT_surface_sensor1 ; Yes
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_sensor2 ; Skip Sensor 1
TFT_surface_sensor1:
    movff   o2_ppo2_sensor1,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_sensor2:
    WIN_SMALL surf_hud_sensor2_column,surf_hud_sensor2_row
    btfsc   use_O2_sensor2      ; Use Sensor 2?
    bra     TFT_surface_sensor3 ; Yes
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_sensor4 ; Skip Sensor 2
TFT_surface_sensor3:
    movff   o2_ppo2_sensor2,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_sensor4:
    WIN_SMALL surf_hud_sensor3_column,surf_hud_sensor3_row
    btfsc   use_O2_sensor3      ; Use Sensor 3?
    bra     TFT_surface_sensor5 ; Yes
   	call	TFT_standard_color
    STRCPY_PRINT "--- "
    bra     TFT_surface_sensor6 ; Skip Sensor 3
TFT_surface_sensor5:
    movff   o2_ppo2_sensor3,lo
    TFT_color_code  warn_ppo2_hud       ; With ppO2 [cbar] in lo
    clrf    hi
    output_16dp  .3         ; x.xx bar
    STRCAT_PRINT ""
TFT_surface_sensor6:
    bcf     leftbind
   	call	TFT_standard_color
    return

    global  TFT_menu_hud
TFT_menu_hud:            ; Yes, update HUD data
    call    compute_ppo2			; compute mv_sensorX and ppo2_sensorX arrays
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

    btfss   analog_o2_input
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

    global  TFT_menu_calibrate
TFT_menu_calibrate:     ; update mV data in calibration menu
    call    compute_ppo2			; compute mv_sensorX and ppo2_sensorX arrays
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
;    WIN_SMALL   surf_menu2_ambient_column,surf_menu2_ambient_row
;    PUTC    "@"
;    SAFE_2BYTE_COPY amb_pressure, lo
;    output_16
;    STRCAT_TEXT tMBAR     ; mbar
;    STRCAT_PRINT " "
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
    call    TFT_warning_set_window_com
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
    call    TFT_warning_set_window_end
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

	global	TFT_debug_output
TFT_debug_output:
    return
    WIN_TINY   .80,.0
	call	TFT_standard_color
	lfsr	FSR2,buffer
    movff   active_gas,lo
    output_8
	STRCAT_PRINT ""
    return

    global  TFT_divetimeout                     ; Show timeout counter
TFT_divetimeout:
	call	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
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
    movlw   dm_warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
    call    TFT_warning_set_window_end
	return

	global	TFT_ftts
TFT_ftts:
    movff   char_I_extra_time,lo
    tstfsz  lo
    bra     $+4
    return                              ; char_I_extra_time=0, return.

    if dm_offset !=0
        incf    warning_counter,F			; increase counter
        call    TFT_warning_set_window		; Sets the row and column for the current warning
        tstfsz  WREG                        ; Is there room for the warning?
        return                              ; No
        call    TFT_warning_set_window_com
    else
        btfsc   divemode_menu               ; Is the dive mode menu shown?
        return                              ; Yes, return
        call    TFT_standard_color
        WIN_SMALL dm_ftts_value_column, dm_ftts_value_row
    endif

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
    movlw   dm_warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
    call    TFT_warning_set_window_end
	return

TFT_ftts2:
    STRCAT  "---"
	bcf     leftbind
    movlw   dm_warning_length             ; Divemode string length
    call    TFT_fillup_with_spaces     ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
    call    TFT_warning_set_window_end
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
	rcall	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
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
	rcall	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
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
    WIN_BOX_BLACK   dm_menu_row+.1, dm_menu_lower-.1, dm_menu_item1_column-.8, dm_menu_item1_column-.1
    WIN_BOX_BLACK   dm_menu_row+.1, dm_menu_lower-.1, dm_menu_item4_column-.8, dm_menu_item4_column-.1
    call	TFT_standard_color

    movlw   dm_menu_item1_column-.8
    btfsc   menupos,2       ; >3?
    movlw   dm_menu_item4_column-.8  ; Yes
    movff   WREG,win_leftx2
    
    movff   menupos,lo                      ; Copy menu pos
    movlw   dm_menu_item6_row
    dcfsnz  lo,F
    movlw   dm_menu_item1_row
    dcfsnz  lo,F
    movlw   dm_menu_item2_row
    dcfsnz  lo,F
    movlw   dm_menu_item3_row
    dcfsnz  lo,F
    movlw   dm_menu_item4_row
    dcfsnz  lo,F
    movlw   dm_menu_item5_row
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
	WIN_SMALL	dm_temp_column,dm_temp_row
	call	TFT_standard_color
    bsf     leftbind

    SAFE_2BYTE_COPY    temperature, lo
    TSTOSS  opt_units   			; 0=°C, 1=°F
	bra		TFT_temp_divemode_metric

;TFT_temp_divemode_imperial:
	rcall	TFT_convert_signed_16bit        ; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
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
	rcall	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
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
	WIN_STD  dm_active_gas_column, dm_active_gas_row
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
    bsf     win_invert              ; Set invert flag
    WIN_STD dm_active_gas_column, dm_active_gas_row
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
	bcf     win_invert                  ; Reset invert flag

TFT_active_setpoint_diluent:
    call	TFT_standard_color
    if dm_offset != 0
    	WIN_SMALL  dm_active_dil_column, dm_active_dil_row
    else
    	WIN_STD    dm_active_dil_column, dm_active_dil_row
    endif
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
    bsf     win_invert              ; Set invert flag
    if dm_offset != 0
        WIN_SMALL   dm_active_dil_column, dm_active_dil_row
    else
        WIN_STD     dm_active_dil_column, dm_active_dil_row
    endif
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_show_dil_divemode2      ; Show gas
	bcf     win_invert                  ; Reset invert flag
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
	WIN_STD dm_active_gas_column, dm_active_gas_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_active_gas_divemode2    ; Show gas (Non-Inverted in all cases)
	btfss	better_gas_available        ; =1: A better gas is available and a gas change is advised in divemode
	return					; Done.

	btg		blinking_better_gas         ; Toggle blink bit...
	btfss	blinking_better_gas         ; blink now?
	return                              ; No, Done.
    call    TFT_attention_color         ; blink in yellow
    bsf     win_invert                  ; Set invert flag
    WIN_STD dm_active_gas_column, dm_active_gas_row
    movff   char_I_O2_ratio,lo          ; lo now stores O2 in %
    movff   char_I_He_ratio,hi          ; hi now stores He in %
	rcall	TFT_active_gas_divemode2    ; Show gas (Non-Inverted in all cases)
	bcf     win_invert                  ; Reset invert flag
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
    bsf     ccr_diluent_setup       ; Use CCR Diluents...
    rcall   TFT_gaslist_surfmode    ; Use OC/BAIL routine
    bcf     ccr_diluent_setup       ; Clear flag
    return

;==================================================================

	global	TFT_depth
TFT_depth:
    SAFE_2BYTE_COPY rel_pressure, lo
    call    adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]

    TFT_color_code  warn_depth			; Color-code the output
    call TFT_depth_blink
    WIN_LARGE   dm_depth_column, dm_depth_row

    TSTOSS  opt_units   			; 0=m, 1=ft
    bra     TFT_depth_metric
;TFT_depth_imperial
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
    bcf     win_invert              ; Reset invert flag
    return

depth_less_0.3mtr_feet:
	STRCAT_PRINT "0  "				; manual zero
    bcf     win_invert                  ; Reset invert flag
	return

TFT_depth_metric:
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
	STRCAT	"0"                    ; manual zero

tft_depth3:
	STRCAT_PRINT ""					; Display full meters

	; .1m in MEDIUM font
	WIN_MEDIUM	dm_depth_dm_column, dm_depth_dm_row
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
    bcf     win_invert                  ; Reset invert flag
	WIN_FONT 	FT_SMALL
	return

depth_less_0.3mtr:
	STRCAT_PRINT "0"				; Display 0.0m manually
    bcf     win_invert                  ; Reset invert flag
	WIN_FONT 	FT_SMALL
	return

depth_greater_99_84mtr:			; Display only in full meters
	btfss	depth_greater_100m		; Is depth>100m already?
	rcall	TFT_clear_depth			; No, clear depth area and set flag
    TFT_color_code	warn_depth
	; Depth is already in hi:lo
	; Show depth in Full meters
	; That means ignore figure 4 and 5
	lfsr    FSR2,buffer
	bsf		ignore_digit4
	bsf		leftbind
	output_16
	bcf		leftbind
    STRCAT_PRINT ""					; Display full meters only
    bcf     win_invert                  ; Reset invert flag
	WIN_FONT 	FT_SMALL
	return

TFT_clear_depth:            			; No, clear depth area and set flag
    WIN_BOX_BLACK   dm_depth_row, dm_depth_bot, dm_depth_column, dm_depth_rgt    ;top, bottom, left, right
	bsf		depth_greater_100m			; Set Flag
	return

TFT_depth_blink:
    TSTOSS  opt_modwarning			; 0=standard, 1=blink
    return

    ; check if previous cycle had the blinking warning or not
    btfsc   blinking_depth_prev         ; did we have warning prev?
    bra     TFT_depth_blink_prevwarn    ; Yes

    ; No; check if it's set now
    btfsc   blinking_depth_warning      ; do we have warning set now?
    bra     TFT_depth_blink_warn        ; Yes  - so we have warning now but not prev

    ; no warning in previous cycle, no warning now, reset all flags
    bcf     blinking_depth_prev         ; reset prev flag
    bcf     blinking_depth_toggle       ; reset toggle
    bcf     win_invert
    ; all done
    return

TFT_depth_blink_prevwarn:
    ; ...we had warning in previous cycle, check if we still have the warning set
    btfss   blinking_depth_warning              ; do we still have the warning?
    bra     TFT_depth_blink_prevwarn_nowarn     ; No, clear the depth area

    ; we still have the warning, set previous flag for next cycle...
    bsf     blinking_depth_prev                 ; set prev flag
    ; and set toggle and invert if required
    btfss   blinking_depth_toggle               ; do we have the toggle set?
    bra    TFT_depth_blink_set                  ; No:  set inverse,   do color_box, set flag
    bra    TFT_depth_blink_reset                ; Yes: clear inverse, do black box, reset flag

TFT_depth_blink_prevwarn_nowarn:
    ; we had warning, but not now... (e.g. ascended or switched to better gas)
    ; reset the previous cycle flag for the next cycle...
    bcf     blinking_depth_prev         ; reset prev flag
    ; clear it - just in case if we had a blinked before
    bra    TFT_depth_blink_reset                ; Yes: clear inverse, do black box, reset flag

TFT_depth_blink_warn:
    ; new blinking warning activated (had no warning in previous cycle)
    bsf     blinking_depth_prev                 ; set prev flag
    ; set toggle and invert
    bra    TFT_depth_blink_set

TFT_depth_blink_set:
    ; clear the area with color
    movlw   color_red  ; that should not be hardcoded...
    WIN_BOX_COLOR    dm_depth_row, dm_depth_bot, dm_depth_column, dm_depth_rgt    ;top, bottom, left, right
    ;set the invert color
    bsf     win_invert
    ; set the toggle
    bsf     blinking_depth_toggle
    ; all done
    return

TFT_depth_blink_reset:
    ; clear the area with black
    WIN_BOX_BLACK    dm_depth_row, dm_depth_bot, dm_depth_column, dm_depth_rgt    ;top, bottom, left, right
    ;reset the invert color
    bcf     win_invert
    ; reset the toggle
    bcf     blinking_depth_toggle
    ; if it's still warning...
    btfsc   blinking_depth_warning
    call    TFT_warnings_color
    ; all done
    return

;=============================================================================

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
    STRCPY_TEXT_PRINT  tMBAR        ; mbar
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

    ; Setup charge indicator
    btfsc   cc_active
    bsf     win_invert
    btfsc   cc_active
    movlw   color_yellow
    btfsc   cv_active
    movlw   color_green
    btfsc   cc_active
    call	TFT_set_color

	WIN_TINY batt_percent_column,batt_percent_row
	bsf		leftbind
	output_8
	bcf		leftbind
	STRCAT	"% "
	movlw	0x00
	movff	WREG,buffer+4			; Only "xxx%"
    STRCAT_PRINT	""
    bcf     win_invert
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
	WIN_MEDIUM	dm_max_depth_column, dm_max_depth_row
	call	TFT_standard_color
	output_16_3
	STRCAT_PRINT ""
	return

TFT_max_pressure2_metric:
    WIN_MEDIUM	dm_max_depth_column, dm_max_depth_row
    call    TFT_standard_color

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
	WIN_MEDIUM	dm_max_depth_column, dm_max_depth_row
	STRCAT	"0"

tft_max_depth3:
	call	TFT_standard_color
	STRCAT_PRINT ""					; Display full meters
    bcf     leftbind

	; .1m in SMALL font
	WIN_SMALL	dm_max_depth_dm_column, dm_max_depth_dm_row

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
    WIN_BOX_BLACK   dm_max_depth_row, dm_max_depth_bot, dm_max_depth_column, dm_max_depth_rgt    ;top, bottom, left, right
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
    WIN_TINY    dm_apnoe_last_max_depth_text_col, dm_apnoe_last_max_depth_text_row
    STRCPY_TEXT_PRINT   tApnoeMax

	call	TFT_standard_color
	SAFE_2BYTE_COPY max_pressure, lo
    call	adjust_depth_with_salinity			; computes salinity setting into lo:hi [mbar]
    TSTOSS  opt_units   			; 0=m, 1=ft
	bra		TFT_display_apnoe_last_m_metric
;TFT_display_apnoe_last_max_imperial
	call	convert_mbar_to_feet              	; convert value in lo:hi from mbar to feet
	WIN_MEDIUM	dm_apnoe_last_max_depth_column, dm_apnoe_last_max_depth_row
	output_16
	STRCAT_PRINT ""
	return

TFT_display_apnoe_last_m_metric:
	WIN_MEDIUM	dm_apnoe_last_max_depth_column, dm_apnoe_last_max_depth_row
	bsf		ignore_digit5		; do not display 1cm depth
	output_16dp	d'3'
	STRCAT_PRINT ""
	return

;=============================================================================
	global	TFT_divemins
TFT_divemins:
	movff	divemins+0,lo
	movff	divemins+1,hi

    ; Already showing divemins > 99min
	btfsc	no_more_divesecs		; Ignore seconds?
	bra     TFT_divemins2           ; Show minutes only

    ; check if divemins has hi, definitely > 99min
    movlw   .1
    cpfslt  hi                      ; HI less than 1?
    bra     TFT_divemins_clr        ; No, show mins only

    ; check if divemins (when HI is zero) > 99min
	movlw	.99
	cpfsgt	lo                      ; bigger than 99?
	bra		TFT_divemins1           ; No show mins:secs

TFT_divemins_clr:
	; Yes, remove second display for the rest of the dive and clear seconds
	bsf		no_more_divesecs        ; Set flag
	; Clear rest of seconds
	WIN_BOX_BLACK   dm_divetime_row, dm_divetime_bot, dm_divetime_column, dm_divetime_rgt ;top, bottom, left, right
    bra     TFT_divemins2           ; Show minutes only

TFT_divemins1:
    ; Print out the minutes, up to 99min, only 2chars !
	call	TFT_standard_color
	WIN_MEDIUM	dm_divetime_column, dm_divetime_row

    movlw	.9
	cpfsgt	lo                      ; bigger then 9?
    bra     TFT_divemins1_pad       ; No, need padding
    ; Yes, just print out the value
	bsf		leftbind
	movff	divemins+0,lo
	output_99x                     ; displays only last two figures from a 8Bit value (00-99)
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show minutes in large font
    bra     TFT_divemins1_sec

TFT_divemins1_pad:
	bcf     leftbind
	PUTC    " "                     ; Add a padding space
	bsf		leftbind
	movff	divemins+0,lo
	output_99                     ; displays only last two figures from a 8Bit value (0-99)
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show minutes in large font

TFT_divemins1_sec:
    ; Print out the seconds
	WIN_SMALL  dm_divetime_secs_column, dm_divetime_secs_row   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff   divesecs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font
	return

TFT_divemins2:
	call	TFT_standard_color
	WIN_MEDIUM	dm_divetime_minsonly_column, dm_divetime_row
    bcf		leftbind

    ; if we are at or over the limit, do a WARNIGN color
    ; 9999 = 27 0F = [39][15]
    movlw   .38
    cpfsgt  hi                  ; hi > 38 ?
    bra     TFT_divemins2_out   ; No, hi <= 38, no need to warn

    movlw   .40
    cpfslt  hi                  ; hi < 40 ?
    bra     TFT_divemins2_warn  ; No, hi >= 40, need to warn
    
    ; hi = 39, check lo. check 14 as 9999 should be already WARN!
    movlw   .14
    cpfsgt  lo
    bra     TFT_divemins2_out   ; No, lo <= 14, no need to warn
    ; Yes, lo > 14, need to warn

TFT_divemins2_warn:
    call    TFT_warnings_color
    bsf     win_invert

TFT_divemins2_out:
	output_16_4
	STRCAT_PRINT ""                 ; Show minutes in large font
    bcf     win_invert
    return

;=============================================================================
	global	TFT_display_apnoe_surface
TFT_display_apnoe_surface:
    call    TFT_divemask_color
    WIN_TINY    dm_apnoe_surface_time_text_col,  dm_apnoe_surface_time_text_row
    STRCPY_TEXT_PRINT   tApnoeSurface

	call	TFT_standard_color
	WIN_MEDIUM	dm_apnoe_surface_time_column, dm_apnoe_surface_time_row
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
	WIN_BOX_BLACK   dm_apnoe_surface_time_text_row, .239, dm_apnoe_surface_time_text_col, .159                 ;top, bottom, left, right
	return

	global	TFT_display_apnoe_descent
TFT_display_apnoe_descent:		; Descent divetime
	movff	apnoe_mins,lo
    clrf    hi
	WIN_MEDIUM	dm_divetime_apnoe_column, dm_divetime_apnoe_row
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL   dm_divetime_apnoe_secs_column, dm_divetime_apnoe_secs_row   		; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff	apnoe_secs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font

    call    TFT_divemask_color
    WIN_TINY    dm_total_apnoe_text_column,dm_total_apnoe_text_row
    STRCPY_TEXT_PRINT   tApnoeTotal
	call	TFT_standard_color
	movff	divemins,lo
    clrf    hi
	WIN_MEDIUM	dm_apnoe_total_divetime_column, dm_apnoe_total_divetime_row
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999)
	call	TFT_standard_color
	STRCAT_PRINT ""                 ; Show minutes in large font
	WIN_SMALL   dm_apnoe_total_divetime_secs_col, dm_apnoe_total_divetime_secs_row	; left position for two sec figures
	PUTC    ':'
	bsf		leftbind
	movff	divesecs,lo
	output_99x
	bcf     leftbind
	STRCAT_PRINT ""                 ; Show seconds in small font
	return
	
;=============================================================================
; Writes ostc #Serial and Firmware version in splash screen
;
	global	TFT_serial
TFT_serial:		
    WIN_TINY	.5,.225
    STRCPY  "OSTC"                    ; Won't translate that...

    movlw   0x0A
    cpfseq  hardware_flag
    bra     TFT_serial2
    STRCAT  "3 #"
    bra     TFT_serial_common
TFT_serial2:
    movlw   0x05
    cpfseq  hardware_flag
    bra     TFT_serial3
    STRCAT  " cR #"
    bra     TFT_serial_common
TFT_serial3:
    movlw   0x11
    cpfseq  hardware_flag
    bra     TFT_serial4
    STRCAT  "2 #"
    bra     TFT_serial_common
TFT_serial4:
    movlw   0x1A
    cpfseq  hardware_flag
    bra     TFT_serial5
    STRCAT  "3 #"
;    bra     TFT_serial_common
TFT_serial5:
TFT_serial_common:
    rcall   TFT_cat_serial
    STRCAT  " v"
    WIN_COLOR   color_greenish
    rcall   TFT_cat_firmware

    ifdef __DEBUG
        movlw   color_grey              ; Write header in blue when
        call    TFT_set_color           ; compiled in DEBUG mode...
        STRCAT_PRINT "DEBUG"    
    else
        STRCAT_PRINT ""
        bcf     win_invert              ; Reset invert flag
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
    rcall   TFT_cat_firmware
    bcf     win_invert              ; Reset invert flag
    return

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
    ; Check firmware date
    movlw   firmware_expire_year-.1
    cpfsgt  year                    ; > threshold?
    return
    movlw   firmware_expire_month-.1
    cpfsgt  month                   ; > threshold?
    return
    movlw   firmware_expire_day-.1
    cpfsgt  day                     ; > threshold?
    return

    ; Show in "change firmware" style
    movlw   color_yellow
    call	TFT_set_color
    bsf     win_invert              ; Set invert flag
    return

;-----------------------------------------------------------------------------
; For the Information menu: append serial number 
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
    WIN_BOX_BLACK   dm_warning_row,    dm_warning_bot,        dm_warning_column,    dm_warning_rgt	;top, bottom, left, right
    return
TFT_clear_warning_text2:
    WIN_BOX_BLACK   surf_warning1_row, surf_warning2_row+.24, surf_warning1_column, surf_warning1_column+.76     ;top, bottom, left, right
    return

    global  TFT_clear_warning_text_2nd_row
TFT_clear_warning_text_2nd_row:
    btfss   divemode                            ; in divemode?
    bra     TFT_clear_warning_text_2nd_2        ; No, setup for surface mode
    WIN_BOX_BLACK  dm_warning2_row,    dm_warning2_bot,       dm_warning2_column,   dm_warning2_rgt	;top, bottom, left, right
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
    call    TFT_warning_set_window_com
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
    call    TFT_warning_set_window_end
	return

	global	TFT_nofly_time
TFT_nofly_time:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
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
    call    TFT_warning_set_window_end
	return

    global  TFT_warning_agf
TFT_warning_agf:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
    call	TFT_warnings_color
	STRCPY_TEXT tDiveaGF_active         ; "aGF!"
    movlw   dm_warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
    call    TFT_warning_set_window_end
    return

    global  TFT_warning_fallback
TFT_warning_fallback:                ; Show fallback warning
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call	TFT_warnings_color
	STRCPY_TEXT tDiveFallback           ; "Fallback!"
    movlw   dm_warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
    return

    global  TFT_warning_gf
TFT_warning_gf:                         ;GF
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
	TFT_color_code		warn_gf		; Color-code Output
	STRCPY  "GF:"
	movff	char_O_gradient_factor,lo		; gradient factor
    bsf     leftbind
	output_8
    PUTC    "%"
    movlw   dm_warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT  ""
    bcf     leftbind
	call	TFT_standard_color
    call    TFT_warning_set_window_end
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
	WIN_SMALL	dm_warning1_column, dm_warning1_row
    bcf     second_row_warning          ; =1: The second row contains a warning
	retlw   .0                          ; WREG=0 -> Warning window defined
TFT_warning_set_window2:
	WIN_SMALL	dm_warning2_column, dm_warning2_row
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

TFT_warning_set_window_com:
    if dm_offset == 0
        bsf     win_invert
    endif
    return

TFT_warning_set_window_end:
    bcf     win_invert
    return

	global	TFT_update_batt_percent_divemode
TFT_update_batt_percent_divemode:
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
    movff   batt_percent,lo         ; Get battery percent
    TFT_color_code		warn_battery; Color-code battery percent
    STRCPY  "Batt:"
	bsf		leftbind
	output_8
	bcf		leftbind
    PUTC    "%"
    movlw   dm_warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT	""
	call	TFT_standard_color
    call    TFT_warning_set_window_end
	return

    global  TFT_gf_mask                         ; Setup Mask
TFT_gf_mask:
    ; The mask
    call    TFT_divemask_color
    WIN_TINY          dm_custom_gf_title_col1, dm_custom_gf_title_row
    STRCPY_TEXT_PRINT tGFactors
    WIN_TINY          dm_custom_gf_title_col2, dm_custom_gf_title_row
    STRCPY_TEXT_PRINT taGFactors
    WIN_TINY          dm_custom_gf_title_col3, dm_custom_gf_title_row
    STRCPY_TEXT_PRINT tGFInfo

    ; Show GF (Static)
    call    TFT_disabled_color
    btfss   use_agf
    call	TFT_standard_color

    WIN_STD   dm_custom_gf_column, dm_custom_gf_row
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

    WIN_STD   dm_custom_agf_column, dm_custom_agf_row
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
    WIN_STD   dm_custom_agf_column+.10, dm_custom_agf_row
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
    WIN_STD   dm_custom_currentgf_column, dm_custom_currentgf_row
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
    WIN_TINY          dm_custom_ead_column,     dm_custom_eadend_title_row
    STRCPY_TEXT_PRINT tDiveEAD_END
    WIN_TINY          dm_custom_tissue_title_column,  dm_custom_tissue_title_row
    STRCPY_TEXT_PRINT tDiveTissues
TFT_ead_end_tissues_clock_mask2:            ; Show only clock
    WIN_TINY    dm_custom_clock_column,  dm_custom_clock_title_row
    STRCPY_TEXT_PRINT tDiveClock
    call	TFT_standard_color
    return

    global  TFT_ead_end_tissues_clock           ; Show EAD/END, Tissues and clock
TFT_ead_end_tissues_clock:
    ; Update clock and date
    WIN_SMALL   dm_custom_clock_column, dm_custom_clock_row
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
    WIN_SMALL   dm_custom_ead_column, dm_custom_ead_row
    STRCPY_TEXT tEAD                            ; EAD:
    movff   char_O_EAD,lo
    rcall   TFT_end_ead_common                  ; print "lo m" (or ft) and limit to 8 chars
    WIN_SMALL   dm_custom_end_column, dm_custom_end_row
    STRCPY_TEXT tEND                            ; END:
    movff   char_O_END,lo
    rcall   TFT_end_ead_common                  ; print "lo m" (or ft) and limit to 8 chars

    ; Show tissue diagram
    call    TFT_divemask_color
    WIN_TINY    dm_custom_tissue_N2_column, dm_custom_tissue_N2_row
    STRCPY_TEXT_PRINT   tN2
    WIN_TINY    dm_custom_tissue_He_column, dm_custom_tissue_He_row
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
	movlw	surf_tissue_diagram_right - surf_tissue_diagram_left - .4  ; Width
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
    WIN_FRAME_COLOR16   dm_custom_tissue_diagram_top, dm_custom_tissue_diagram_bottom, dm_custom_tissue_diagram_left, .159    ; outer frame

	movlw	.1
	movff	WREG,win_height             ; row bottom (0-239)
    movlw   dm_custom_tissue_diagram_left+.3      ; divemode
	movff	WREG,win_leftx2             ; column left (0-159)
	movlw	.159-dm_custom_tissue_diagram_left-.4  ; Width
	movff   WREG,win_width

    ;---- Draw N2 Tissues
	lfsr	FSR2, char_O_tissue_N2_saturation
	movlw	d'16'
	movwf	wait_temp                   ; 16 tissues
	clrf	waitms_temp                 ; Row offset
tissue_saturation_graph_N2:
    movlw   dm_custom_tissue_diagram_top+.3        ; divemode
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
    movlw   dm_custom_tissue_diagram_top+.3+.22    ; divemode
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
    call    TFT_warning_set_window_com
	TFT_color_code		warn_cns		; Color-code CNS output
	STRCPY_TEXT tCNS2                   ; CNS:
	movff	int_O_CNS_fraction+0,lo
    movff	int_O_CNS_fraction+1,hi
	bsf		leftbind
	output_16_3					;Displays only 0...999
	bcf		leftbind
    PUTC    "%"
    movlw   dm_warning_length              ; Divemode string length
    btfss   divemode                    ; In Divemode?
    movlw   surf_warning_length         ; No, use surface string length
    rcall   TFT_fillup_with_spaces          ; Fillup FSR2 with spaces (Total string length in #WREG)
	STRCAT_PRINT ""
	call	TFT_standard_color
    call    TFT_warning_set_window_end
	return

	global	TFT_display_ppo2
TFT_display_ppo2:                       ; Show ppO2 (ppO2 stored in xC, in mbar!)
	rcall	TFT_warning_set_window		; Sets the row and column for the current warning
    tstfsz  WREG                        ; Is there room for the warning?
    return                              ; No
    call    TFT_warning_set_window_com
	TFT_color_code		warn_ppo2		; Color-code output (ppO2 stored in xC)
    STRCPY_TEXT tppO2                   ; ppO2:
; Check very high ppO2 manually
	tstfsz	xC+2                        ; char_I_O2_ratio * p_amb/10 > 65536, ppO2>6,55bar?
	bra		TFT_show_ppO2_3             ; Yes, display fixed Value!
	movff	xC+0,lo
	movff	xC+1,hi
	bsf		ignore_digit4
	output_16dp	d'1'
TFT_show_ppO2_2:
    movlw   dm_warning_length              ; Divemode string length
    rcall   TFT_fillup_with_spaces      ; Fillup FSR2 with spaces (Total string length in #WREG)
    STRCAT_PRINT ""
	call	TFT_standard_color
    call    TFT_warning_set_window_end
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
	
	global	TFT_VSImenu_dynamictitle
TFT_VSImenu_dynamictitle:
	STRCPY_TEXT tVSItext1
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