;=============================================================================
;
;   File simulator.asm
;
;   Decoplan interface to C model code.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-07-09 : [jDG] Creation...

#include "ostc3.inc"					; Mandatory include.
#include "convert.inc"                  ; output_*
#include "shared_definitions.h"         ; Mailbox from/to p2_deco.c
#include "strings.inc"                  ; STRCPY,...
#include "tft.inc"                      ; WIN_LEFT,...
#include "wait.inc"                     ; speed_*
#include "start.inc"
#include "divemode.inc"
#include "math.inc"
#include "eeprom_rs232.inc"

gui     CODE

    extern  deco_clear_tissue
    extern  deco_push_tissues_to_vault
    extern  deco_calc_dive_interval
    extern  deco_calc_hauptroutine
    extern  deco_calc_tissue
    extern  deco_calc_CNS_fraction
    extern  deco_calc_CNS_planning
    extern  deco_pull_tissues_from_vault

    extern  log_screendump_and_onesecond, logbook_preloop_tasks

;---- Private temp variables -------------------------------------------------
        CBLOCK  tmp+0x10                ; Reserved space for wordprocessor and convert
            decoplan_index              ; within each page
            decoplan_gindex             ; global index
            decoplan_last               ; Depth of last stop (CF#29)
            decoplan_max                ; Number of lines per page.
            decoplan_flags              ; Various private flags.
            decoplan_CNS:2              ; Backup CNS before vault restore
            ; Reserved to tmp+0x1F...
        ENDC
#define decoplan_last_ceiling_shown   decoplan_flags,0

;---- Demo decoplanner -------------------------------------------------------
        global  do_demo_planner
        extern  do_planner_menu

do_demo_planner:
        call    speed_fastest
;        call    deco_reset              ; TODO: remove reset all Decodata
        call    deco_planer
        call    deco_show_plan
        bcf     switch_right
        bcf     switch_left
        goto    do_planner_menu

;=============================================================================
; Pass all parameters to the C code
;

    global  get_first_dil_to_WREG
get_first_dil_to_WREG:                  ; Gets first dil (0-4) into WREG
        lfsr    FSR1,opt_dil_type       ; Point to dil types
        clrf    lo                      ; start with Gas0
        bra     get_first_gas_to_WREG2  ; Start

    global  get_first_gas_to_WREG
get_first_gas_to_WREG:                  ; Gets first gas (0-4) into WREG
        lfsr    FSR1,opt_gas_type       ; Point to gas types
        clrf    lo                      ; start with Gas0
get_first_gas_to_WREG2:
        movf    lo,W                    ;
        movf    PLUSW1,W                ; Get Type of Gas #lo
        sublw   .1                      ; it is = 1 (First Gas)
        bz      get_first_gas_to_WREG3  ; Found the first gas!
        incf    lo,F                    ; ++
        movlw   NUM_GAS+1
        cpfseq  lo                      ; All done?
        bra     get_first_gas_to_WREG2  ; Not yet
        retlw  .1                       ; No first gas found, use #1
get_first_gas_to_WREG3:
        movf    lo,W                    ; Put into Wreg
        return                          ; Done

        global  deco_setup
deco_setup:
        banksel char_I_step_is_1min     ; Select the right bank...
        clrf    char_I_step_is_1min     ; Default to 2sec steps.

        ; Fixed ambient surface pressure to 1bar.
        movlw   LOW(.1000)
        movwf   int_I_pres_surface+0
        movwf   int_I_pres_respiration+0
        movlw   HIGH(.1000)
        movwf   int_I_pres_surface+1
        movwf   int_I_pres_respiration+1
    
        clrf    int_I_divemins+0        ; Dive start
        clrf    int_I_divemins+1

        call    get_first_gas_to_WREG           ; Gets first gas (0-4) into WREG
        movff   WREG,char_I_first_gas           ; Copy for compatibility
        extern  setup_gas_registers
        call    setup_gas_registers             ; With WREG=Gas 0-4, set current N2/He/O2 ratios.
        extern  set_actual_ppo2
        call    set_actual_ppo2                 ; Then configure char_I_actual_ppO2 (For CNS)

        global	deco_setup_dive
deco_setup_dive:						; Called from divemode
        banksel common                  ; Bank1

        btfss   FLAG_ccr_mode           ; =1: CCR mode (Fixed ppO2 or Sensor) active
        rcall   deco_setup_oc_gases     ; Setup OC Gases
        btfsc   FLAG_ccr_mode           ; =1: CCR mode (Fixed ppO2 or Sensor) active
        rcall   deco_setup_cc_diluents  ; Setup CC Diluents
        btfsc   is_bailout              ; =1: Bailout
        rcall   deco_setup_oc_gases     ; Setup OC/Bailout Gases

        movlw   deco_distance
        movff   WREG,char_I_deco_distance
        movff   opt_last_stop,char_I_depth_last_deco
        movff   opt_GF_low,char_I_GF_Low_percentage
        movff   opt_GF_high,char_I_GF_High_percentage
        ;Overwrite GF if aGF is wanted
        btfsc   use_agf                         ; =1: Use aGF
        movff   opt_aGF_low,char_I_GF_Low_percentage
        btfsc   use_agf                         ; =1: Use aGF        
        movff   opt_aGF_high,char_I_GF_High_percentage
        return

deco_setup_cc_diluents:
        movff   opt_dil_He_ratio+0,char_I_deco_He_ratio+0
        movff   char_I_deco_He_ratio+0,lo
        movff   opt_dil_O2_ratio+0,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+0
        movff   opt_dil_type+0,WREG   ; 0=Disabled, 1=First, 2=Normal
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+0   ; Yes, clear char_I_deco_gas_change

        movff   opt_dil_He_ratio+1,char_I_deco_He_ratio+1
        movff   char_I_deco_He_ratio+1,lo
        movff   opt_dil_O2_ratio+1,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+1
        movff   opt_dil_type+1,WREG   ; 0=Disabled, 1=First, 2=Normal
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+1   ; Yes, clear char_I_deco_gas_change

        movff   opt_dil_He_ratio+2,char_I_deco_He_ratio+2
        movff   char_I_deco_He_ratio+2,lo
        movff   opt_dil_O2_ratio+2,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+2
        movff   opt_dil_type+2,WREG   ; 0=Disabled, 1=First, 2=Normal
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+2   ; Yes, clear char_I_deco_gas_change

        movff   opt_dil_He_ratio+3,char_I_deco_He_ratio+3
        movff   char_I_deco_He_ratio+3,lo
        movff   opt_dil_O2_ratio+3,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+3
        movff   opt_dil_type+3,WREG   ; 0=Disabled, 1=First, 2=Normal
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+3   ; Yes, clear char_I_deco_gas_change

        movff   opt_dil_He_ratio+4,char_I_deco_He_ratio+4
        movff   char_I_deco_He_ratio+4,lo
        movff   opt_dil_O2_ratio+4,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+4
        movff   opt_dil_type+4,WREG   ; 0=Disabled, 1=First, 2=Normal
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+4   ; Yes, clear char_I_deco_gas_change
        return

deco_setup_oc_gases:
        movff   opt_gas_He_ratio+0,char_I_deco_He_ratio+0
        movff   char_I_deco_He_ratio+0,lo
        movff   opt_gas_O2_ratio+0,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+0
        movff   opt_gas_type+0,WREG   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+0   ; Yes, clear char_I_deco_gas_change

        movff   opt_gas_He_ratio+1,char_I_deco_He_ratio+1
        movff   char_I_deco_He_ratio+1,lo
        movff   opt_gas_O2_ratio+1,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+1
        movff   opt_gas_type+1,WREG   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+1   ; Yes, clear char_I_deco_gas_change

        movff   opt_gas_He_ratio+2,char_I_deco_He_ratio+2
        movff   char_I_deco_He_ratio+2,lo
        movff   opt_gas_O2_ratio+2,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+2
        movff   opt_gas_type+2,WREG   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+2   ; Yes, clear char_I_deco_gas_change

        movff   opt_gas_He_ratio+3,char_I_deco_He_ratio+3
        movff   char_I_deco_He_ratio+3,lo
        movff   opt_gas_O2_ratio+3,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+3
        movff   opt_gas_type+3,WREG   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+3   ; Yes, clear char_I_deco_gas_change

        movff   opt_gas_He_ratio+4,char_I_deco_He_ratio+4
        movff   char_I_deco_He_ratio+4,lo
        movff   opt_gas_O2_ratio+4,WREG
        addwf   lo,W                  ; O2 + He -> WREG
        sublw   .100                  ; 100 - (O2 + He) -> WREG
        movff   WREG,char_I_deco_N2_ratio+4
        movff   opt_gas_type+4,WREG   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
        tstfsz  WREG                  ; Disabled?
        bra     $+4                   ; No
        movff   WREG,char_I_deco_gas_change+4   ; Yes, clear char_I_deco_gas_change
        return

;=============================================================================
; Reset decompression tissues
; 
        global  deco_reset
deco_reset:
        rcall   deco_setup              ; Setup all model parameters.
        call    deco_clear_tissue       ; Set all tissues to Pamb * N2_ratio
        call    deco_clear_CNS_fraction ; Reset CNS value.
        banksel common                  ; Bank1
        return

;=============================================================================
; Launch decoplanning
; 
    global  deco_planer
deco_planer:
        call    speed_fastest           ; Quick !
        rcall   deco_setup              ; Setup all model parameters.
        call    deco_push_tissues_to_vault
        banksel common                  ; Bank1

;---- Specific settings ------------------------------------------------------
   
        banksel char_O_deco_status      ; Bank 2
        movlw   .3                      ; Start in surface state.
        movwf   char_O_deco_status
    
        banksel char_I_step_is_1min     ; Bank 3
        movlw   1
        movwf   char_I_step_is_1min     ; Set 1min steps

;---- Add delay at surface, if needed ----------------------------------------
        tstfsz  char_I_dive_interval
        call    deco_calc_dive_interval

;---- Dive loop --------------------------------------------------------------

        ; Compute dive ambiant conditions
        banksel char_I_bottom_depth
        movf    char_I_bottom_depth,W
        mullw   .100
        movlw   LOW(.1000)
        addwf   PRODL,W
        movwf   int_I_pres_respiration+0
        movlw   HIGH(.1000)
        addwfc  PRODH,W
        movwf   int_I_pres_respiration+1

        banksel int_I_divemins          ; Bank 4
        clrf    int_I_divemins+0        ; Clear dive time
        clrf    int_I_divemins+1

        clrf    TMR5L
        clrf    TMR5H                   ; 30,51757813µs/bit in TMR5L:TMR5H
        call    deco_calc_hauptroutine  ; Reset + simulate first min.

deco_planer_loop:
        banksel int_I_divemins          ; Bank 3
        incf    int_I_divemins,F        ; Done 1 min.
        btg     LEDg
    
        movf    char_I_bottom_time,W    ; Finished ?
        xorwf   int_I_divemins,W
        bz      deco_planer_endloop     ; YES
    
        call	deco_calc_tissue	    ; JUST calc tissue (faster).
        call	deco_calc_CNS_fraction  ; Also calculate CNS (in 1min loop)
        bra     deco_planer_loop

deco_planer_endloop:
        banksel char_I_step_is_1min
        clrf    char_I_step_is_1min     ; Back to 2sec loops

;---- Wait until status reach zero -------------------------------------------
deco_planer_finishing:
        btg     LEDg
        clrf    TMR5L
        clrf    TMR5H                       ; 30,51757813µs/bit in TMR5L:TMR5H
        call    deco_calc_hauptroutine  ; Simulate 2sec more

        banksel char_O_deco_status      ; Bank 2
        movf    char_O_deco_status,W
        bz      deco_planer_finished
        
        bra     deco_planer_finishing

deco_planer_finished:
        call    deco_calc_CNS_planning
        movff   int_O_CNS_fraction+0,decoplan_CNS+0
        movff   int_O_CNS_fraction+1,decoplan_CNS+1
        call    deco_pull_tissues_from_vault
        bcf     LEDg
        banksel common                  ; Bank1
		movlw	b'00111000'				; 1:8 Prescaler -> 65,536ms@16MHz
		movwf	T3CON
        call    speed_normal
        return

;-----------------------------------------------------------------------------
; Draw a stop of the deco plan (simulator or dive).
; Inputs: lo      = depth. Range 3m...93m
;                 + 80 if this is a switch-gas stop.
;         up      = minutes. range 1'..240'.
;         win_top = line to draw on screen.
; Trashed: up, lo, win_height, win_leftx2, win_width, win_color*,
;          WREG, PROD, TBLPTR TABLAT.
;
deco_plan_show_stop:
        ;---- Print depth ----------------------------------------------------
        btfss   lo,7                    ; Bit set ?
        bra     deco_plan_show_std_stop  ; No : Just an usual stop.

        TFT_ATTENTION_COLOR
        bcf     lo,7                    ; and cleanup depth.
        bra     deco_plan_show_nstd_stop

deco_plan_show_std_stop:
    	TFT_STD_COLOR

deco_plan_show_nstd_stop:        
	    lfsr	FSR2,buffer

		TSTOSS  opt_units			; 0=Meters, 1=Feets
		bra		deco_plan_show_nstd_stop_metric

        WIN_LEFT .85
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
		bsf		leftbind
		bsf		ignore_digit4			; Only full feet
		output_16
		STRCAT_PRINT	"ft "
		bra		deco_plan_show_nstd_stop_common

deco_plan_show_nstd_stop_metric:
        WIN_LEFT .90
	    bsf     leftbind
	    output_8					    ; outputs into Postinc2!
        STRCAT_PRINT 	"m "
deco_plan_show_nstd_stop_common:
        ;---- Print duration -------------------------------------------------
	    WIN_LEFT	.135
	    lfsr	FSR2,buffer
	    
	    movf    lo,W                    ; Swap up & lo
	    movff   up,lo
	    movwf   up

	    output_8					    ; Allow up to 240'
        STRCAT_PRINT "'  "              ; 1 to 3 chars for depth.

	    movf    lo,W                    ; Swap back up & lo
	    movff   up,lo
	    movwf   up

        ;---------------------------------------------------------------------
        ; Draw the bar graph used for deco stops (decoplan in simulator or dive).
        incf   win_top,F
        movlw	.19
        movwf   win_height
        movlw	.118
        movwf	win_leftx2    		    ; column left (0-159)
        movlw	.16
        movwf	win_width    		    ; column max width.

        ; Draw used area (up = minutes):
        movlw	.16                     ; Limit length (16min)
        cpfslt	up
        movwf	up
        movff	up,win_bargraph         ; Active width, the rest is cleared.
        call	TFT_box

        ; Restore win_top
	    TFT_STD_COLOR
        decf    win_top,F               ; Restore win_top
        return

;-----------------------------------------------------------------------------
; Clear unused area belw last stop
; Inputs: win_top : last used area...
deco_plan_show_clear_bottom:
        movf    win_top,W               ; Get back from bank0
        btfsc   divemode                ; In dive mode ?
        sublw   .168                    ; Yes: bottom row in divemode
        btfss   divemode                ; In dive mode ?
        sublw   .240                    ; No: bottom row in planning
        movwf   win_height

        WIN_LEFT .85                    ; Full divemenu width
        movlw   .160-.85+1
        movwf   win_width

        clrf    win_color1              ; Fill with black
        clrf    win_color2

        goto	TFT_box

;-----------------------------------------------------------------------------
; Display the decoplan (simulator or divemode).
; Inputs: char_O_deco_table (array of stop times, in minutes)
;         decoplan_page = page number.
;
deco_show_plan_page:
        WIN_INVERT 0

        ;---- Is there deco stops ? ------------------------------------------
    	movff   char_O_first_deco_depth,WREG
    	iorwf   WREG
        bnz		deco_plan_show_1

        ;---- No Deco --------------------------------------------------------
    	TFT_STD_COLOR
        TEXT_SMALL   .80, .0, tNoDeco
        bsf     decoplan_last_ceiling_shown
        return

deco_plan_show_1:
    	lfsr	FSR0,char_O_deco_depth  ; Initialize indexed addressing.
	    lfsr	FSR1,char_O_deco_time

        movlw   .8                      ; 8 lines/page in decoplan
        btfsc   divemode
        movlw   .6                      ; 6 lines/page in divemode.
        movwf   decoplan_max

        clrf    decoplan_index          ; Start with index = 0
        clrf	win_top                 ; and row = 0

        ; Read stop parameters, indexed by decoplan_index and decoplan_page
        movf    decoplan_page,W         ; decoplan_gindex = 6*decoplan_page + decoplan_index
        mulwf   decoplan_max
        movf    decoplan_index,W
        addwf   PRODL,W
        movwf   decoplan_gindex         ; --> decoplan_gindex

        bcf     decoplan_last_ceiling_shown   ; Not finished yet...

deco_plan_show_2:
        btfsc   decoplan_gindex,5       ; Reached table length (32) ?
        bra     deco_plan_show_99       ; YES: finished...

        ; Read stop parameters, indexed by decoplan_index
        movf    decoplan_gindex,W       ; index
    	movff	PLUSW1,up               ; char_O_deco_time [gindex] --> up
	    movff	PLUSW0,lo               ; char_O_deco_depth[gindex]
        movf    lo,W
        bz      deco_plan_show_99       ; depth == 0 : finished.

        ; Display the stop line
    	rcall	deco_plan_show_stop

        ; Next
        movlw   .24
        addwf   win_top,F               ; row: += 24
	    incf	decoplan_index,F        ; local index += 1
	    incf	decoplan_gindex,F       ; global index += 1

        ; Max number of lines/page reached ?
    	movf    decoplan_max,W          ; index+1 == max ?
    	cpfseq	decoplan_index
    	bra		deco_plan_show_2         ; NO: loop

    	; Check if next stop if end-of-list ?
    	movf    decoplan_gindex,W
	    movf	PLUSW0,W                ; char_O_deco_depth[gindex]
    	bz      deco_plan_show_99         ; End of list...

        ; Display the message "more..."
        rcall   deco_plan_show_clear_bottom  ; Clear from next line

    	TFT_STD_COLOR
        TEXT_SMALL .85, .240-.25, tMore
        return

deco_plan_show_99:
        bsf		decoplan_last_ceiling_shown ; Nothing more in table to display.
        rcall   deco_plan_show_clear_bottom ; Clear from next line
        return

;-----------------------------------------------------------------------------
; Loop to show all pages of the decoplan (surfacemode)

        global  deco_show_plan
deco_show_plan:
        clrf    decoplan_page
        call    TFT_ClearScreen
        WIN_COLOR   color_greenish
        TEXT_SMALL  .1,.1, tDivePlan
        TFT_STD_COLOR
        WIN_LEFT    .0
        
        ;---- Display model
        movff   char_I_deco_model,WREG
        iorwf   WREG
        bnz     deco_show_plan_m1
        
        ; Display ZH-L16 sat/desat model.
        TEXT_SMALL  .0,.40,  tZHL16
        WIN_TOP .65
        lfsr    FSR2,buffer
        movff   char_I_desaturation_multiplier,lo
        bsf     leftbind
        output_8
        STRCAT  "%/"
        movff   char_I_saturation_multiplier,lo
        output_8
        STRCAT_PRINT  "%"
        bra     deco_show_plan_m2

        ; Display ZH-L16-GF low/high model.
deco_show_plan_m1:
        TEXT_SMALL  .0,.40,  tZHL16GF
        WIN_TOP .65
        lfsr    FSR2,buffer
        movff   char_I_GF_Low_percentage,lo
        output_99x
        STRCAT  "%/"
        movff   char_I_GF_High_percentage,lo
        output_99x
        STRCAT_PRINT  "%"
        ;bra     deco_show_plan_m2

deco_show_plan_m2:        
        
        ;---- Display TTS result
        WIN_TOP     .165
        STRCPY_TEXT tTTS
        STRCAT  ": "

        movff   int_O_ascenttime+0,lo
        movff   int_O_ascenttime+1,hi
        bsf     leftbind
        output_16
        STRCAT_PRINT "'"

        ;---- Display CNS result
        WIN_TOP     .190
        STRCPY_TEXT tCNS
        STRCAT  ": "
        movff   int_O_CNS_fraction+0,lo
        movff   int_O_CNS_fraction+1,hi
        output_16_3
        STRCAT  "%\x92"					; "->"
        movff   decoplan_CNS+0,lo
        movff   decoplan_CNS+1,hi
        output_16_3
        STRCAT_PRINT "%"
       
        ;---- Loop through pages
deco_show_plan_1:
        call    speed_normal
        rcall   deco_show_plan_page
        incf    decoplan_page,F

        call    logbook_preloop_tasks
deco_show_plan_2:
        btfsc   switch_right
        bra     deco_show_plan_3
        btfsc   switch_left
        bra     deco_show_plan_4
        call    log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
    	btfsc	sleepmode                       ; Timeout?
        goto    restart
        bra     deco_show_plan_2

deco_show_plan_3:
        btfss   decoplan_last_ceiling_shown
        bra     deco_show_plan_1

deco_show_plan_4:
        call    speed_normal            ; Display in fast mode.
        return

;=============================================================================
;
        global  do_demo_divemode
do_demo_divemode:
		extern	option_save_all
		call	option_save_all			; Save all settings into EEPROM before starting simulation
 		call    deco_push_tissues_to_vault
        banksel common                  ; Bank1

		bsf		restore_deco_data		; Restore tissue and CNS after sim

		bcf		pressure_refresh
		btfss	pressure_refresh					; Wait for sensor
		bra		$-2

		bsf		simulatormode_active				; Set Flag
		movlw	LOW			simulator_start_depth
		movff	WREG,rel_pressure+0
		movlw	HIGH		simulator_start_depth
		movff	WREG,rel_pressure+1					; Set Depth

		movlw	LOW			(simulator_start_depth+.1000)
		movff	WREG,sim_pressure+0
		movlw	HIGH		(simulator_start_depth+.1000)
		movff	WREG,sim_pressure+1					; Set Depth

		bsf		divemode
		goto	diveloop							; Switch into Divemode!


        END