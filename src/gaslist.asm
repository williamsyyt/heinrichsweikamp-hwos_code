;=============================================================================
;
;   File gaslist.asm
;
;   Managing OSTC3 gas list
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-08-11 : [jDG] Creation.

#include    "ostc3.inc"                  ; Mandatory header
#include    "convert.inc"
#include    "math.inc"                  ; div16x16 for MOD calculations
#include    "strings.inc"
#include    "tft.inc"
#include    "tft_outputs.inc"

#include    "shared_definitions.h"

        CBLOCK  tmp+0x40                ; Keep space for menu processor
            gaslist_gas
            gaslist_O2
            gaslist_He
            gaslist_depth
            gaslist_Type
		; Reserved to tmp+0x5F
        ENDC

    extern	convert_mbar_to_feet

gui     CODE
;=============================================================================
; Append gas description to current string.
; Input: PRODL : gas number (0..4)
;        FSR2 : Current string position.
; Output: Text appended into buffer pointed by FSR2.
        extern  customview_show_mix
        global  gaslist_strcat_gas
gaslist_strcat_gas:
        rcall   gaslist_setgas          ; Sets gaslist_gas (0-4 for OC/Bailout, 5-9 for Diluents)
        ; Retrieve gas definition:
gaslist_strcat_gas_0:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_O2_ratio   ; Read opt_gas_O2_ratio[WREG]
        movff   PLUSW1,gaslist_O2
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_He_ratio   ; Read opt_gas_He_ratio[WREG]
        movff   PLUSW1,gaslist_He

        movff   gaslist_O2,lo
        movff   gaslist_He,hi
        call    customview_show_mix     ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
        return


;=============================================================================
; Append current mix to current string (For divemode)
; Input: FSR2 : Current string position.
; Output: Text appended into buffer pointed by FSR2.
    global  gaslist_strcat_gasx
gaslist_strcat_gasx:        ; Show current O2/He mix
        STRCAT_TEXT tGas
        STRCAT  ": "
        movff   char_I_O2_ratio,lo
        movff   char_I_He_ratio,hi
        call    customview_show_mix     ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
        return

    global  gaslist_show_type
    extern  tGasDisabled
    extern  tDilDisabled
gaslist_show_type:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_type       ; Read opt_gas_type[WREG]
        movff   PLUSW1,gaslist_Type
        STRCAT_TEXT tType
        lfsr    FSR1,tGasDisabled       ; Base text number
        btfsc   ccr_diluent_setup       ; In CCR setup?
        lfsr    FSR1,tDilDisabled       ; Base text number
        movff   gaslist_Type,WREG       ; 0-3
        rlncf   WREG                    ; x2
        addwf   FSR1L,F
        movlw   .0
        addwfc  FSR1H,F
        call    strcat_text
        return

    global  gaslist_toggle_type
gaslist_toggle_type:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_type       ; Read opt_gas_type[WREG]
        movff   PLUSW1,gaslist_Type
        incf    gaslist_Type,F          ; 0-3/0-2
        btfsc   ccr_diluent_setup       ; In CCR setup?
        bra     gaslist_toggle_type2    ; Yes
        btfsc   gaslist_Type,2          ; >3?
        clrf    gaslist_Type            ; Clear to zero
        movff   gaslist_Type,PLUSW1     ; Copy back result
        return

gaslist_toggle_type2:
        movlw   .3
        cpfslt  gaslist_Type            ; >2?
        clrf    gaslist_Type            ; Clear to zero
        movf    gaslist_gas,W
        movff   gaslist_Type,PLUSW1     ; Copy back result
        return

        global  gaslist_setSP
gaslist_setSP:
        movff   PRODL,gaslist_gas       ; 0-4
        return

        extern  tbar
        global  gaslist_strcat_setpoint
        global  gaslist_strcat_setpoint_0
gaslist_strcat_setpoint:
        call    gaslist_setSP
gaslist_strcat_setpoint_0:
        bsf     leftbind
        btfsc   short_gas_decriptions       ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
        bra     gaslist_strcat_setpoint2    ; Short version
        STRCAT_TEXT tSP
        incf    gaslist_gas,W
        movwf   lo
        output_8
        bcf     leftbind
        PUTC    ":"
gaslist_strcat_setpoint2:                   ; Short version
        lfsr    FSR1,opt_gas_type
        btfsc   divemode
        bra     gaslist_strcat_setpoint4    ; no "*" in divemode
        movf    gaslist_gas,W
        decf    PLUSW1,W                    ; Type-1 into WREG
        bnz     gaslist_strcat_setpoint3    ; Not "first"
        PUTC    "*"
        bra     gaslist_strcat_setpoint4
gaslist_strcat_setpoint3:
        PUTC    " "
gaslist_strcat_setpoint4:
        movf    gaslist_gas,W
        lfsr    FSR1,char_I_setpoint_cbar
        movf    PLUSW1,W
        movwf   lo
    	clrf	hi
        bsf     leftbind
    	output_16dp d'3'
        btfsc   divemode
        bra     gaslist_strcat_setpoint5        ; Skip text in divemode
        STRCAT_TEXT	 tbar
gaslist_strcat_setpoint5:
        PUTC    " "
       ; Read switch depth
        movf    gaslist_gas,W
        lfsr    FSR1,char_I_setpoint_change
        movff   PLUSW1,lo
        bra     gaslist_strcat_4        ; And return...

;----------------------------------------------------------------------------
; Append gas description to current string.
; Input: PRODL : gas number (0..4)
;        FSR2 : Current string position.
; Output: Text appended into buffer pointed by FSR2.
;
; NOTE: used in the menu-tree for the MENU_CALLBACK entry.

gaslist_strcat_gas_better:   ; Yes, check if this is a "better gas"
        movlw   .0
        movff   WREG,win_invert
        incf    gaslist_gas,W               ; gaslist_gas+1 -> WREG
        cpfseq  better_gas_number           ; 1-5 for OC/Bailout and 6-10 for diluents
        return
        call    TFT_attention_color         ; show in yellow
        movlw   .1
        movff   WREG,win_invert             ; and invert
        return

        global  gaslist_strcat_gas_mod
gaslist_strcat_gas_mod:
        rcall   gaslist_setgas          ; Sets gaslist_gas (0-4 for OC/Bailout, 5-9 for Diluents)

        global  gaslist_gastitle
gaslist_gastitle:
        btfsc   short_gas_decriptions       ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
        bra     gaslist_gastitle2           ; Short version
        STRCAT_TEXT tGas
        incf    gaslist_gas,W
        movwf   lo
        bsf     leftbind
        output_8
        bcf     leftbind
        PUTC    ":"

gaslist_gastitle2:                          ; Short version
        lfsr    FSR1,opt_gas_type
        btfsc   divemode
        bra     gaslist_gastitle3           ; no "*" in divemode
        movf    gaslist_gas,W
        decf    PLUSW1,W                    ; Type-1 into WREG
        bnz     gaslist_gastitle1           ; Not "First"
        PUTC    "*"
        bra     gaslist_gastitle3
gaslist_gastitle1:
        PUTC    " "
gaslist_gastitle3:                          ; Short version
        call    TFT_standard_color

        btfsc   divemode                    ; In divemode?
        rcall   gaslist_strcat_gas_better   ; Yes, check if this is a "better gas"

        movf    gaslist_gas,W           ; (0-4 for OC/Bailout, 5-9 for Diluents)
        movf    PLUSW1,W
        bnz     gaslist_strcat_3
        call    TFT_disabled_color
gaslist_strcat_3:
        rcall   gaslist_strcat_gas_0
		PUTC	" "
        ; Read switch depth
        movf    gaslist_gas,W           ; (0-4 for OC/Bailout, 5-9 for Diluents)
        lfsr    FSR1,char_I_deco_gas_change
        movff   PLUSW1,lo
        rcall   gaslist_calc_mod        ; Compute MOD into WREG
        cpfsgt  lo
        bra		gaslist_strcat_4
        call    TFT_warnings_color      ; Turn red if bigger
gaslist_strcat_4:
		TSTOSS	opt_units               ; 0=Meters, 1=Feets
		bra		gaslist_strcat_3_metric
;gaslist_strcat_3_imperial:
        movf    lo,W
        mullw   .100                    ; convert meters to mbar
        movff   PRODL,lo
        movff   PRODH,hi
		call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
        bsf     leftbind
        output_16_3
        STRCAT_TEXT	 tFeets				; "ft"
        return
		
gaslist_strcat_3_metric:
        output_99
        STRCAT_TEXT	tMeters				; "m"
        return

;----------------------------------------------------------------------------
; Store current menu item, and display gas description later.
; Input: PRODL : gas number (0..4)
; NOTE: used in the menu-tree for the MENU_CALLBACK entry.
        global  gaslist_setgas
gaslist_setgas:
        movff   PRODL,gaslist_gas
        movlw   .5
        btfsc   ccr_diluent_setup       ; in CCR menus?
        addwf   gaslist_gas,F           ; Yes, offset to gases 5-9
        return


    global  gaslist_cleanup_list    ; Takes care that only one gas can be first and first has 0m change depth
gaslist_cleanup_list:
        bcf     ignore_last_edited_gas
        movlw   .0
        btfsc   ccr_diluent_setup       ; In CCR-Menu?
        addlw   .5                      ; Yes, adjust offset
        subwf   gaslist_gas,F
gaslist_cleanup_list1:
        clrf    lo
        lfsr    FSR1,opt_gas_type       ; Read opt_gas_type[WREG]
        movlw   .5                      ; Check 5 gases
        movwf   hi
gaslist_cleanup_list2:
        decf    hi,w                    ; 0-4
        btfsc   ccr_diluent_setup       ; In CCR-Menu?
        addlw   .5                      ; Yes, adjust offset
        movff   PLUSW1,hi_temp
        movlw   .1
        cpfseq  hi_temp                 ; gas = first ?
        bra     gaslist_cleanup_list3   ; No
        incf    lo,F                    ; Yes, count "first gases"

        btfss   ignore_last_edited_gas  ; If we are not in the second-pass mode
        bra     gaslist_cleanup_list2b

        decf    hi,w                    ; 0-4
        cpfseq  gaslist_gas             ; Do not disable last edited gas
gaslist_cleanup_list2b:
        movff   hi,lo_temp              ; Keep the last "first gas" found
gaslist_cleanup_list3:
        decfsz  hi,F
        bra     gaslist_cleanup_list2   ; Loop

        tstfsz  lo                      ; No gas active?
        bra     gaslist_cleanup_list4   ; No, at least one is active

        btfsc   ccr_diluent_setup       ; In CCR-Menu?
        bra     gaslist_cleanup_list3a  ; Yes.
        ; make gas1 first and zero
        movlw   .1                      ; First
        movwf   lo_temp
        movwf   INDF1
        bra     gaslist_cleanup_list5   ; Set change depth to zero

gaslist_cleanup_list3a:
        movlw   .5
        addwf   FSR1L,F
        movlw   .0
        addwfc  FSR1H,F                 ; Setup to Diluents
        ; make dil1 first and zero
        movlw   .1                      ; First
        movwf   lo_temp
        movwf   INDF1
        bra     gaslist_cleanup_list5   ; Set change depth to zero

gaslist_cleanup_list4:
        movlw   .1
        cpfsgt  lo                      ; More then one "first gas"?
        bra     gaslist_cleanup_list5   ; No, done.
        ; More then one Gas is "first gas"
        ; Disable last found "first gas" but keep it's change depth
        decf    lo_temp,W               ; 0-4
        cpfseq  gaslist_gas             ; Do not disable last edited gas
        bra     gaslist_cleanup_list4b
        ; Do not disable last edited gas
        ; search again but ignore last edited gas
        bsf     ignore_last_edited_gas
        bra     gaslist_cleanup_list1   ; Loop until only one "first gas" is left

gaslist_cleanup_list4b:
        btfsc   ccr_diluent_setup       ; In CCR-Menu?
        addlw   .5                      ; Yes, adjust offset
        clrf    PLUSW1                  ; Disable gas
        bra     gaslist_cleanup_list    ; Loop until only one "first gas" is left

gaslist_cleanup_list5:
        lfsr    FSR1,char_I_deco_gas_change
        decf    lo_temp,W
        btfsc   ccr_diluent_setup       ; In CCR-Menu?
        addlw   .5                      ; Yes, adjust offset
        clrf    PLUSW1                  ; Set First gas to zero m
        return

;----------------------------------------------------------------------------
; Increment/Decrement O2 ratio
        global  gaslist_pO2
gaslist_pO2:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_He_ratio   ; Read opt_gas_He_ratio[WREG]
        movff   PLUSW1,gaslist_He
        lfsr    FSR1,opt_gas_O2_ratio   ; Read opt_gas_O2_ratio[WREG]
        movff   PLUSW1,gaslist_O2

        incf    gaslist_O2,F            ; O2++
        movf    gaslist_He,W
        addwf   gaslist_O2,W
        movwf   lo
        movlw   .101
        cpfslt  lo                      ; O2+He<101?
        decf    gaslist_O2,F            ; O2-- (Unchanged)
     
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; And write back to opt_gas_O2_ratio[WREG]
        return
        
        global  gaslist_mO2
gaslist_mO2:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_O2_ratio   ; Read opt_gas_O2_ratio[WREG]
        movff   PLUSW1,gaslist_O2

        decf    gaslist_O2,F
        movlw   gaslist_min_o2
        cpfslt  gaslist_O2
        bra     gaslist_mO2_1
        movlw   gaslist_min_o2
        movwf   gaslist_O2
gaslist_mO2_1:
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; And write back to opt_gas_O2_ratio[WREG]
        return

;----------------------------------------------------------------------------
; Increment/Decrement He ratio
        global  gaslist_pHe
gaslist_pHe:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_O2_ratio   ; Read opt_gas_O2_ratio[WREG]
        movff   PLUSW1,gaslist_O2
        lfsr    FSR1,opt_gas_He_ratio   ; Read opt_gas_He_ratio[WREG]
        movff   PLUSW1,gaslist_He

        incf    gaslist_He,F            ; He++
        movf    gaslist_He,W
        addwf   gaslist_O2,W
        movwf   lo
        movlw   .101
        cpfslt  lo                      ; O2+He<101?
        decf    gaslist_He,F            ; Yes, He-- (Unchanged)

        movf    gaslist_gas,W
        movff   gaslist_He,PLUSW1       ; And write back to opt_gas_He_ratio[WREG]
        return

        global  gaslist_mHe
gaslist_mHe:
        movf    gaslist_gas,W
        lfsr    FSR1,opt_gas_He_ratio   ; Read opt_gas_He_ratio[WREG]
        movff   PLUSW1,gaslist_He

        decf    gaslist_He,F
        bnn     gaslist_mHe_1
        clrf    gaslist_He
gaslist_mHe_1:
        movf    gaslist_gas,W
        movff   gaslist_He,PLUSW1       ; And write back to opt_gas_He_ratio[WREG]
        return

;----------------------------------------------------------------------------
; Increment/Decrement switch depth
        global  gaslist_pDepth
gaslist_pDepth:
        movf    gaslist_gas,W
        lfsr    FSR1,char_I_deco_gas_change
        movff   PLUSW1,gaslist_O2       ; Read char_I_deco_gas_change[WREG]

        incf    gaslist_O2,F
        movlw   gaslist_max_change_depth
        cpfsgt  gaslist_O2
        bra     gaslist_pDepth_1
        movlw   gaslist_max_change_depth
        movwf   gaslist_O2
gaslist_pDepth_1:
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; Write back to char_I_deco_gas_change[WREG]
        return
        
        global  gaslist_mDepth
gaslist_mDepth:
        movf    gaslist_gas,W
        lfsr    FSR1,char_I_deco_gas_change
        movff   PLUSW1,gaslist_O2       ; Read char_I_deco_gas_change[WREG]

        decf    gaslist_O2,F
        btfsc   STATUS,N
        clrf    gaslist_O2
       
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; And write back to char_I_deco_gas_change[WREG]
        return

    global  gaslist_spplus
gaslist_spplus:
        movf    gaslist_gas,W
        lfsr    FSR1,char_I_setpoint_cbar
        movff   PLUSW1,lo               ; Read char_I_setpoint_cbar[WREG]
        movlw	gaslist_sp_stepsize
        addwf	lo,F
        movlw	gaslist_sp_max
        cpfsgt	lo
        bra     gaslist_spplus2
        movlw	gaslist_sp_min
        movwf	lo
gaslist_spplus2:
        movf    gaslist_gas,W
        movff   lo,PLUSW1               ; Write back to char_I_setpoint_cbar
        return
    
    global  gaslist_spdepthplus
gaslist_spdepthplus:
        movf    gaslist_gas,W
        bz      gaslist_spdepthplus3    ; Setpoint 1 is always 0m
        lfsr    FSR1,char_I_setpoint_change
        movff   PLUSW1,gaslist_O2       ; Read char_I_deco_gas_change[WREG]
        incf    gaslist_O2,F
        movlw   gaslist_max_change_depth
        cpfsgt  gaslist_O2
        bra     gaslist_spdepthplus_1
        movlw   gaslist_max_change_depth
        movwf   gaslist_O2
gaslist_spdepthplus_1:
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; Write back to char_I_deco_gas_change[WREG]
        return

gaslist_spdepthplus3:
        movlw   .0
        movff   WREG,char_I_setpoint_change+0   ; Reset to 0m
        return

    global  gaslist_spdepthminus
gaslist_spdepthminus:
        movf    gaslist_gas,W
        bz      gaslist_spdepthplus3    ; Setpoint 1 is always 0m
        lfsr    FSR1,char_I_setpoint_change
        movff   PLUSW1,gaslist_O2       ; Read opt_gas_O2_ratio[WREG]
        decf    gaslist_O2,F
        btfsc   STATUS,N
        clrf    gaslist_O2
        movf    gaslist_gas,W
        movff   gaslist_O2,PLUSW1       ; And write back to opt_gas_O2_ratio[WREG]
        return

;----------------------------------------------------------------------------
; Compute MOD from ppO2 Max and current O2 Ratio.
;
; Input:  gaslist_gas = current gas index.
;         opt_gas_O2_ratio[gaslist_gas] = current O2 ratio
; Output: WREG = MOD [m]
;
gaslist_calc_mod:
        movf    gaslist_gas,W           ; Read current gas O2 ratio
        lfsr    FSR1,opt_gas_O2_ratio   ; Read opt_gas_O2_ratio[WREG]
        movf    PLUSW1,W

        btfsc   divemode                ; In divemode?
        bra     gaslist_calc_mod_divemode   ; Yes.

        ; Pamb max = ppO2 Max / O2 ratio
        movwf   xB+0
        clrf    xB+1

        movff   opt_ppO2_max,WREG
        mullw   .10
        movff   PRODL,xA+0
        movff   PRODH,xA+1
        call    div16x16
        
        ; Prof = Pamb - 1bar.
        movf    xC+0,W
        addlw   -.10
        return

gaslist_calc_mod_divemode:
    extern  TFT_color_code1
        movwf   hi                          ; Copy O2%
        movlw	warn_gas_in_gaslist
        call	TFT_color_code1             ; Color-code current row in Gaslist (%O2 in hi), opt_ppO2_max as threshold
        return
;----------------------------------------------------------------------------
        global  gaslist_MOD_END
gaslist_MOD_END:
        rcall   gaslist_calc_mod        ; Compute MOD into WREG
        movwf   lo                      ; Copy to lo
        STRCAT_TEXT tMOD                ; MOD:
   		TSTOSS	opt_units               ; 0=Meters, 1=Feets
		bra		gaslist_MOD_metric
;gaslist_MOD_imperial:
        movf    lo,W
        mullw   .100                    ; convert meters to mbar
        movff   PRODL,lo
        movff   PRODH,hi
		call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
        bsf     leftbind
        output_16_3
        STRCAT_TEXT	 tFeets				; "ft"
        bra     gaslist_MOD_common
gaslist_MOD_metric:
        output_8
        STRCAT_TEXT tMeters             ; m
gaslist_MOD_common:
        PUTC      "/"
        STRCAT_TEXT tEND                ; END:
        rcall    gaslist_calc_mod       ; Output: WREG = MOD [m]
        addlw   .10                     ; MOD=MOD+10m
        movwf   xB+0
        clrf    xB+1
    	movlw	d'100'
    	movwf	xA+0
    	movf	gaslist_He,W                ; He value in % -> WREG
    	subwf	xA+0,F                      ; xA+0 = 100 - He Value in %
    	clrf	xA+1
    	call	mult16x16                   ; xA*xB=xC
        movff	xC+0,xA+0
        movff	xC+1,xA+1
        movlw	d'100'
        movwf	xB+0
        clrf	xB+1
        call	div16x16                    ; xA/xB=xC with xA as remainder
        ;	xC:2 = ((MOD+10) * 100 - HE Value in %) / 100
        movlw	d'10'
        subwf	xC+0,F				        ; Subtract 10m...
        movff	xC+0,lo
; END 8Bit only
;    	movlw	d'0'
;    	subwfb	xC+1,F
;        movff	xC+1,hi
   		TSTOSS	opt_units               ; 0=Meters, 1=Feets
		bra		gaslist_END_metric
;gaslist_END_imperial:
        movf    lo,W
        mullw   .100                    ; convert meters to mbar
        movff   PRODL,lo
        movff   PRODH,hi
		call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
        bsf     leftbind
        output_16_3
        STRCAT_TEXT	 tFeets				; "ft"
        return
gaslist_END_metric:
        output_8
        STRCAT_TEXT tMeters             ; m
        return

;----------------------------------------------------------------------------
        global  gaslist_reset_mod_title
gaslist_reset_mod_title:
        STRCAT_TEXT tDepthReset

gaslist_reset_mod_title2:
        rcall   gaslist_calc_mod        ; Compute MOD into WREG
        movwf   lo                      ; Copy to lo

        movf    gaslist_gas,W           ; Compare to switch depth
        lfsr    FSR1,char_I_deco_gas_change
        movf   	PLUSW1,W
        cpfslt  lo
        bra     gaslist_strcat_4        ; And return...
        call    TFT_warnings_color      ; Turn red if bigger !
        bra     gaslist_strcat_4        ; And return...

;----------------------------------------------------------------------------
        global  gaslist_reset_mod
gaslist_reset_mod:
        rcall   gaslist_calc_mod        ; Compute MOD
        movwf   gaslist_depth

        movf    gaslist_gas,W           ; Read current gas O2 ratio
        lfsr    FSR1,char_I_deco_gas_change
        movff   gaslist_depth,PLUSW1    ; And save new change depth
        return
;----------------------------------------------------------------------------
        END