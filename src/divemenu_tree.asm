;=============================================================================
;
;   File divemenu_tree.asm
;
;   OSTC3 dive mode menu
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2013-02-02 : [mH] Made out of menu_tree.asm

#include    "ostc3.inc"                  ; Mandatory header
#include    "menu_processor.inc"
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include    "tft_outputs.inc"
#include    "customview.inc"
#include    "strings.inc"

divegui     CODE
;=============================================================================
; Main Menu
        global  do_main_divemenu
do_main_divemenu:
		call    menu_processor_reset    ; restart from first icon.

do_continue_main_divemenu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; drop exit line.

        btfsc   FLAG_ccr_mode
        bra     main_divemenu_ccr       ; CCR Menu

        bcf     ccr_diluent_setup      ; For OC gases
        bcf     is_bailout_menu
        movlw   .1
        movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tMainMenu, .4
        MENU_CALL   tDivemenu_Gaslist,  do_divemode_gaslist
        MENU_CALL   tDivemenu_ResetAvr, do_divemode_resetavr
        MENU_CALL   tDivemenu_ToggleGF, do_divemode_togglegf
        MENU_CALL   tExit,              do_exit_divemode_menu
    MENU_END

main_divemenu_ccr:
    bsf     ccr_diluent_setup      ; For diluents
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tMainMenu, .6
        MENU_CALL   tDiveBailout,       do_divemode_gaslist_bail
        MENU_CALL   tDivemenu_Setpoint, do_divemode_splist
        MENU_CALL   tDivemenu_Gaslist,  do_divemode_gaslist
        MENU_CALL   tDivemenu_ResetAvr, do_divemode_resetavr
        MENU_CALL   tDivemenu_ToggleGF, do_divemode_togglegf
        MENU_CALL   tExit,              do_exit_divemode_menu
    MENU_END


do_togglegf:
    TSTOSS  opt_enable_aGF          ; =1: aGF can be selected underwater
    bra     do_exit_divemode_menu   ; exit
    bsf     toggle_gf               ; Set flag...
    bra     do_exit_divemode_menu   ; ...and exit

do_switch_to_sensor:
    movlw   .1                      ; Switch to Sensor
    movff   WREG,opt_ccr_mode       ; =0: Fixed SP, =1: Sensor
    bra     do_switch_sp2

do_divemode_resetavr:
    bsf     reset_average_depth     ; Set Flag
    bra     do_exit_divemode_menu   ; And exit


do_switch_gas6:
    movlw   .6
    movwf   active_gas              ; Gas6 selected
    bra     do_switch_gasX
    extern  diveloop_loop4
    extern  timeout_divemode_menu2
do_switch_gas:
    bsf     divemode_gaschange      ; Set flag
do_switch_gasX:
    btfsc   is_bailout_menu         ; Bailout confirmed?
    bsf     is_bailout              ; =1: Bailout
do_exit_divemode_menu:
    call    timeout_divemode_menu2
    clrf    STKPTR
    goto    diveloop_loop4

do_switch_sp:
    decf    menupos,W               ; 1-5 -> 0-4
    lfsr    FSR1,char_I_setpoint_cbar
    movff   PLUSW1,char_I_const_ppO2; Setup fixed Setpoint
    bsf     setpoint_changed        ; Set flag (For profile)
    bsf		event_occured			; Set global event byte

    ; Reconfigure last diluent
    extern  setup_dil_registers
    bcf     is_bailout              ; =1: Bailout
    movff   active_diluent,WREG     ; As a backup when switching back from Bailout to CCR
    decf    WREG                    ; 0-4
    call    setup_dil_registers     ; With WREG=Gas 0-4

    clrf    WREG                    ; Switch to fixed SP
    movff   WREG,opt_ccr_mode       ; =0: Fixed SP, =1: Sensor
    clrf    WREG
    movff   WREG,char_O_deco_status ; Restart decoplan computation

do_switch_sp2:
    ; Clear some flags in case we were in bailout before...
    bcf     is_bailout              ; =1: Bailout
    bcf     is_bailout_menu         ;
    bcf		better_gas_available	;=1: A better gas is available and a gas change is advised in divemode
    bcf     blinking_better_gas     ; Clear blinking flag
    bra     do_exit_divemode_menu   ; And exit

do_divemode_gaslist_bail:
    bcf     ccr_diluent_setup       ; For OC gases
    bsf     is_bailout_menu         ; =1: Bailout
do_divemode_gaslist:
    btfsc   is_bailout              ; In Bailout case?
    bcf     ccr_diluent_setup       ; Yes, use OC gases
    bsf     short_gas_decriptions
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tGaslist, .6
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_switch_gas
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_switch_gas
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_switch_gas
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_switch_gas
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_switch_gas
        MENU_CALL       tMore,                  do_divemode_gaslist_more0
    MENU_END

do_divemode_gaslist_more0:
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
do_divemode_gaslist_more:
    MENU_BEGIN  tGaslist, .6
        MENU_DYNAMIC    gaslist_strcat_gasx,    do_dive_nothing
        MENU_CALL       tO2Plus,                do_dive_pO2
        MENU_CALL       tO2Minus,               do_dive_mO2
        MENU_CALL       tHePlus,                do_dive_pHe
        MENU_CALL       tHeMinus,               do_dive_mHe
        MENU_CALL       tEnter,                 do_switch_gas6
    MENU_END

do_dive_nothing:
        bra     do_divemode_gaslist_more

do_dive_pO2:
        banksel char_I_O2_ratio
        incf    char_I_O2_ratio,F            ; O2++
        movf    char_I_He_ratio,W
        addwf   char_I_O2_ratio,W
        movwf   temp_bankx400
        movlw   .101
        cpfslt  temp_bankx400                ; O2+He<101?
        decf    char_I_O2_ratio,F            ; O2-- (Unchanged)
;        bra     do_divemode_gaslist_more_common
do_divemode_gaslist_more_common:
    	movf    char_I_O2_ratio,W       ; Add O2...
        addwf   char_I_He_ratio,W       ; ...and He...
    	sublw   .100                    ; ...subtract both from 100
    	movwf   char_I_N2_ratio         ; -> N2!
        banksel common
        bsf     gas6_changed                ; Set flag
        bra     do_divemode_gaslist_more

do_dive_mO2:
        banksel char_I_O2_ratio
        decf    char_I_O2_ratio,F           ; O2--
        movlw   gaslist_min_o2
        cpfslt  char_I_O2_ratio
        bra     do_divemode_gaslist_more_common
        movlw   gaslist_min_o2
        movwf   char_I_O2_ratio
        bra     do_divemode_gaslist_more_common


do_dive_pHe:
        banksel char_I_O2_ratio
        incf    char_I_He_ratio,F            ; He++
        movf    char_I_He_ratio,W
        addwf   char_I_O2_ratio,W
        movwf   lo
        movlw   .101
        cpfslt  lo                           ; O2+He<101?
        decf    char_I_He_ratio,F            ; Yes, He-- (Unchanged)
        bra     do_divemode_gaslist_more_common

do_dive_mHe:
        banksel char_I_O2_ratio
        decf    char_I_He_ratio,F           ; He--
        bnn     do_divemode_gaslist_more_common
        clrf    char_I_He_ratio
        bra     do_divemode_gaslist_more_common

do_divemode_splist:
    bsf     short_gas_decriptions
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tGaslist, .6
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_switch_sp
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_switch_sp
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_switch_sp
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_switch_sp
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_switch_sp
        MENU_CALL   tCCRSensor,         do_divemode_sensor
  MENU_END

do_divemode_sensor:
    ; Set customview to 1 (HUD Data)
    clrf    menupos3                    ; customview to come-1
    bsf     toggle_customview			; Set flag, the customview will be toggled very soon now...
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tGaslist, .2
        MENU_CALL       tDivemenu_UseSensor,    do_switch_to_sensor
        MENU_CALL       tExit,                  do_continue_main_divemenu
    MENU_END

do_divemode_togglegf:
    ; Set customview to 5 (GF informations)
    movlw   .4
    movwf   menupos3                    ; Customview to come-1
    bsf     toggle_customview			; Set flag, the customview will be toggled very soon now...
    movlw   .1
    movwf   menupos                 ; Set to first option in divemode menu
    MENU_BEGIN  tDivemenu_ToggleGF, .2
        MENU_CALL       tDivemenu_ToggleGF,     do_togglegf
        MENU_CALL       tExit,                  do_continue_main_divemenu
    MENU_END

    END