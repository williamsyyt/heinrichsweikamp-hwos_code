;=============================================================================
;
;   File menu_tree.asm
;
;   OSTC menus
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-07-11 : [jDG] Creation.

#include    "hwos.inc"                  ; Mandatory header
#include    "gaslist.inc"
#include    "menu_processor.inc"
#include	"start.inc"
#include	"comm.inc"
#include	"logbook.inc"
#include	"tft.inc"
#include	"eeprom_rs232.inc"
#include	"external_flash.inc"
#include 	"shared_definitions.h"      ; Mailbox from/to p2_deco.c
#include	"isr.inc"
#include	"ghostwriter.inc"
#include    "adc_lightsensor.inc"

        CBLOCK  tmp+0x40                ; Keep space for menu processor
            gaslist_gas ; Check ram position in gaslist.asm, too!
        ENDC

gui     CODE
;=============================================================================
; Main Menu
        global  do_main_menu,do_main_menu2
do_main_menu:
        movff   menupos3,customview_surfmode; save last customview
do_main_menu2:
		bcf		sleepmode				; for timeout
		call    menu_processor_reset    ; restart from first icon.

do_continue_main_menu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last icon.

        extern  do_demo_divemode, restart
    MENU_BEGIN  tMainMenu, .7
        MENU_CALL   tLogbook,       logbook
        MENU_CALL   tGasSetup,      do_gas_menu
        MENU_CALL   tCCRSetup,      do_ccr_menu
        MENU_CALL   tPlan,          do_planner_menu_reset
        MENU_CALL   tDiveModeMenu,  do_divemode_menu
        MENU_CALL   tSystSets,      do_settings_menu
        MENU_CALL   tExit,          restart
    MENU_END

do_info_menu:
	MENU_BEGIN  tInfoMenu, .5
	    MENU_DYNAMIC    info_menu_serial,       0
	    MENU_DYNAMIC    info_menu_firmware,     0
	    MENU_DYNAMIC    info_menu_total_dives,  0
        MENU_DYNAMIC    info_menu_battery_volts,0
        MENU_CALL       tExit,                  do_return_settings
    MENU_END

;=============================================================================
; CCR Setup

return_ccr_menu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last gas.

do_ccr_menu:
    bcf     menu_show_sensors           ; Clear flag
    bcf     menu_show_sensors2          ; Clear flag
    btfsc   analog_o2_input
    bra     do_ccr_menu_cR
    btfss   optical_input
    bra     do_ccr_menu_ostc2

    MENU_BEGIN  tCCRSetup, .5          ; OSTC3 menu
        MENU_OPTION     tCCRMode,    oCCRMode,    0
        MENU_CALL       tCCRSensor,             do_ccr_sensor
        MENU_CALL       tDiluentSetup,          do_diluent_setup
        MENU_CALL       tFixedSetpoints,        do_fixed_setpoints
        MENU_CALL       tExit,                  do_continue_main_menu
    MENU_END

do_ccr_menu_cR:                         ; cR menu
    MENU_BEGIN  tCCRSetup, .6
        MENU_OPTION     tCCRMode,    oCCRMode,    0
        MENU_CALL       tCCRSensor,             do_ccr_sensor
        MENU_CALL       tCalibrateMenu,         do_calibrate_menu
        MENU_CALL       tDiluentSetup,          do_diluent_setup
        MENU_CALL       tFixedSetpoints,        do_fixed_setpoints
        MENU_CALL       tExit,                  do_continue_main_menu
    MENU_END

do_ccr_menu_ostc2:
    MENU_BEGIN  tCCRSetup, .3           ; ostc2 menu
        MENU_CALL       tDiluentSetup,          do_diluent_setup
        MENU_CALL       tFixedSetpoints,        do_fixed_setpoints
        MENU_CALL       tExit,                  do_continue_main_menu
    MENU_END


do_calibrate_menu:
    call    enable_ir_s8                ; Enable IR/S8-Port
    bsf     menu_show_sensors2          ; Set flag
do_calibrate_menu2:
	MENU_BEGIN  tCalibrateMenu, .6
	    MENU_CALL       tDiveHudMask1,       0
        MENU_CALL       tDiveHudMask2,       0
        MENU_CALL       tDiveHudMask3,       0
        MENU_OPTION     tCalibrationGas,oCalGasO2,  0
        MENU_CALL       tCalibrate,                 do_calibrate_mix
        MENU_CALL       tExit,                      return_ccr_menu
    MENU_END

do_calibrate_mix:
    extern  calibrate_mix
    call    calibrate_mix               ; Calibrate with opt_calibration_O2_ratio, also calibrate S8 HUD if connected
    goto    restart                     ; Restart into surface mode

do_ccr_sensor:
    call    enable_ir_s8                ; Enable IR/S8-Port
    bsf     menu_show_sensors           ; Set flag
	MENU_BEGIN  tCCRSensor, .5
	    MENU_CALL       tDiveHudMask1,       0
        MENU_CALL       tDiveHudMask2,       0
        MENU_CALL       tDiveHudMask3,       0
        MENU_OPTION     tSensorFallback,oSensorFallback,  0
        MENU_CALL       tExit,               return_ccr_menu
    MENU_END

do_diluent_setup:
       bsf     ccr_diluent_setup       ; =1: Setting up Diluents ("Gas6-10")
       bcf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
       call    gaslist_cleanup_list    ; Takes care that only one gas can be first and first has 0m change depth
     MENU_BEGIN  tDiluentSetup, .6
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_CALL       tExit,                  return_ccr_menu
    MENU_END

do_return_fixed_setpoints:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last gas.

do_fixed_setpoints:
        bcf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
     MENU_BEGIN  tFixedSetpoints, .6
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_edit_sp_menu
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_edit_sp_menu
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_edit_sp_menu
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_edit_sp_menu
        MENU_DYNAMIC    gaslist_strcat_setpoint, do_edit_sp_menu
        MENU_CALL       tExit,                   return_ccr_menu
    MENU_END

do_edit_sp_menu:
        call    gaslist_setSP              ; Save current item.
    MENU_BEGIN  tFixedSetpoints, .5
        MENU_DYNAMIC    gaslist_strcat_setpoint_0,0
        MENU_CALL       tSPPlus,                gaslist_spplus
        MENU_CALL       tDepthPlus,             gaslist_spdepthplus
        MENU_CALL       tDepthMinus,            gaslist_spdepthminus
        MENU_CALL       tExit,                  do_return_fixed_setpoints
    MENU_END

;=============================================================================
; OC Gas Setup

return_gas_menu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last gas.

        btfsc   ccr_diluent_setup       ; Return to CCR-Menu?
        bra     do_diluent_setup        ; Yes.
do_gas_menu:
        bcf     ccr_diluent_setup       ; =1: Setting up Diluents ("Gas6-10")
        bcf     short_gas_decriptions   ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
        call    gaslist_cleanup_list    ; Takes care that only one gas can be first and first has 0m change depth
    MENU_BEGIN  tGaslist, .6
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_DYNAMIC    gaslist_strcat_gas_mod, do_edit_gas_menu
        MENU_CALL       tExit,                  do_continue_main_menu
    MENU_END

return_gas_depth:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last gas.
        bra     do_edit_gas_menu_1

do_edit_gas_menu:
    call    gaslist_setgas              ; Save current item.
do_edit_gas_menu_1:                     ; Keep current gas.
    MENU_BEGIN          tGasEdit, .6
        MENU_DYNAMIC    gaslist_gastitle,       0
        MENU_DYNAMIC    gaslist_MOD_END,        0
        MENU_DYNAMIC    gaslist_show_type,      gaslist_toggle_type
        MENU_CALL       tSetup_mix,             do_setup_mix
        MENU_CALL       tGasDepth,              do_gas_depth_menu
        MENU_CALL       tExit,                  return_gas_menu
    MENU_END

do_setup_mix:
    MENU_BEGIN          tGasEdit, .7
        MENU_DYNAMIC    gaslist_gastitle,       0
        MENU_DYNAMIC    gaslist_MOD_END,        0
        MENU_CALL       tO2Plus,                gaslist_pO2
        MENU_CALL       tO2Minus,               gaslist_mO2
        MENU_CALL       tHePlus,                gaslist_pHe
        MENU_CALL       tHeMinus,               gaslist_mHe
        MENU_CALL       tExit,                  return_gas_depth
    MENU_END

    global  do_gas_depth_menu
do_gas_depth_menu:
    movff   gaslist_gas,WREG
    lfsr    FSR1,opt_gas_type       ; Read opt_gas_type[WREG]
    movff   PLUSW1,lo               ; Used as temp
    movlw   .3                      ; 3=Deco
    btfsc   ccr_diluent_setup       ; =1: Setting up Diluents ("Gas6-10")
    movlw   .2                      ; 2=Normal
    cpfseq  lo
    bra     return_gas_depth        ; Non-Deco gas or "Normal" Diluent, Return!

    MENU_BEGIN  tGasEdit, .7
        MENU_DYNAMIC    gaslist_gastitle,       0
        MENU_DYNAMIC    gaslist_MOD_END,        0
        MENU_DYNAMIC    gaslist_ppo2,           0               ; ppO2 at change depth
        MENU_CALL       tDepthPlus,             gaslist_pDepth
        MENU_CALL       tDepthMinus,            gaslist_mDepth
        MENU_DYNAMIC    gaslist_reset_mod_title,gaslist_reset_mod
        MENU_CALL       tExit,                  return_gas_depth
    MENU_END

;=============================================================================
; Simulator menus

        global  do_planner_menu

do_planner_menu_reset:
		extern	option_save_all
		call	option_save_all
        ;---- Reset dive time/depth to default values
        extern  option_reset
        lfsr    FSR0,odiveInterval
        call    option_reset
        lfsr    FSR0,obottomTime
        call    option_reset
        lfsr    FSR0,obottomDepth
        call    option_reset

do_planner_menu:
    extern  do_demo_planner
    MENU_BEGIN  tPlan, .7
		MENU_CALL       tInter,                 do_demo_divemode
        MENU_OPTION     tIntvl, odiveInterval,  0
        MENU_OPTION     tBtTm,  obottomTime,    0
        MENU_OPTION     tMxDep, obottomDepth,   0
        MENU_CALL       tDeco,                  do_demo_planner
        MENU_CALL       tSystSets,              do_planner_config
        MENU_CALL       tExit,                  do_continue_main_menu
    MENU_END

do_planner_config:
    MENU_BEGIN  tPlan, .3
        MENU_OPTION     tSetBotUse, obottom_usage,  0
        MENU_OPTION     tSetDecoUse,  odeco_usage,  0
        MENU_CALL       tExit,                      do_planner_menu
    MENU_END


;=============================================================================
; Divemode menu

do_return_divemode_menu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to last gas.

do_divemode_menu:
    MENU_BEGIN  tDiveModeMenu, .7
        MENU_OPTION  tDvMode,    oDiveMode,     0
        MENU_OPTION  tDkMode,    oDecoMode,     0
        MENU_CALL    tppO2settings,          	do_ppo2_menu
        MENU_OPTION  tsafetystopmenu,oSafetyStop,    0
        MENU_OPTION  tFTTSMenu,                 oExtraTime,0
        MENU_CALL    tDecoparameters,          	do_decoparameters_menu
        MENU_CALL    tExit,                  	do_continue_main_menu
    MENU_END

do_ppo2_menu:
    MENU_BEGIN  tppO2settings, .3
		MENU_DYNAMIC divesets_ppo2_max,         do_toggle_ppo2_max
		MENU_DYNAMIC divesets_ppo2_min,         do_toggle_ppo2_min
        MENU_CALL    tExit,                  	do_return_divemode_menu
    MENU_END

do_return_decoparameters_menu:
        call    menu_processor_pop      ; drop exit line.
        call    menu_processor_pop      ; back to setting
do_decoparameters_menu:
    MENU_BEGIN  tDecoparameters, .7
        MENU_OPTION  tGF_low,   oGF_low,        0
        MENU_OPTION  tGF_high,  oGF_high,       0
        MENU_CALL    taGFMenu,                  do_aGF_menu
        MENU_OPTION  tSaturationMult,osatmult,  0
        MENU_OPTION  tDesaturationMult,odesatmult,0
        MENU_OPTION  tLastDecostop,oLastDeco,   0
        MENU_CALL    tExit,                  	do_return_divemode_menu
    MENU_END

do_aGF_menu:
    MENU_BEGIN  taGFMenu, .4
        MENU_OPTION  taGF_enable,oEnable_aGF,    0
        MENU_OPTION  taGF_low,   oaGF_low,       0
        MENU_OPTION  taGF_high,  oaGF_high,      0
        MENU_CALL    tExit,                  	do_return_decoparameters_menu
    MENU_END
;=============================================================================
; Setup Menu

do_return_settings:
		bcf		settime_setdate					; Clear flag
        call    menu_processor_pop              ; Drop exit entry
        call    menu_processor_pop              ; Pop return line.
        
        extern  compass_calibration_loop
do_settings_menu:
    MENU_BEGIN  tSystSets, .7
        MENU_CALL   tInfoMenu,      do_info_menu
        MENU_CALL   tSetTimeDate,   do_date_time_menu
        MENU_CALL   tDispSets,      do_dispsets_menu
        MENU_OPTION tLanguage,      oLanguage,       0
        MENU_CALL   tResetMenu,     do_reset_menu
        MENU_CALL   tMore,          do_settings_menu_more
        MENU_CALL   tExit,          do_continue_main_menu
    MENU_END

    
do_settings_menu_more:
    btfsc   rechargeable            ; piezo buttons available
    bra     do_settings_menu_more_piezo
    btfsc   ble_available           ; ble available
    bra     do_settings_menu_more_ostc3p
    MENU_BEGIN  tSystSets, .6
        MENU_CALL   tCompassMenu,   do_compass_menu
		MENU_CALL	tLogOffset,					do_log_offset_menu
        MENU_OPTION tUnits,    oUnits,          0
        MENU_OPTION tSamplingrate,oSamplingRate,0
        MENU_OPTION tDvSalinity,oDiveSalinity,  0
        MENU_CALL   tExit,          do_return_settings
    MENU_END

do_settings_menu_more_piezo:
    MENU_BEGIN  tSystSets, .7
        MENU_CALL   tCompassMenu,   do_compass_menu
		MENU_CALL	tLogOffset,					do_log_offset_menu
        MENU_OPTION tUnits,    oUnits,          0
        MENU_OPTION tSamplingrate,oSamplingRate,0
        MENU_OPTION tDvSalinity,oDiveSalinity,  0
        MENU_CALL   tMore,          do_settings_cr_menu
        MENU_CALL   tExit,          do_return_settings
    MENU_END

    extern  comm_mode0

do_settings_cr_menu:        
    btfsc   ble_available           ; ble available
    bra     do_settings_menu_more_ostc2
    ; Menu with features only available in cR hardware
    MENU_BEGIN  tSystSets, .4
        MENU_CALL   tUsbTitle,  comm_mode0
        MENU_OPTION tButtonleft,ocR_button_left  ,0  ; left button sensitivity
        MENU_OPTION tButtonright,ocR_button_right,0  ; right button sensitivity
        MENU_CALL   tExit,          do_settings_menu_more_piezo
    MENU_END

do_settings_menu_more_ostc2:
    ; Menu OSTC2
    MENU_BEGIN  tSystSets, .4
        MENU_CALL   tBleTitle,  comm_mode0
        MENU_OPTION tButtonleft,ocR_button_left  ,0  ; left button sensitivity
        MENU_OPTION tButtonright,ocR_button_right,0  ; right button sensitivity
        MENU_CALL   tExit,          do_settings_menu_more_piezo
    MENU_END
    

do_settings_menu_more_ostc3p:  ; Menu with BLE feature
    MENU_BEGIN  tSystSets, .7
        MENU_CALL   tCompassMenu,   do_compass_menu
		MENU_CALL	tLogOffset,					do_log_offset_menu
        MENU_OPTION tUnits,    oUnits,          0
        MENU_OPTION tSamplingrate,oSamplingRate,0
        MENU_OPTION tDvSalinity,oDiveSalinity,  0
        MENU_CALL   tBleTitle,  comm_mode0
        MENU_CALL   tExit,          do_return_settings
    MENU_END

do_compass_menu:
    MENU_BEGIN  tSystSets, .2
        MENU_CALL   tCompassMenu,   compass_calibration_loop
;        MENU_OPTION tCompassGain,   oCompassGain,       0
        MENU_CALL   tExit,          do_return_settings
    MENU_END

;=============================================================================
; Reset and confirmation menu.

do_reset_menu:
    MENU_BEGIN  tResetMenu, .6
        MENU_CALL       tExit,                  do_return_settings
        MENU_CALL       tReboot,	            do_reset_menu2			; Confirm
		MENU_CALL       tResetDeco,	            do_reset_menu3			; Confirm
		MENU_CALL       tResetSettings,	        do_reset_menu4			; Confirm
        MENU_CALL       tResetLogbook,	        do_reset_menu5			; Confirm
        MENU_CALL       tResetBattery,          new_battery_menu        ; New Battery submenu
    MENU_END

do_reset_menu2:
    MENU_BEGIN  tResetMenu2, .2
        MENU_CALL       tAbort,                 do_continue_menu_3stack
        MENU_CALL       tReboot,                do_reboot               ; Reboot
    MENU_END

do_reset_menu3:
    MENU_BEGIN  tResetMenu2, .2
        MENU_CALL       tAbort,                 do_continue_menu_3stack
        MENU_CALL       tResetDeco,             do_reset_deco			; Reset Deco
    MENU_END

do_reset_menu4:
    MENU_BEGIN  tResetMenu2, .2
        MENU_CALL       tAbort,                 do_continue_menu_3stack
        MENU_CALL       tResetSettings,         do_reset_settings		; Reset all settings
    MENU_END

do_reset_menu5:
    MENU_BEGIN  tResetMenu2, .2
        MENU_CALL       tAbort,                 do_continue_menu_3stack
        MENU_CALL       tResetLogbook,          do_reset_logbook		; Reset logbook
    MENU_END

do_reset_logbook:
	clrf    EEADRH                      ; Make sure to select eeprom bank 0
	clrf	EEDATA
    read_int_eeprom     .2
    write_int_eeprom    .15         
    read_int_eeprom     .3
    write_int_eeprom    .16             ; Copy number of dives
    clrf	EEDATA
    write_int_eeprom    .2
    write_int_eeprom    .3              ; Clear total dives
	write_int_eeprom	.4
	write_int_eeprom	.5
	write_int_eeprom	.6              ; Reset logbook pointers
	call	ext_flash_erase_logbook		; And complete logbook (!)
    goto	do_continue_main_menu		; back to menu


do_reset_deco:
	movlw	d'79'							; 79% N2
	movff	WREG,char_I_N2_ratio
	movlw	d'0'
	movff	WREG,char_I_step_is_1min		; 2 second deco mode
    SAFE_2BYTE_COPY amb_pressure,int_I_pres_respiration ; copy for deco routine
	movff	int_I_pres_respiration+0,int_I_pres_surface+0     ; copy for desat routine
	movff	int_I_pres_respiration+1,int_I_pres_surface+1		

	extern	deco_reset
	call	deco_reset
	call	deco_calc_desaturation_time     ; calculate desaturation time
	banksel common
	call	deco_calc_wo_deco_step_1_min	; calculate deco in surface mode 
	banksel common
  	clrf	nofly_time+0	              	; Reset NoFly
  	clrf	nofly_time+1
	clrf	desaturation_time+0				; Reset Desat
	clrf	desaturation_time+1
	goto	do_return_settings				; back to menu

do_reset_settings:
    banksel common
	extern	option_reset_all
    call	option_reset_all        	; Reset all options to factory default.
	goto	restart                     ; Restart into surfacemode

do_continue_menu_3stack:			; Return three levels deep
    call    menu_processor_pop
	goto	do_return_settings

do_reboot:
	call	ext_flash_enable_protection			; Enables write protection
	reset


do_date_time_menu:
    MENU_BEGIN  tSetTimeDate, .4
        MENU_CALL	tSetTime,               do_time_menu
        MENU_CALL   tSetDate,	            do_date_menu
		MENU_OPTION tDateFormat,oDateFormat,    0
        MENU_CALL   tExit,                  do_return_settings
    MENU_END

do_date_menu:
	bsf		settime_setdate
    MENU_BEGIN  tSetDate, .4
        MENU_OPTION tSetDay,	oSetDay,		0
		MENU_OPTION	tSetMonth,	oSetMonth,  	0
        MENU_OPTION tSetYear,	oSetYear,		0
        MENU_CALL   tExit,                      do_continue_menu_3stack
    MENU_END


do_reset_seconds:
	clrf	secs
	extern	rtc_set_rtc
	call	rtc_set_rtc				; writes mins,sec,hours,day,month and year to rtc module
do_time_menu:
	bsf		settime_setdate
    MENU_BEGIN  tSetTime, .4
        MENU_OPTION tSetHours,	oSetHours,		0
		MENU_OPTION	tSetMinutes,oSetMinutes,	0
		MENU_CALL   tSetSeconds,                do_reset_seconds
        MENU_CALL   tExit,                      do_continue_menu_3stack
    MENU_END


do_toggle_ppo2_max:             ; add 0.1bar, with hard-coded max.
    movff   opt_ppO2_max,lo     ; banksafe
    movlw	.10
	addwf	lo,F
	movlw	ppo2_highest_setting
	cpfsgt	lo
    bra     do_toggle_ppo2_max2
	movlw	.120
	movwf	lo
do_toggle_ppo2_max2:
    movff   lo,opt_ppO2_max
    return

do_toggle_ppo2_min:             ; sub 0.1bar, with hard-coded min.
    movff   opt_ppO2_min,lo     ; banksafe
    incf    lo,F
	movlw	.21
	cpfsgt	lo
    bra     do_toggle_ppo2_min2
	movlw	ppo2_lowest_setting
	movwf	lo
do_toggle_ppo2_min2:
    movff   lo,opt_ppO2_min
    return


	; Logbook offset sub-menu
do_log_offset_menu:
	MENU_BEGIN  tLogOffset, .6
        MENU_DYNAMIC    TFT_LogOffset_Logtitle,      0
		MENU_CALL       tLogOffsetp1,				do_logoffset_plus1
		MENU_CALL       tLogOffsetp10,				do_logoffset_plus10
		MENU_CALL       tLogOffsetm1,				do_logoffset_minus1
		MENU_CALL       tLogOffsetm10,				do_logoffset_minus10
        MENU_CALL       tExit,                  	do_dispsets_menu_3stack
    MENU_END


do_logoffset_minus1:
	call	do_logoffset_common_read			; Read into lo:hi
	movlw	d'1'
	subwf	lo
	movlw	d'0'
	subwfb	hi
	btfss	hi,7								; <0?
	goto	do_logoffset_common_write			; Store and return
	clrf	lo
	clrf	hi
	goto	do_logoffset_common_write			; Store and return

do_logoffset_minus10:
	call	do_logoffset_common_read			; Read into lo:hi
	movlw	d'10'
	subwf	lo
	movlw	d'0'
	subwfb	hi
	btfss	hi,7								; <0?
	goto	do_logoffset_common_write			; Store and return
	clrf	lo
	clrf	hi
	goto	do_logoffset_common_write			; Store and return

do_logoffset_plus1:
	call	do_logoffset_common_read			; Read into lo:hi
    infsnz  lo,F
    incf    hi,F
	goto	do_logoffset_common_write			; Store and return

do_logoffset_plus10:
	call	do_logoffset_common_read			; Read into lo:hi
	movlw	d'10'
	addwf	lo
	movlw	d'0'
	addwfc	hi
	goto	do_logoffset_common_write			; Store and return

do_dispsets_menu_3stack:
    bcf		in_color_menu
    call    menu_processor_pop
    call    menu_processor_pop

do_dispsets_menu:
    MENU_BEGIN  tDispSets, .5
        MENU_OPTION tBright,       oBrightness,   0
        MENU_CALL   tColorScheme,                 do_color_scheme
        MENU_OPTION tFlip,         oFlipScreen,   0
        MENU_OPTION tMODwarning,   oDepthBlink,   0
        MENU_CALL   tExit,                        do_return_settings
    MENU_END


    extern  oColorSetDive
do_color_scheme:
    bsf		in_color_menu
	MENU_BEGIN  tColorScheme, .2
        MENU_OPTION     tColorSetDive,oColorSetDive,  0
        MENU_CALL       tExit,                  	do_dispsets_menu_3stack
    MENU_END


;=============================================================================

	global	new_battery_menu
new_battery_menu:
    bsf     enable_screen_dumps     ; To prevent exiting into COMM mode immediately
	call	TFT_boot                ; Initialize TFT (includes clear screen)
	call    TFT_Display_FadeIn      ; Show splash
    movlw   .100
    movwf   batt_percent            ; make sure to reset batt_percent

    ; Default (In cases of timeout or USB): Use old battery
	clrf	EEADRH
	read_int_eeprom 0x07
	movff	EEDATA,battery_gauge+0
	read_int_eeprom 0x08
	movff	EEDATA,battery_gauge+1
	read_int_eeprom 0x09
	movff	EEDATA,battery_gauge+2
	read_int_eeprom 0x0A
	movff	EEDATA,battery_gauge+3
	read_int_eeprom 0x0B
	movff	EEDATA,battery_gauge+4
	read_int_eeprom 0x0C
	movff	EEDATA,battery_gauge+5

    call    menu_processor_reset    ; restart from first icon.

    MENU_BEGIN tNewBattTitle, .3
		MENU_CALL   tNewBattOld,                 use_old_batteries
        MENU_CALL   tNewBattNew36,               use_new_36V_batteries
        MENU_CALL   tNewBattNew15,               use_new_15V_batteries
    MENU_END

	global	use_old_batteries
use_old_batteries:
	clrf	EEADRH
	read_int_eeprom 0x07
	movff	EEDATA,battery_gauge+0
	read_int_eeprom 0x08
	movff	EEDATA,battery_gauge+1
	read_int_eeprom 0x09
	movff	EEDATA,battery_gauge+2
	read_int_eeprom 0x0A
	movff	EEDATA,battery_gauge+3
	read_int_eeprom 0x0B
	movff	EEDATA,battery_gauge+4
	read_int_eeprom 0x0C
	movff	EEDATA,battery_gauge+5
    movlw   .100
    movwf   batt_percent                ; To have 1,5V batteries right after firmware update
	goto	power_on_return

use_new_15V_batteries:
use_new_36V_batteries:
    call    reset_battery_pointer       ; Resets battery pointer 0x07-0x0C and battery_gauge:5
	goto	power_on_return

    END