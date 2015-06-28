;=============================================================================
;
;   File tft_outputs.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-07 : [mH] moving from OSTC code

; Macros
	extern	TFT_color_code1
TFT_color_code macro color_code_temp
	movlw	color_code_temp
	call	TFT_color_code1
	endm

; Surface mode
    extern  TFT_show_OC_startgas_surface   ; Show first gas and "OSTC2-like" active gases
	extern	TFT_serial
	extern	TFT_clock
	extern	TFT_date
	extern	TFT_desaturation_time
	extern	TFT_nofly_time
    extern  TFT_interval
	extern	TFT_gaslist_surfmode
    extern  TFT_dillist_surfmode
    extern  TFT_splist_surfmode             ; Show Setpoint list
	extern	TFT_temp_surfmode
	extern	TFT_update_batt_voltage
	extern	TFT_display_decotype_surface
    extern  TFT_display_decotype_surface1   ; Used from logbook!
	extern	TFT_update_surf_press
    extern  TFT_surface_sensor             ; Update HUD data in surface mode
;    extern  TFT_clear_customview_surfacemode
    extern  TFT_menu_hud                ; Update HUD data in menu (including mV)
    extern  TFT_menu_calibrate               ; Update mV data in menu
    extern  TFT_custom_text             ; The custom text
    extern  TFT_surface_tissues         ; Show Tissue diagram in surface mode
;    extern  TFT_surface_compass_mask    ; Compass heading mask
;    extern  TFT_surface_compass_heading ; Compass heading value
    extern  TFT_surface_decosettings    ; Show all deco settings
    extern  TFT_cat_serial              ; Add serial to current string
    extern  TFT_cat_firmware            ; Add firmware version to current string
;    extern  TFT_compass_show_gain       ; Show the current compass gain
;    extern  TFT_show_timeout_testmode   ; Show the timeout during calibration
; Menu
	extern	TFT_LogOffset_Logtitle
    extern  TFT_show_color_schemes      ; Yes, update the color schemes

; Debug and useless stuff
;	extern	TFT_update_raw_data
;    extern	TFT_update_raw_data2
;    extern  TFT_compass_fast

; Dive mode
	extern	TFT_display_cns
	extern	TFT_divemins
	extern	TFT_depth
	extern	TFT_temp_divemode
	extern	TFT_max_pressure
	extern	TFT_active_gas_divemode
	extern	TFT_divemode_mask
	extern	TFT_display_ndl_mask
	extern	TFT_show_TTS_divemode
	extern	TFT_display_ndl
	extern	TFT_display_deko_mask
	extern	TFT_divemode_warning
	extern	TFT_divemode_warning_clear
	extern	TFT_velocity_clear
	extern	TFT_display_velocity
	extern	TFT_clear_customview_divemode
	extern	TFT_display_deko
	extern	TFT_mask_ppo2
	extern	TFT_display_ppo2
	extern	TFT_display_ppo2_val
	extern	TFT_update_batt_percent_divemode
	extern	TFT_display_apnoe_descent
	extern	TFT_apnoe_clear_surface
	extern	TFT_display_apnoe_surface
	extern	TFT_display_apnoe_last_max
	extern	TFT_ftts
    extern  TFT_clear_warning_text
    extern  TFT_clear_warning_text_2nd_row
    extern  TFT_warning_gf
    extern  TFT_update_ppo2_sensors             ; Update Sensor data
    extern  TFT_hud_mask                        ; The HUD mask
    extern  TFT_mask_avr_stopwatch              ; Show mask for average depth and stopwatch
    extern  TFT_update_avr_stopwatch            ; Update average depth and stopwatch
    extern  TFT_decoplan                        ; Decoplan
    extern  TFT_clear_divemode_menu             ; Clear the divemode menu
    extern  TFT_draw_gassep_line                ; Draw the gas separator grid line in spec mode only
    extern  TFT_divemode_menu_cursor            ; the divemode cursor
    extern  TFT_ead_end_tissues_clock_mask      ; Setup Mask
    extern  TFT_ead_end_tissues_clock           ; Show EAD/END, Tissues and clock
    extern  TFT_gf_mask                         ; Setup Mask
    extern  TFT_gf_mask_cGF                     ; Setup Mask - current GF only
    extern  TFT_gf_info                         ; Show GF informations
    extern  TFT_warning_agf                     ; Show a warning if aGF is selected
;    extern  TFT_dive_compass_mask               ; Compass mask
;    extern  TFT_dive_compass_heading            ; Compass heading
    extern  TFT_dyn_gaslist                     ; Show the dyn gaslist
    extern  TFT_divetimeout                     ; Show timeout counter
    extern  TFT_hud_voltages                    ; Show HUD details
    extern  TFT_show_safety_stop                ; Show the safety stop
    extern  TFT_clear_decoarea                  ; Cleanup deco area on screen
    extern  TFT_ceiling_mask                    ; The ceiling mask
    extern  TFT_ceiling                         ; Ceiling
    extern  TFT_clear_safety_stop               ; Clear safety stop
    extern  TFT_warning_fallback                ; Show fallback warning

; Misc
    extern	TFT_standard_color
    extern	TFT_disabled_color
    extern	TFT_attention_color
    extern	TFT_warnings_color
    extern      TFT_divemask_color
    extern	adjust_depth_with_salinity          ; computes salinity setting into lo:hi [mbar]
    extern	adjust_depth_with_salinity_log      ; computes salinity setting (FROM WREG!) into lo:hi [mbar]
    extern	TFT_convert_date_short
    extern	TFT_convert_signed_16bit
    extern	TFT_convert_date
    extern	TFT_debug_output
    extern	TFT_show_time_date_menu
    extern	convert_mbar_to_feet   			; convert value in lo:hi from mbar to feet
    extern	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit