;=============================================================================
;
;   File divemode.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-15 : [mH] moving from OSTC code

 	extern 	set_dive_modes
 	extern	diveloop
	extern	apnoe_calc_maxdepth

; Divemode layout:
; row=0...239
; column=0...159 (x2)

#DEFINE	depth_row			.14
#DEFINE	depth_column		.0
#DEFINE	depth_feet_row		depth_row
#DEFINE	depth_feet_column	.0
#DEFINE	depth_dm_row		depth_row+.25
#DEFINE	depth_dm_column		.40

#DEFINE	max_depth_row           depth_row
#DEFINE	max_depth_column        .64
#DEFINE	max_depth_dm_column     .87
#DEFINE max_depth_dm_row        depth_row+.12
#DEFINE	max_depth_feet_row		max_depth_row
#DEFINE	max_depth_feet_column	.64

#DEFINE	divetime_row            depth_row
#DEFINE	divetime_column             .103        ; Mins:Secs
#DEFINE divetime_minsonly_column    .100        ; Minutes only
#DEFINE divetime_secs_column    .139
#DEFINE divetime_secs_row       depth_row+.12
#DEFINE	divetime_apnoe_row		depth_row
#DEFINE	divetime_apnoe_column	.103
#DEFINE apnoe_total_divetime_row            divetime_apnoe_row+.50
#DEFINE apnoe_total_divetime_column         divetime_apnoe_column
#DEFINE apnoe_total_divetime_secs_row       apnoe_total_divetime_row+.12
#DEFINE apnoe_total_divetime_secs_column    divetime_secs_column
#DEFINE total_apnoe_text_column             .132
#DEFINE total_apnoe_text_row                apnoe_total_divetime_row-.14


#DEFINE divemode_mask_depth_row         .0
#DEFINE divemode_mask_depth_column      .12
#DEFINE divemode_mask_maxdepth_row      .0
#DEFINE divemode_mask_maxdepth_column   .62
#DEFINE divemode_mask_divetime_row      .0
#DEFINE divemode_mask_divetime_column   .122

#DEFINE surface_apnoe_text_row      .190
#DEFINE surface_apnoe_text_column   .30
#DEFINE	surface_time_apnoe_row		.204
#DEFINE	surface_time_apnoe_column	.15

#DEFINE last_max_apnoe_text_row     .190
#DEFINE last_max_apnoe_text_column  .100
#DEFINE apnoe_last_max_depth_row	.204
#DEFINE	apnoe_last_max_depth_column	.100

#DEFINE	dive_temp_row		.163
#DEFINE	dive_temp_column	.2

; TTS
#DEFINE	tts_text_row		.211
#DEFINE	tts_text_column     .85
#DEFINE	tts_value_row		.207
#DEFINE	tts_value_column	.118

#DEFINE	ndl_text_row		tts_text_row
#DEFINE	ndl_text_column     tts_text_column
#DEFINE	ndl_value_row		tts_value_row
#DEFINE	ndl_value_column	tts_value_column

#DEFINE	active_gas_row		.210
#DEFINE	active_gas_column	.1
#DEFINE active_dil_row      .186
#DEFINE active_dil_column   .1

; Warnings
#DEFINE	warning_icon_row	.55
#DEFINE	warning_icon_column	.135
#DEFINE	warning1_row		.50
#DEFINE	warning1_column		.70
#DEFINE	warning2_row		.74
#DEFINE	warning2_column		warning1_column
#DEFINE warning_length      .9                 ; total string length

#DEFINE	velocity_text_row		.76
#DEFINE	velocity_text_column	.0

#DEFINE	divemode_customview_row		.101        ; Upper border
#DEFINE	divemode_customview_column	.0

#DEFINE dive_custom_hud_row         divemode_customview_row+.1
#DEFINE dive_custom_hud_column1     .10
#DEFINE dive_custom_hud_column2     .65
#DEFINE dive_custom_hud_column3     .120
#DEFINE dive_hud_data_row           divemode_customview_row+.16
#DEFINE dive_hud_sensor1_column     .6
#DEFINE dive_hud_sensor2_column     .62
#DEFINE dive_hud_sensor3_column     .118

; Customview Ceiling
#DEFINE dive_ceiling_text_row       divemode_customview_row+.1
#DEFINE dive_ceiling_text_column    .66
#DEFINE dive_ceiling_value_row      divemode_customview_row+.18
#DEFINE dive_ceiling_value_column   .64

; Customview GF
#DEFINE dive_gf_text_row            divemode_customview_row+.1
#DEFINE dive_gf_column1             .8
#DEFINE dive_gf_column2             .62
#DEFINE dive_gf_column3             .115
#DEFINE dive_gf_row                 divemode_customview_row+.18
#DEFINE dive_gf_column              .0
#DEFINE dive_agf_row                dive_gf_row
#DEFINE dive_agf_column             .60
#DEFINE dive_currentgf_row          dive_gf_row
#DEFINE dive_currentgf_column       .118

; Customview Dynamic Gaslist
#DEFINE dive_custom_dyn_mask_column .59
#DEFINE dive_custom_dyn_mask_row    decoplan_title_row
#DEFINE dive_custom_dyn_mask_row1   dive_hud_data_row
#DEFINE dive_custom_dyn_mask_column1   .0
#DEFINE dive_custom_dyn_mask_row2   dive_hud_data_row+.23
#DEFINE dive_custom_dyn_mask_column2   .80

; Customview clock/date, END/EAD and Tissues
#DEFINE dive_clock_row              dive_hud_data_row
#DEFINE dive_clock_column           .0
#DEFINE dive_endtime_row            dive_clock_row+.23
#DEFINE dive_endtime_column         dive_clock_column
#DEFINE dive_ead_row                dive_hud_data_row
#DEFINE dive_ead_column             .46
#DEFINE dive_end_row                dive_ead_row+.23
#DEFINE dive_end_column             dive_ead_column

#DEFINE dive_tissue_N2_row          dive_ead_row+.5
#DEFINE dive_tissue_N2_column       .105
#DEFINE dive_tissue_He_row          dive_end_row+.5
#DEFINE dive_tissue_He_column       dive_tissue_N2_column
#DEFINE tissue_diagram_top          divemode_customview_row+.16
#DEFINE tissue_diagram_bottom       .160
#DEFINE tissue_diagram_left         .116
#DEFINE tissue_diagram_frame_spacing    .8

#DEFINE dive_custom_avr_stop_row        dive_custom_hud_row
#DEFINE dive_custom_avr_stop_column1    .2
#DEFINE dive_custom_avr_stop_column2    .57
#DEFINE dive_custom_avr_stop_column3    .110
#DEFINE dive_avr_stop_row               dive_hud_data_row
#DEFINE dive_avr_stop_column1           .0
#DEFINE dive_avr_stop_column2           .54
#DEFINE dive_avr_stop_column3           .118

#DEFINE	decostop_1st_stop_row		.171		; In medium font...
#DEFINE	decostop_1st_stop_column	.82
#DEFINE	safetystop_row              .171		; In medium font...
#DEFINE	safetystop_column           .118

#DEFINE safetystop_text_row         .175
#DEFINE safetystop_text_column      .70

#DEFINE decoplan_title_row          .101
#DEFINE decoplan_title_column       .65

#DEFINE	decostop_2nd_stop_row		divemode_customview_row+.15
#DEFINE	decostop_2nd_stop_column	.0
#DEFINE	decostop_3rd_stop_row		divemode_customview_row+.39
#DEFINE	decostop_3rd_stop_column	decostop_2nd_stop_column
#DEFINE	decostop_4th_stop_row		decostop_2nd_stop_row
#DEFINE	decostop_4th_stop_column	.56
#DEFINE	decostop_5th_stop_row		decostop_3rd_stop_row
#DEFINE	decostop_5th_stop_column	decostop_4th_stop_column
#DEFINE	decostop_6th_stop_row		decostop_2nd_stop_row
#DEFINE	decostop_6th_stop_column	.112
#DEFINE	decostop_7th_stop_row		decostop_3rd_stop_row
#DEFINE	decostop_7th_stop_column	decostop_6th_stop_column

#DEFINE divemode_simtext_row        .164
#DEFINE divemode_simtext_column     .32

; Divemode menu
#DEFINE divemode_menu_row           .164        ; Upper row
#DEFINE divemode_menu_lower         .239        ; Lower border
#DEFINE divemode_menu_left          .0          ; Left
#DEFINE divemode_menu_right         .159        ; Right

#DEFINE divemode_menu_item1_row    .165
#DEFINE divemode_menu_item1_column .9
#DEFINE divemode_menu_item2_row    divemode_menu_item1_row+.24
#DEFINE divemode_menu_item2_column divemode_menu_item1_column
#DEFINE divemode_menu_item3_row    divemode_menu_item2_row+.24
#DEFINE divemode_menu_item3_column divemode_menu_item1_column
#DEFINE divemode_menu_item4_row    divemode_menu_item1_row
#DEFINE divemode_menu_item4_column .89
#DEFINE divemode_menu_item5_row    divemode_menu_item4_row+.24
#DEFINE divemode_menu_item5_column divemode_menu_item4_column
#DEFINE divemode_menu_item6_row    divemode_menu_item5_row+.24
#DEFINE divemode_menu_item6_column divemode_menu_item4_column

#DEFINE dive_compass_mask_row       decoplan_title_row
#DEFINE dive_compass_mask_column    decoplan_title_column
#DEFINE dive_compass_head_column    .57
#DEFINE dive_compass_head_row       divemode_customview_row+.30

#DEFINE dive_compass_graph_row      .102
#DEFINE dive_compass_graph_height   .23
#DEFINE dive_compass_graph_width    .2
#DEFINE dive_compass_graph_left     .5
#DEFINE dive_compass_graph_leftx2   dive_compass_graph_left*.2
#DEFINE dive_compass_graph_right    .155

