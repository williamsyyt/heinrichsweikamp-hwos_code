;=============================================================================
;
;   File surfmode.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-07 : [mH] moving from OSTC code

 extern surfloop
 extern	calc_deko_surfmode
 extern	timeout_surfmode
 extern	timeout_testmode

; Surfacemode/Homescreen layout:
; row=0...239
; column=0...159 (x2)

#DEFINE	surf_clock_row		.0
#DEFINE	surf_clock_column	.11
#DEFINE	surf_date_row		.25
#DEFINE	surf_date_column	.0

#DEFINE	batt_voltage_row	.8
#DEFINE	batt_voltage_column	.60
#DEFINE	batt_percent_row	.26
#DEFINE	batt_percent_column	.60

#DEFINE surf_decotype_row       .60
#DEFINE surf_decotype_column    .100

#DEFINE	surf_desat_row		.103
#DEFINE	surf_desat_column	.65
#DEFINE	surf_nofly_row		.128
#DEFINE	surf_nofly_column	.65

#DEFINE surf_warning1_column    .40
#DEFINE surf_warning1_row       .191
#DEFINE surf_warning2_column    surf_warning1_column
#DEFINE surf_warning2_row       surf_warning1_row+.24
#DEFINE surf_warning_length     .11                 ; total string length

#DEFINE	surf_temp_row		.185
#DEFINE	surf_temp_column	.112
#DEFINE	surf_press_row		.160
#DEFINE	surf_press_column	.98

#DEFINE	surf_gaslist_row        .66
#DEFINE	surf_gaslist_column     .2
#DEFINE surf_gaslist_spacing    .24

#DEFINE surf_customtext_row1    .66
#DEFINE surf_customtext_row2    surf_customtext_row1+.24
#DEFINE surf_customtext_row3    surf_customtext_row2+.24
#DEFINE surf_customtext_row4    surf_customtext_row3+.24
#DEFINE surf_customtext_row5    surf_customtext_row4+.24
#DEFINE surf_customtext_column  .5

#DEFINE surf_compass_mask_row       surf_customtext_row2
#DEFINE surf_compass_mask_column    .10
#DEFINE surf_compass_head_row       surf_customtext_row4-.5
#DEFINE surf_compass_head_column    .10

#DEFINE surf_hud_sensor1_column .112
#DEFINE surf_hud_sensor1_row    .87
#DEFINE surf_hud_sensor2_column surf_hud_sensor1_column
#DEFINE surf_hud_sensor2_row    .111
#DEFINE surf_hud_sensor3_column surf_hud_sensor1_column
#DEFINE surf_hud_sensor3_row    .135

#DEFINE surf_menu_sensor1_column    .80
#DEFINE surf_menu_sensor1_row       .60
#DEFINE surf_menu_sensor2_column    surf_menu_sensor1_column
#DEFINE surf_menu_sensor2_row       surf_menu_sensor1_row+.27*.1
#DEFINE surf_menu_sensor3_column    surf_menu_sensor1_column
#DEFINE surf_menu_sensor3_row       surf_menu_sensor1_row+.27*.2
#DEFINE surf_menu_sensor4_column    surf_menu_sensor1_column
#DEFINE surf_menu_sensor4_row       surf_menu_sensor1_row-.27*.1
#DEFINE surf_menu2_sensor1_row      .47
#DEFINE surf_menu2_sensor2_row      surf_menu2_sensor1_row+.27*.1
#DEFINE surf_menu2_sensor3_row      surf_menu2_sensor1_row+.27*.2
#DEFINE surf_menu2_ambient_row      surf_menu2_sensor1_row+.27*.4
#DEFINE surf_menu2_ambient_column   .95

#DEFINE surf_tissue_diagram_top     .66
#DEFINE surf_tissue_diagram_bottom  tissue_diagram_top+.64
#DEFINE surf_tissue_diagram_left    .5
#DEFINE surf_tissue_diagram_right   surf_decotype_column-.5
#DEFINE surf_tissue_N2_row          surf_tissue_diagram_top+.1
#DEFINE surf_tissue_N2_column       surf_tissue_diagram_left+.5
#DEFINE surf_tissue_He_row          surf_tissue_diagram_top+.56
#DEFINE surf_tissue_He_column       surf_tissue_N2_column
