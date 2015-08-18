;=============================================================================
;
;   File divemode.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-15 : [mH] moving from OSTC code

    extern    set_dive_modes
    extern    diveloop
    extern    apnoe_calc_maxdepth

; Divemode layout:
; row=0...239
; column=0...159 (x2)

; Divemode has multiple layouts but basicly splits the screen into 3 rows:
;   - upper content row: depth, max depth, dive time, etc.
;   - custom content view with selectable views
;   - bottom content: temp, gas, ndl, tts, etc.

; I. The upper conent row. (0-99)
;   The top row can be divided 2 areas: 
;    - header: has the titles (mask)
;    - content: has the values
;******* Upper content / header row *******
#DEFINE    dm_mask_depth_row              .0
#DEFINE    dm_mask_depth_column           .12
#DEFINE    dm_mask_maxdepth_row           .0
#DEFINE    dm_mask_maxdepth_column        .73
#DEFINE    dm_mask_maxdepth_column_nvsi   .63
#DEFINE    dm_mask_divetime_row           .0
#DEFINE    dm_mask_divetime_column        .122

;   The content row contains 3 columns:
;    - depth and ascend rate warning
;    - max depth and warning messages
;    - dive time, apnea dive times and warning icon
;   DIVEMODE_OFFSET=position below the title row
;******* Upper content / content row / 1st col *******
; GLOBAL
#DEFINE    dm_offset              .14                    ; 14
; Depth
#DEFINE    dm_depth_row           dm_offset              ; TOP  - 14 - start position of the detph numbers
#DEFINE    dm_depth_column        .0                     ; LEFT - 0
#DEFINE    dm_depth_bot           dm_depth_row+.61       ; 75
#DEFINE    dm_depth_rgt           dm_depth_column+.59    ; 59
#DEFINE    dm_depth_dm_row        dm_depth_row+.25       ; 39 - if metric and d<100, decimeter shown as: ".5"
#DEFINE    dm_depth_dm_column     dm_depth_column+.40    ; 40 - bottom aligned so it has its own position (2nd content line only)
; Ascend rate
#DEFINE    dm_velocity_text_row        dm_depth_row+.62             ; 76
#DEFINE    dm_velocity_text_column     dm_depth_column              ; 0
#DEFINE    dm_velocity_text_bot        dm_velocity_text_row+.23     ; 99
#DEFINE    dm_velocity_text_rgt        dm_depth_rgt                 ; 61
; Ascend/Descend bar
#DEFINE    dm_velobar_top          dm_offset           ; 14
#DEFINE    dm_velobar_lft          dm_depth_rgt+.1     ; 62
#DEFINE    dm_velobar_bot          dm_offset+.85       ; 99  // must fit into 8b
#DEFINE    dm_velobar_rgt          .73                 ; 73
#DEFINE    dm_velobar_width        .12

;******* Upper content / content row / 2nd col *******
#DEFINE    dm_upcnt_2ndcol              .74                            ; 74
#DEFINE    dm_upcnt_2ndcol_nvsi         .64                            ; 64
; Max depth
#DEFINE    dm_max_depth_row             dm_offset                      ; 14
#DEFINE    dm_max_depth_column          dm_upcnt_2ndcol                ; 74
#DEFINE    dm_max_depth_column_nvsi     dm_upcnt_2ndcol_nvsi           ; 64
#DEFINE    dm_max_depth_bot             dm_max_depth_row+.34           ; 48
#DEFINE    dm_max_depth_rgt             dm_max_depth_column+.36        ; 100
#DEFINE    dm_max_depth_dm_row          dm_max_depth_row+.11           ; 25
#DEFINE    dm_max_depth_dm_column       dm_max_depth_column+.23        ; 87
#DEFINE    dm_max_depth_dm_column_nvsi  dm_max_depth_column_nvsi+.23   ; 77
; Warning area (combined)
#DEFINE    dm_warning_row             dm_offset+.36             ; 50
#DEFINE    dm_warning_column          dm_upcnt_2ndcol           ; 74
#DEFINE    dm_warning_bot             dm_warning_row+.49        ; 99
#DEFINE    dm_warning_rgt             dm_warning_column+.62     ; 136
#DEFINE    dm_warning_length          .9                        ; total string length
; Warning row #1
#DEFINE    dm_warning1_row            dm_warning_row            ; 36/50
#DEFINE    dm_warning1_column         dm_warning_column         ; 64
#DEFINE    dm_warning1_bot            dm_warning1_row+.23       ; 59/73
#DEFINE    dm_warning1_rgt            dm_warning_rgt            ; 136
; Warning row #2
#DEFINE    dm_warning2_row            dm_warning_row+.24        ; 60/74
#DEFINE    dm_warning2_column         dm_warning_column         ; 64
#DEFINE    dm_warning2_bot            dm_warning2_row+.23       ; 83/97
#DEFINE    dm_warning2_rgt            dm_warning_rgt            ; 136

;******* Upper content / content row / 3rd col *******
; Dive time
#DEFINE    dm_divetime_row                dm_offset              ; 14
#DEFINE    dm_divetime_column             .115                   ; 115
#DEFINE    dm_divetime_minsonly_column    .113                   ; 113
#DEFINE    dm_divetime_bot                dm_divetime_row+.34    ; 48
#DEFINE    dm_divetime_rgt                .159                   ; 159
#DEFINE    dm_divetime_secs_row           dm_divetime_row+.11    ; 25
#DEFINE    dm_divetime_secs_column        dm_divetime_column+.24 ; 139
; Warning icon
#DEFINE    dm_warning_icon_row            dm_offset+.41                ; 55
#DEFINE    dm_warning_icon_column         .137                         ; 137
#DEFINE    dm_warning_icon_bot            dm_warning_icon_row+.38      ; 93
#DEFINE    dm_warning_icon_rgt            dm_warning_icon_column+.21   ; 156
; Apnea dive time
#DEFINE    dm_divetime_apnoe_row                  dm_offset                        ; 14
#DEFINE    dm_divetime_apnoe_column               .103                             ; 103
#DEFINE    dm_divetime_apnoe_secs_row             dm_divetime_apnoe_row+.11        ; 25
#DEFINE    dm_divetime_apnoe_secs_column          dm_divetime_apnoe_column+.36     ; 139
; Apnea total time
#DEFINE    dm_apnoe_total_divetime_row            dm_divetime_apnoe_row+.50        ; 64
#DEFINE    dm_apnoe_total_divetime_column         dm_divetime_apnoe_column         ; 103
#DEFINE    dm_apnoe_total_divetime_secs_row       dm_apnoe_total_divetime_row+.11  ; 75
#DEFINE    dm_apnoe_total_divetime_secs_col       dm_divetime_apnoe_column+.36     ; 139
#DEFINE    dm_total_apnoe_text_row                dm_apnoe_total_divetime_row-.11  ; 53
#DEFINE    dm_total_apnoe_text_column             .132                             ; 132
; I. End of the upper conent row. (0-99)



; 1px space between the 1st and 2nd content rows
#DEFINE    dm_sep_1_2_row    dm_offset+.86         ; 100



; II. Custom/selectable content row
;   The costume view display area is:  101,163,0,159 (t,b,l,r), or 0,101->159,163
;   1. Stopwatch with avarage depth
;   2. Decoplan - decompression stops up to 6 stops
;   3. Time, EAD/END and tissue saturation (graph)
;   4. GF and aGF pairs
;   5. Compass
;   6. HUD - 3 sensor readings
;   7. Gas list
;   8. Ceiling (for GF_hi)
;******* Custom content / Global *******
#DEFINE    dm_customview_row              dm_offset+.87             ; 101
#DEFINE    dm_customview_column           .0                        ; 0
#DEFINE    dm_customview_bot              dm_customview_row+.62     ; 163
#DEFINE    dm_customview_rgt              .159                      ; 159

;******* Custom content / #1 - Avg depth, stopwatch and avg depth *******
; Title
#DEFINE    dm_custom_avr_stop_title_row         dm_customview_row+.1      ; 102
#DEFINE    dm_custom_avr_stop_title_column1     .2                        ; 2
#DEFINE    dm_custom_avr_stop_title_column2     .57                       ; 57
#DEFINE    dm_custom_avr_stop_title_column3     .110                      ; 110
; Values
#DEFINE    dm_custom_avr_stop_row               dm_customview_row+.16     ; 117
#DEFINE    dm_custom_avr_stop_column1           .0                        ; 0
#DEFINE    dm_custom_avr_stop_column2           .54                       ; 54
#DEFINE    dm_custom_avr_stop_column3           .118                      ; 118

;******* Custom content / #2 - Decompressions stops *******
; Title
#DEFINE    dm_custom_decoplan_title_row          dm_customview_row              ; 101
#DEFINE    dm_custom_decoplan_title_column       .65                            ; 65
; 1st col
#DEFINE    dm_cust_dstop_2nd_stop_row            dm_customview_row+.15          ; 116
#DEFINE    dm_cust_dstop_2nd_stop_column         .0                             ; 0
#DEFINE    dm_cust_dstop_3rd_stop_row            dm_customview_row+.39          ; 140
#DEFINE    dm_cust_dstop_3rd_stop_column         dm_cust_dstop_2nd_stop_column  ; 0
; 2nd col
#DEFINE    dm_cust_dstop_4th_stop_row            dm_cust_dstop_2nd_stop_row     ; 116
#DEFINE    dm_cust_dstop_4th_stop_column         .56                            ; 56
#DEFINE    dm_cust_dstop_5th_stop_row            dm_cust_dstop_3rd_stop_row     ; 140
#DEFINE    dm_cust_dstop_5th_stop_column         dm_cust_dstop_4th_stop_column  ; 56
; 3rd col
#DEFINE    dm_cust_dstop_6th_stop_row            dm_cust_dstop_2nd_stop_row     ; 116
#DEFINE    dm_cust_dstop_6th_stop_column         .112                           ; 112
#DEFINE    dm_cust_dstop_7th_stop_row            dm_cust_dstop_3rd_stop_row     ; 140
#DEFINE    dm_cust_dstop_7th_stop_column         dm_cust_dstop_6th_stop_column  ; 112

;******* Custom content / #3 - Time, EAD/END and tissue saturation *******
; Clock
#DEFINE    dm_custom_clock_title_row               dm_customview_row+.1              ; 102
#DEFINE    dm_custom_clock_row                     dm_customview_row+.16             ; 117
#DEFINE    dm_custom_clock_column                  .0                                ; 0
#DEFINE    dm_custom_endtime_row                   dm_custom_clock_row+.23           ; 140 - not used
#DEFINE    dm_custom_endtime_column                dm_custom_clock_column            ; 0   - not used
; EAD/AND
#DEFINE    dm_custom_eadend_title_row              dm_customview_row+.1              ; 102
#DEFINE    dm_custom_ead_row                       dm_customview_row+.16             ; 117
#DEFINE    dm_custom_ead_column                    .46                               ; 46
#DEFINE    dm_custom_end_row                       dm_custom_ead_row+.23             ; 140
#DEFINE    dm_custom_end_column                    dm_custom_ead_column              ; 46
; Tissue title
#DEFINE    dm_custom_tissue_title_row              dm_customview_row+.1              ; 102
#DEFINE    dm_custom_tissue_title_column           .120                              ; 120
; N2 / He values
#DEFINE    dm_custom_tissue_N2_row                 dm_custom_ead_row+.5              ; 122
#DEFINE    dm_custom_tissue_N2_column              .105                              ; 105
#DEFINE    dm_custom_tissue_He_row                 dm_custom_end_row+.5              ; 145
#DEFINE    dm_custom_tissue_He_column              dm_custom_tissue_N2_column        ; 105
; Tissue diagram
#DEFINE    dm_custom_tissue_diagram_top            dm_customview_row+.16             ; 117
#DEFINE    dm_custom_tissue_diagram_bottom         dm_custom_tissue_diagram_top+.43  ; 160
#DEFINE    dm_custom_tissue_diagram_left           .116                              ; 116
#DEFINE    dm_custom_tissue_diagram_frame_spacing  .8                                ; 8

;******* Custom content / #4 - GF-lo/hi, aGF-lo/hi, current GF value *******
; Title
#DEFINE    dm_custom_gf_title_row            dm_customview_row+.1        ; 102
#DEFINE    dm_custom_gf_title_col1           .8                          ; 8
#DEFINE    dm_custom_gf_title_col2           .62                         ; 62
#DEFINE    dm_custom_gf_title_col3           .115                        ; 115
; GF
#DEFINE    dm_custom_gf_row                  dm_customview_row+.18       ; 119
#DEFINE    dm_custom_gf_column               .0                          ; 0
; aGF
#DEFINE    dm_custom_agf_row                 dm_custom_gf_row            ; 119
#DEFINE    dm_custom_agf_column              .60                         ; 60
; Current GF
#DEFINE    dm_custom_currentgf_row           dm_custom_gf_row            ; 119
#DEFINE    dm_custom_currentgf_column        .118                        ; 118

;******* Custom content / #5 - Compass *******
; Title
#DEFINE    dm_custom_compass_mask_row           dm_customview_row        ; 101
#DEFINE    dm_custom_compass_mask_column        .65                      ; 65
; Head and arrows
#DEFINE    dm_custom_compass_head_row           dm_customview_row+.39    ; 140
#DEFINE    dm_custom_compass_head_column        .62                      ; 62
#DEFINE    dm_custom_compass_ldir_column        .5                       ; 5
#DEFINE    dm_custom_compass_rdir_column        .140                     ; 140
; Ruler
#DEFINE    dm_custom_compass_graph_row          dm_customview_row                  ; 101
#DEFINE    dm_custom_compass_graph_height       .33                                ; 33
#DEFINE    dm_custom_compass_tick_height        .3                                 ; 3
#DEFINE    dm_custom_compass_tick_top_top       dm_custom_compass_graph_row+.1     ; 102
#DEFINE    dm_custom_compass_tick_top_bot       dm_custom_compass_graph_row+.4     ; 105
#DEFINE    dm_custom_compass_label_row          dm_custom_compass_graph_row+.6     ; 107
#DEFINE    dm_custom_compass_label_height       .24                                ; 24
#DEFINE    dm_custom_compass_tick_bot_top       dm_custom_compass_graph_row+.30    ; 131
#DEFINE    dm_custom_compass_tick_bot_bot       dm_custom_compass_graph_row+.33    ; 134

;******* Custom content / #6 - HUD / O2 sensor values *******
#DEFINE    dm_custom_hud_row                dm_customview_row+.1        ; 102
#DEFINE    dm_custom_hud_column1            .10                         ; 10
#DEFINE    dm_custom_hud_column2            .65                         ; 65
#DEFINE    dm_custom_hud_column3            .120                        ; 120
#DEFINE    dm_custom_hud_data_row           dm_customview_row+.16       ; 117
#DEFINE    dm_custom_hud_sensor1_column     .6                          ; 6
#DEFINE    dm_custom_hud_sensor2_column     .62                         ; 62
#DEFINE    dm_custom_hud_sensor3_column     .118                        ; 118

;******* Custom content / #7 - Gas list *******
; Title
#DEFINE    dm_custom_dyn_gas_mask_row          dm_customview_row        ; 101
#DEFINE    dm_custom_dyn_gas_mask_column       .59                      ; 59
; col #1
#DEFINE    dm_custom_dyn_gas_row1              dm_customview_row+.16    ; 117
#DEFINE    dm_custom_dyn_gas_column1           .0                       ; 0
; col #2
#DEFINE    dm_custom_dyn_gas_row2              dm_customview_row+.39    ; 140
#DEFINE    dm_custom_dyn_gas_column2           .80                      ; 80

;******* Custom content / #8 - Ceiling value for GF_hi *******
; Title
#DEFINE    dm_custom_ceiling_text_row       dm_customview_row+.1        ; 102
#DEFINE    dm_custom_ceiling_text_column    .68                         ; 66
#DEFINE    dm_custom_ceiling_ppo2_column    .8                          ; 8
; Value
#DEFINE    dm_custom_ceiling_value_row      dm_customview_row+.18       ; 119
#DEFINE    dm_custom_ceiling_value_column   .66                         ; 64
#DEFINE    dm_custom_ceiling_ppo2_val_col   .2                          ; 2
; II. End of the custom conent row. (101-163)



; 1px space between the 2nd and 3rd content rows
#DEFINE    dm_sep_2_3_row    dm_offset+.150         ; 164



; III. The 3rd content row contains temperature, active gas, NDL/TTS time,
;      simulator menu, active/dil gas, decostop, Apnea surface time and max depth
;  The content row contains 2 columns:
;    - temperature, gas names
;    - NDL/TTS, DecoStop
#DEFINE    dm_3rdrow_top     dm_offset+.151    ; 165
#DEFINE    dm_3rdrow_bot     .239              ; 239
#DEFINE    dm_3rdrow_lft     .0                ; 0
#DEFINE    dm_3rdrow_rgt     .159              ; 159

;******* Bottom content / 1st col *******
; Temperature
#DEFINE    dm_temp_row               dm_3rdrow_top     ; 165
#DEFINE    dm_temp_column            .0                ; 0
; Simulation text
#DEFINE    dm_simtext_row            dm_3rdrow_top     ; 165
#DEFINE    dm_simtext_column         .30               ; 30
; Diluent gas
#DEFINE    dm_active_dil_row         dm_3rdrow_top+.24 ; 189
#DEFINE    dm_active_dil_column      .0                ; 0
; active gas for OC, blinking better gas, setpoint or bailout for CCR
#DEFINE    dm_active_gas_row         .208              ; 208
#DEFINE    dm_active_gas_column      .0                ; 0

;******* Bottom content / 2nd col *******
; Next deco stop for TTS
#DEFINE    dm_decostop_1st_stop_row       dm_3rdrow_top             ; 165
#DEFINE    dm_decostop_1st_stop_column    .82                       ; 82
; Safety Stop
#DEFINE    dm_safetystop_row              dm_3rdrow_top             ; 165 (pre: 171)
#DEFINE    dm_safetystop_column           .118                      ; 118
#DEFINE    dm_safetystop_bot              dm_safetystop_row+.31     ; 196
#DEFINE    dm_safetystop_text_row         dm_safetystop_row+.1      ; 166 (pre: 175)
#DEFINE    dm_safetystop_text_column      .80                       ; 80 - for the 4char "Stop"!!!
; TTS
#DEFINE    dm_tts_value_row               dm_3rdrow_top+.32;        ; 197 (pre: 207)
#DEFINE    dm_tts_value_column            .118                      ; 118
#DEFINE    dm_tts_text_row                dm_tts_value_row+.5       ; 202 (pre: 211)
#DEFINE    dm_tts_text_column             .85                       ; 85
; NDL - the same position as TTS
#DEFINE    dm_ndl_value_row               dm_tts_value_row          ; 197
#DEFINE    dm_ndl_value_column            dm_tts_value_column       ; 118
#DEFINE    dm_ndl_text_row                dm_tts_text_row           ; 202
#DEFINE    dm_ndl_text_column             dm_tts_text_column        ; 85
; FTTS (only modded screen)
#DEFINE    dm_ftts_value_row              dm_3rdrow_top+.64         ; 215
#DEFINE    dm_ftts_value_column           .97                       ; 97
; Grid line (only modded screen)
#DEFINE     dm_gassep_row                 dm_sep_2_3_row            ; 164
#DEFINE     dm_gassep_bot                 .239                      ; .239
#DEFINE     dm_gassep_column              .78                       ; .78

;******* Bottom content / Apnea mode *******;
#DEFINE    dm_apnoe_surface_time_text_row           .190                   ; 190
#DEFINE    dm_apnoe_surface_time_text_col           .30                    ; 30
#DEFINE    dm_apnoe_surface_time_row                .204                   ; 204
#DEFINE    dm_apnoe_surface_time_column             .15                    ; 15
#DEFINE    dm_apnoe_last_max_depth_text_row         .190                   ; 190
#DEFINE    dm_apnoe_last_max_depth_text_col         .100                   ; 100
#DEFINE    dm_apnoe_last_max_depth_row              .204                   ; 204
#DEFINE    dm_apnoe_last_max_depth_column           .100                   ; 100


; IV. The last set of parameters are for the menus displayed in dive mode
; Divemode menu
#DEFINE    dm_menu_row           .164        ; 164 Upper row -- the frame's top line is the separator
#DEFINE    dm_menu_lower         .239        ; 239 Lower border
#DEFINE    dm_menu_left          .0          ; 0   Left
#DEFINE    dm_menu_right         .159        ; 159 Right

#DEFINE    dm_menu_item1_row         dm_menu_row+.1          ; 165
#DEFINE    dm_menu_item1_column      .9                      ; 9
#DEFINE    dm_menu_item2_row         dm_menu_item1_row+.24   ; 189
#DEFINE    dm_menu_item2_column      dm_menu_item1_column    ; 9
#DEFINE    dm_menu_item3_row         dm_menu_item2_row+.24   ; 213
#DEFINE    dm_menu_item3_column      dm_menu_item1_column    ; 9

#DEFINE    dm_menu_item4_row         dm_menu_row+.1          ; 165
#DEFINE    dm_menu_item4_column      .89                     ; 89
#DEFINE    dm_menu_item5_row         dm_menu_item4_row+.24   ; 189
#DEFINE    dm_menu_item5_column      dm_menu_item4_column    ; 89
#DEFINE    dm_menu_item6_row         dm_menu_item5_row+.24   ; 213
#DEFINE    dm_menu_item6_column      dm_menu_item4_column    ; 89

