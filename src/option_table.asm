;=============================================================================
;
;   File option_table.asm
;
;   Thje option table
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2014-08-03 : mH creation
;

#include    "hwos.inc"                  ; Mandatory header
#include    "eeprom_rs232.inc"

;=============================================================================
; Options Tables

option_table    CODE 0x00700

OPTION_UINT8    MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      1, min
        db      max, eeprom
        dw      unit
        dw      register
        ENDM

OPTION_UINT8p2  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      2, min
        db      max, eeprom
        dw      unit
        dw      register
        ENDM

OPTION_UINT8p3  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      3, min
        db      max, eeprom
        dw      unit
        dw      register
        ENDM

OPTION_UINT8p10  MACRO   lbl, min, max, default, unit, eeprom, register
        global  lbl
lbl:    db      0, default  ; Type0 = INT8
        db      .10, min
        db      max, eeprom
        dw      unit
        dw      register
        ENDM

OPTION_ENUM8    MACRO   lbl, max, default, tValue, eeprom, register
        global  lbl
        extern  tValue
lbl:    db      1, default  ; Type1 = ENUM
        db      LOW(tValue), HIGH(tValue)
        db      max, eeprom
        dw      0                       ; No unit
        dw      register
        ENDM

OPTION_BOOL     MACRO   lbl, default, eeprom, register
        OPTION_ENUM8    lbl, 2, default, tNo, eeprom, register
        ENDM


OPTION_STRING   MACRO   lbl, length, defText, eeprom, register
        global  lbl
lbl:    db      2, LOW(defText)         ; Type2 = STRING
        db      HIGH(defText), 0
        db      length, eeprom
        dw      0                       ; No unit
        dw      register
        ENDM


;=============================================================================
        extern  tPercent, tMeters, tMinutes, tGasDisabled, tbar
        extern  char_I_deco_gas_change, char_I_setpoint_change, char_I_setpoint_cbar, char_I_dil_change
        extern  char_I_dive_interval, char_I_bottom_time, char_I_bottom_depth
        extern  char_I_deco_model, char_I_saturation_multiplier, char_I_desaturation_multiplier
        extern  char_I_extra_time
        extern  tDefName
        extern  char_I_bottom_usage,char_I_deco_usage,tLitersMinute
    ; Option table
    ; OPTION_UINT8  Label,   min,    max,    default, text-string,   EEPROM location (-1 for RAM only),   RAM location
    global  option_table_begin
option_table_begin:
;=============================================================================
; Manage Decoplaner & Dive parameters
        OPTION_UINT8p10 odiveInterval,  .0, .240,   .0,     tMinutes, -1,     char_I_dive_interval
        OPTION_UINT8p2  obottomTime,    .1, .60,    .5,     tMinutes, -1,     char_I_bottom_time
        OPTION_UINT8p3  obottomDepth,   .12,.120,    .21,    tMeters,  -1,     char_I_bottom_depth
        OPTION_ENUM8    oDiveMode,      5,  0,  tDvOC,               .8,    opt_dive_mode               ; 0=OC, 1=CC, 2=Gauge, 3=Apnea, 4=PSCR 
        OPTION_ENUM8    oDecoMode,      2,  1,  tZHL16,              .9,    char_I_deco_model           ; 0 = ZH-L16, 1 = ZH-L16-GF
        OPTION_UINT8p10 oPPO2Max,       .120, ppo2_warning_high, .160,   0,      .10,    opt_ppO2_max
        OPTION_UINT8    oLastDeco,      .3,  .6, .3,  tMeters,      .11,    opt_last_stop
        OPTION_UINT8    oGF_low,       .10,  .100, .30,  tPercent,  .12,    opt_GF_low
        OPTION_UINT8    oGF_high,      .45,  .110, .85, tPercent,   .13,    opt_GF_high
        OPTION_UINT8p10 osatmult,      .100,  .140, .110,tPercent,  .14,    char_I_saturation_multiplier
        OPTION_UINT8p10 odesatmult,    .60,  .100,  .90,tPercent,   .15,    char_I_desaturation_multiplier
        OPTION_UINT8p10 oPPO2Min,       .16, ppo2_warning_low, .19,   0,         .16,    opt_ppO2_min
        OPTION_UINT8    oaGF_low,      .10,  .100, .30,  tPercent,  .17,    opt_aGF_low
        OPTION_UINT8    oaGF_high,     .45,  .110, .85, tPercent,   .18,    opt_aGF_high
        OPTION_BOOL     oEnable_aGF,    0,                          .19,    opt_enable_aGF              ; =1: aGF can be selected underwater
        OPTION_UINT8    oCompassGain,   0,  7,  6,       tMinutes,  .20,    opt_compass_gain            ; 0-7 (230LSB/Gauss to 1370LSB/Gauss)
        OPTION_ENUM8    oSamplingRate,  2,  0,  tSampling2s,        .21,    opt_sampling_rate           ; =1: 10s, =0: 2s

;=============================================================================
; Managing Settings
        OPTION_UINT8    oExtraTime,     0,  .9,   0,tMinutes,   .22,    char_I_extra_time               ; Future TTS
        OPTION_ENUM8    oBrightness,    3,  0,  tEco,           .23,    opt_brightness                  ; =0: Eco, =1:Medium, =2:Full
        OPTION_UINT8    oDiveSalinity,  0,  4, 0,  tPercent,    .24,    opt_salinity                    ; 0-4%
        OPTION_ENUM8    oCCRMode,    3,  0,  tCCRModeFixedSP,   .25,    opt_ccr_mode                    ; =0: Fixed SP, =1: Sensor, =2: Auto SP
    IFNDEF	    french_italian
	OPTION_ENUM8    oLanguage,      2,  0,  tEnglish,   .26,    opt_language                        ; 0=EN, 1=DE
    ELSE
	OPTION_ENUM8    oLanguage,      2,  0,  tFrench,    .26,    opt_language                        ; 0=FR, 1=IT
    ENDIF
	OPTION_ENUM8    oDateFormat,    3,  1,  tDateformat,.27,    opt_dateformat                      ; =0:MMDDYY, =1:DDMMYY, =2:YYMMDD
        OPTION_ENUM8    oUnits,         2,  0,  tMetric,    .28,    opt_units                           ; 0=Meters, 1=Feets

;=============================================================================
; Compass calibration data
        OPTION_UINT8    oCalx0,         0,.255,.0,      0,  .29,    compass_CX_f+0
        OPTION_UINT8    oCalx1,         0,.255,.0,      0,  .30,    compass_CX_f+1
        OPTION_UINT8    oCaly0,         0,.255,.0,      0,  .31,    compass_CY_f+0
        OPTION_UINT8    oCaly1,         0,.255,.0,      0,  .32,    compass_CY_f+1
        OPTION_UINT8    oCalz0,         0,.255,.0,      0,  .33,    compass_CZ_f+0
        OPTION_UINT8    oCalz1,         0,.255,.0,      0,  .34,    compass_CZ_f+1

;=============================================================================
; Gas list
        OPTION_ENUM8    oGas1,          3,  1,  tGasDisabled,         .35,     opt_gas_type+0; 0=Disabled, 1=First, 2=Travel, 3=Deco
        OPTION_ENUM8    oGas2,          3,  0,  tGasDisabled,         .36,     opt_gas_type+1
        OPTION_ENUM8    oGas3,          3,  0,  tGasDisabled,         .37,     opt_gas_type+2
        OPTION_ENUM8    oGas4,          3,  0,  tGasDisabled,         .38,     opt_gas_type+3
        OPTION_ENUM8    oGas5,          3,  0,  tGasDisabled,         .39,     opt_gas_type+4
        OPTION_UINT8    oGas1O2,        .7 ,.100,   .21,    tPercent, .40,     opt_gas_O2_ratio+0
        OPTION_UINT8    oGas1He,        .1, .100,   .0,     tPercent, .41,     opt_gas_He_ratio+0
        OPTION_UINT8    oGas2O2,        .7 ,.100,   .21,    tPercent, .42,     opt_gas_O2_ratio+1
        OPTION_UINT8    oGas2He,        .1, .100,   .0,     tPercent, .43,     opt_gas_He_ratio+1
        OPTION_UINT8    oGas3O2,        .7 ,.100,   .21,    tPercent, .44,     opt_gas_O2_ratio+2
        OPTION_UINT8    oGas3He,        .1, .100,   .0,     tPercent, .45,     opt_gas_He_ratio+2
        OPTION_UINT8    oGas4O2,        .7 ,.100,   .21,    tPercent, .46,     opt_gas_O2_ratio+3
        OPTION_UINT8    oGas4He,        .1, .100,   .0,     tPercent, .47,     opt_gas_He_ratio+3
        OPTION_UINT8    oGas5O2,        .7 ,.100,   .21,    tPercent, .48,     opt_gas_O2_ratio+4
        OPTION_UINT8    oGas5He,        .1, .100,   .0,     tPercent, .49,     opt_gas_He_ratio+4
        OPTION_UINT8    oGas1Depth,     .0, .99,   .0,     tMeters,  .50,     opt_OC_bail_gas_change+0
        OPTION_UINT8    oGas2Depth,     .0, .99,   .0,     tMeters,  .51,     opt_OC_bail_gas_change+1
        OPTION_UINT8    oGas3Depth,     .0, .99,   .0,     tMeters,  .52,     opt_OC_bail_gas_change+2
        OPTION_UINT8    oGas4Depth,     .0, .99,   .0,     tMeters,  .53,     opt_OC_bail_gas_change+3
        OPTION_UINT8    oGas5Depth,     .0, .99,   .0,     tMeters,  .54,     opt_OC_bail_gas_change+4
        OPTION_UINT8    oDil1O2,        .7 ,.100,   .21,    tPercent, .55,     opt_dil_O2_ratio+0
        OPTION_UINT8    oDil1He,        .1, .100,   .0,     tPercent, .56,     opt_dil_He_ratio+0
        OPTION_UINT8    oDil2O2,        .7 ,.100,   .21,    tPercent, .57,     opt_dil_O2_ratio+1
        OPTION_UINT8    oDil2He,        .1, .100,   .0,     tPercent, .58,     opt_dil_He_ratio+1
        OPTION_UINT8    oDil3O2,        .7 ,.100,   .21,    tPercent, .59,     opt_dil_O2_ratio+2
        OPTION_UINT8    oDil3He,        .1, .100,   .0,     tPercent, .60,     opt_dil_He_ratio+2
        OPTION_UINT8    oDil4O2,        .7 ,.100,   .21,    tPercent, .61,     opt_dil_O2_ratio+3
        OPTION_UINT8    oDil4He,        .1, .100,   .0,     tPercent, .62,     opt_dil_He_ratio+3
        OPTION_UINT8    oDil5O2,        .7 ,.100,   .21,    tPercent, .63,     opt_dil_O2_ratio+4
        OPTION_UINT8    oDil5He,        .1, .100,   .0,     tPercent, .64,     opt_dil_He_ratio+4
        OPTION_UINT8    oSetPoint1,     .20, .160,  .70,    tbar,     .65,     char_I_setpoint_cbar+0
        OPTION_UINT8    oSetPoint2,     .20, .160,  .90,    tbar,     .66,     char_I_setpoint_cbar+1
        OPTION_UINT8    oSetPoint3,     .20, .160,  .100,   tbar,     .67,     char_I_setpoint_cbar+2
        OPTION_UINT8    oSetPoint4,     .20, .160,  .120,   tbar,     .68,     char_I_setpoint_cbar+3
        OPTION_UINT8    oSetPoint5,     .20, .160,  .140,   tbar,     .69,     char_I_setpoint_cbar+4
        OPTION_UINT8    oSP1Depth,      .0, .100,   .0,     tMeters,  .70,     char_I_setpoint_change+0
        OPTION_UINT8    oSP2Depth,      .0, .100,   .0,     tMeters,  .71,     char_I_setpoint_change+1
        OPTION_UINT8    oSP3Depth,      .0, .100,   .0,     tMeters,  .72,     char_I_setpoint_change+2
        OPTION_UINT8    oSP4Depth,      .0, .100,   .0,     tMeters,  .73,     char_I_setpoint_change+3
        OPTION_UINT8    oSP5Depth,      .0, .100,   .0,     tMeters,  .74,     char_I_setpoint_change+4
        OPTION_ENUM8    oDil1,          2,  1,  tDilDisabled,         .75,     opt_dil_type+0   ; 0=Disabled, 1=First, 2=Normal
        OPTION_ENUM8    oDil2,          2,  0,  tDilDisabled,         .76,     opt_dil_type+1
        OPTION_ENUM8    oDil3,          2,  0,  tDilDisabled,         .77,     opt_dil_type+2
        OPTION_ENUM8    oDil4,          2,  0,  tDilDisabled,         .78,     opt_dil_type+3
        OPTION_ENUM8    oDil5,          2,  0,  tDilDisabled,         .79,     opt_dil_type+4
        OPTION_UINT8    oDil1Depth,     .0, .99,   .0,     tMeters,  .80,     char_I_dil_change+0
        OPTION_UINT8    oDil2Depth,     .0, .99,   .0,     tMeters,  .81,     char_I_dil_change+1
        OPTION_UINT8    oDil3Depth,     .0, .99,   .0,     tMeters,  .82,     char_I_dil_change+2
        OPTION_UINT8    oDil4Depth,     .0, .99,   .0,     tMeters,  .83,     char_I_dil_change+3
        OPTION_UINT8    oDil5Depth,     .0, .99,   .0,     tMeters,  .84,     char_I_dil_change+4

;=============================================================================
; opt_name from 85 to 145
        OPTION_STRING   oName,          opt_name_length,    tDefName, .85,     opt_name

;=============================================================================
; Misc
        OPTION_ENUM8    oColorSetDive,  4,  0, tColorSetName0,        .146,    opt_dive_color_scheme            ; Color scheme divemode
        OPTION_UINT8    oPressureAdjust, .0,.255,   .0,     -1,       .147,    opt_pressure_adjust              ; SIGNED int (-20/+20mbar max.)
        OPTION_BOOL     oSafetyStop,    0,                            .148,    opt_enable_safetystop            ; =1: A safety stop is shown
        OPTION_UINT8    oCalGasO2,      .21,.100,   .21,    tPercent, .149,    opt_calibration_O2_ratio         ; Calibration gas %O2
        OPTION_BOOL     oSensorFallback,1,                            .150,    opt_sensor_fallback              ; =1: Fallback to SP1 when sensor is lost
        OPTION_BOOL     oFlipScreen,    0,                            .151,    opt_flip_screen                  ; =1: Flip the screen
        OPTION_UINT8p10 ocR_button_left, .20, .100,   .40,  tPercent, .152,    opt_cR_button_left               ; left button sensitivity
        OPTION_UINT8p10 ocR_button_right,.20, .100,   .40,  tPercent, .153,    opt_cR_button_right              ; right button sensitivity
        OPTION_UINT8    obottom_usage,  .5,.50,   .20,tLitersMinute,  .154,    char_I_bottom_usage              ; l/min
        OPTION_UINT8    odeco_usage,    .5,.50,   .20,tLitersMinute,  .155,    char_I_deco_usage                ; l/min
        OPTION_BOOL     oMODwarning,    0,                            .156,    opt_modwarning                   ; =1: red depth blinking warning
        OPTION_BOOL     oVSItextv2,     0,                            .157,    opt_vsitextv2                    ; =1: use the dynamic (depends on depth) ascend rate limits
        OPTION_BOOL     oVSIgraph,      0,                            .158,    opt_vsigraph                     ; =1: draw the graphical VSI bar
        OPTION_BOOL     oShowppO2,      0,                            .159,    opt_showppo2                     ; =1:always show the ppO2 value in the warning position
        OPTION_UINT8    oTemperatureAdjust, .0,.255,   .0,     -1,    .160,    opt_temperature_adjust           ; SIGNED int (-2.0/+2.0 °C max.)
        OPTION_UINT8    oSafetyStopLength, .60,.240,   .180,   -1,    .161,    opt_safety_stop_length           ; [s]
        OPTION_UINT8    oSafetyStopStart,  .21,.61,     .51,   -1,    .162,    opt_safety_stop_start            ; [cbar], default 510mbar, min 210mbar, max 610mbar
        OPTION_UINT8    oSafetyStopEnd,    .19,.39,     .29,   -1,    .163,    opt_safety_stop_end              ; [cbar], default 290mbar, min 190mbar, max 390mbar
        OPTION_UINT8    oSafetyStopReset,  .81,.151,    .101,  -1,    .164,    opt_safety_stop_reset            ; [cbar], default 1010mbar, min 810mbar, max 1510mbar
	OPTION_UINT8    oDiveTimeout,	    .1,.20,  divemode_timeout_default,   tMinutes,  .168,    opt_diveTimeout			; Timeout for divemode in minutes
	OPTION_UINT8	oPSCR_drop,	.0,.15,	.4,	     tPercent,.169,    opt_PSCR_drop			; PSCR Drop [%]
	OPTION_UINT8	oPSCR_lungratio,.5,.20,	.10,	     tPercent,.170,    opt_PSCR_lungratio		; PSCR Lung Ratio [1/x]

;=============================================================================
; Set Time/Set Date (RAM only)
		OPTION_UINT8    oSetHours,		.0,	.23, .0,	0, -1,		hours
		OPTION_UINT8    oSetMinutes,	.0,	.59, .0,	0, -1,		mins
		OPTION_UINT8    oSetDay,		.1,	.31, .0,	0, -1,		day
		OPTION_UINT8    oSetMonth,		.1,	.12, .0,	0, -1,		month
		OPTION_UINT8    oSetYear,		.13,.20, .0,	0, -1,		year

        global  option_table_end
option_table_end:
    END