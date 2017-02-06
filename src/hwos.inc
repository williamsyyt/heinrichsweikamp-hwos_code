;=============================================================================
;
;   File hwos.inc
;
;   OSTC Platform definitions
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-05-24 : [jDG] Cleanups from initial Matthias code.

;=============================================================================

        LIST      P=18F87K22	; change also: Configure->SelectDevice from Mplab

;#DEFINE __DEBUG

;#DEFINE	    french_italian	    ; If defined, use french and italian instead of english and german	
	
#include <p18f87k22.inc>
#include <ports.inc>                        ; Portmap

; Firmware definition
#DEFINE	softwareversion_x		d'2'		; Software version  XX.YY
#DEFINE	softwareversion_y		d'14'		; Software version  XX.YY
#DEFINE softwareversion_beta 	1 			; (and 0 for release)
; Firmware version will appear in "Change Firmware" style
#DEFINE firmware_expire_year    .18
#DEFINE firmware_expire_month   .6
#DEFINE firmware_expire_day     .1

#DEFINE comm_service_key        0xABCDEF

;-----------------------------------------------------------------------------

#DEFINE	CCP1CON_VALUE		b'00001100'		; PWM1 for LED dimming
#DEFINE T2CON_ECO           b'00000100'		; 1:1 Postscaler, 1:1 Prescaler, Timer 2 start -> 980Hz (Good compromise of no-flicker and efficiency)
#DEFINE T2CON_NORMAL        b'00001110'
#DEFINE T2CON_FASTEST       b'00001110'		; 1:2 Postscaler, 1:16 Prescaler, Timer 2 start -> 1960Hz (no-flicker)

#DEFINE TMR0H_VALUE         .245             ; RX Timeout (~3ms)

; Timing for button hold-down flags
#DEFINE TMR1H_VALUE_FIRST        .255-.128  ; in steps of 7,8125ms -> 1s
#DEFINE TMR1H_VALUE_CONT         .255-.32   ; in steps of 7,8125ms -> 0.25s
#DEFINE TMR1H_VALUE_CONT_DIVE    .255-.64   ; in steps of 7,8125ms -> 0.5s

; Color Definitions: 8Bit RGB b'RRRGGGBB'
#DEFINE	color_red	    	b'11100000'     ; (7,0,0)
#DEFINE	color_dark_red	    b'10000101'     ; (4,1,1)
#DEFINE	color_violet    	b'11101011'     ; (7,2,3)
#DEFINE	color_blue		    b'11000111'     ; (6,1,3)
#DEFINE	color_green	        b'00011100'     ; (0,7,0)
#DEFINE	color_greenish      b'00111110'     ; (1,7,2)
#DEFINE color_dark_green    b'00111001'     ; (1,6,1)
#DEFINE	color_yellow 	    b'11111101'     ; (7,7,1)
#DEFINE	color_white         b'11111111'     ; (7,7,3)
#DEFINE	color_black         b'00000000'     ; (0,0,0)
#DEFINE	color_deepblue      b'00000010'     ; (0,0,2)
#DEFINE	color_grey	        b'11011111'     ; (6,7,3)
#DEFINE	color_cyan	        b'11011111'     ; (6,7,3)
#DEFINE	color_lightblue     b'11011011'     ; (6,7,3)
#DEFINE color_orange        b'11111000'     ; (7,6,0)
#DEFINE color_pink          b'11111010'     ; (7,6,2)

#DEFINE	FT_TINY             .0
#DEFINE	FT_SMALL            .1
#DEFINE	FT_MEDIUM           .2
#DEFINE	FT_LARGE            .3

#DEFINE warn_depth          .1
#DEFINE warn_cns            .2
#DEFINE warn_gf             .3
#DEFINE warn_ppo2           .4
#DEFINE warn_ceiling        .5
#DEFINE warn_gas_in_gaslist .6
#DEFINE warn_ppo2_hud       .7
#DEFINE warn_battery        .8

; External O2 cell input parameters
#DEFINE	min_mv				.80			; = 8mV
#DEFINE max_mv              .2500       ; = 250mV
#DEFINE ignore_mv           .3500       ; = 350mV (To suppress ghost readings for long, open cables)

; Profile recording parameters
#DEFINE	logbook_profile_version	0x24
#DEFINE	samplingrate_apnoe	.1	; [seconds]
; Warning: Divisors must be <.16 !
#DEFINE	div_temperature		.6	; x samplingrate [s]
#DEFINE	div_deco			.6	; x samplingrate [s]
#DEFINE	div_gf				.12	; x samplingrate [s]
#DEFINE	div_ppo2_sensors    .2	; x samplingrate [s]
#DEFINE	div_decoplan		.12	; x samplingrate [s]
#DEFINE div_cns				.12	; x samplingrate [s]
#DEFINE div_tank			.0	; x samplingrate [s]
; Warning: Information lengths must be <.16 !
#DEFINE infolength_temperature  .2  ; [byte]
#DEFINE infolength_deco         .2  ; [byte]
#DEFINE infolength_gf           .1  ; [byte]
#DEFINE infolength_ppo2_sensors .9  ; [byte]
#DEFINE infolength_decoplan     .15 ; [byte]
#DEFINE infolength_cns          .2  ; [byte]
#DEFINE infolength_tank         .0  ; [byte]

; "Better Gas" behavior
; better_gas_window <= minimum_change_depth !
; minimum_change_depth >=5 !
#DEFINE	minimum_change_depth	.3 			; [m]
#DEFINE	better_gas_window_neg   .3			; [m] (Depth above change depth)
#DEFINE	better_gas_window_pos   .1			; [m] (Depth below change depth, if ppO2<opt_ppO2_max)

; Dive mode limits and thresholds
#DEFINE	start_dive_threshold 	.100	; [cm]
#DEFINE	high_altitude_dive_threshold .300;[cm]
#DEFINE	apnoe_timeout			.15 	; [min]
#DEFINE divemode_menuview_timeout   .10 ; [s]
#DEFINE	divemode_timeout_default	.5	; [min]
#DEFINE	divemode_menu_timeout	.30		; [s]
#DEFINE	ppo2_warning_low		.19     ; [cbar] (Default value)
#DEFINE ppo2_lowest_setting     .16     ; [cbar] (Minimum value)
#DEFINE	ppo2_display_high		.120	; [cbar]
#DEFINE	ppo2_warning_high		.160    ; [cbar] (Default value)
#DEFINE ppo2_highest_setting    .160    ; [cbar] (Max. Value)
#DEFINE	cns_display_high		.70		; [%]
#DEFINE	cns_warning_high		.100    ; [%]
#DEFINE	gf_display_high     	.75		; [%]
#DEFINE	gf_warning_high         .100    ; [%]
#DEFINE	depth_warn_mbar 		.13000	; [mbar]
#DEFINE	wake_up_from_sleep		.1160	; [mbar]
#DEFINE	gf_display_threshold	.20		; [%]
#DEFINE simulator_timeout       .15     ; [s]
#DEFINE sensor_voting_logic_threshold   .20 ; Threshold in 0.01bar


; Surface mode limits and thresholds
#DEFINE	cns_display_surface_high .70	; [%]
#DEFINE	high_altitude_threshold	 .880	; [mbar]
#DEFINE	max_surfpressure		 .1080	; [mbar]
#DEFINE	timeout_surfacemode		 .90	; [s]
#DEFINE timeout_calibrate_menu   .240   ; [s]
#DEFINE timeout_ccr_surface      .240   ; [s]

; Decomodel paramters
#DEFINE no_fly_time_ratio		.60		; [%]
#DEFINE	deco_distance			.10		; [dm]

; Color-code parameters for the divemode
#DEFINE	color_code_cns_high		.100	; [%]
#DEFINE	color_code_gf_warn_high .101	; [%]
#DEFINE	color_code_velocity_warn_high .11   ;[m/min]
#DEFINE	color_code_velocity_attn_high .10   ;[m/min]

; Velocity threshold
#DEFINE	velocity_display_threshold_1	.3	; [m/min]

; Battery thresholds
#DEFINE lithium_36v_empty       .2400   ; [mV] Saft 3,6V LS14500 AA - threshold for battery percent display
#DEFINE	lithium_36v_low			.2000	; [mV] (Must be bigger then aa_15v_high!)
#DEFINE	aa_15v_high				.1550	; [mV] Energizer 1,5V E2 AA
#DEFINE	aa_15v_low				.1100	; [mV] According to Energizer Datasheet EBC-4201R, Page 2
#DEFINE color_code_battery_low  .10     ; [%]
#DEFINE battery_show_level      .24     ; [%]

; 3,6V battery sensing data points at 70mA load
#DEFINE lithium_36v_75          .3000   ; [mV]
#DEFINE lithium_36v_50          .2900   ; [mV]
#DEFINE lithium_36v_25          .2600   ; [mV]
#DEFINE lithium_36v_10          .2500   ; [mV]
	
; Capacity for 2.4Ah Saft LS14500 and 0.8Ah Panasonic UR14500P
; battery_gauge:6 is nAs
; devide through 65536
; a) devide through 364 -> Result is in percent of a 2.4Ah Battery
; or b) devide through 121 -> Result is in percent of a 0.8Ah Battery

; For internal battery gauging
#DEFINE	internal_saft_capacity		.364
#DEFINE	internal_panasonic_capacity	.121

; For gauge IC	
#DEFINE	saft_capacity		.281	; 2.4Ah/0.085mAh/100 [%]
#DEFINE	saft_offset		.37300  ; 65536-(2.4Ah/0.085mAh)

#DEFINE	panasonic_capacity	.94	; 0.8Ah/0.085mAh/100 [%]
#DEFINE	panasonic_offset	.56124	; 65536-(0.8Ah/0.085mAh)

#DEFINE	ncr18650_capacity	.364	; 3.1Ah/0.085mAh/100 [%]
#DEFINE	ncr18650_offset		.29065	; 65536-(3.1Ah/0.085mAh)
	
#DEFINE	ur16650_capacity	.271	; 2.3Ah/0.085mAh/100 [%]
#DEFINE	ur16650_offset		.38477	; 65536-(2.3Ah/0.085mAh)
	
#DEFINE	current_sleepmode           .31
#DEFINE	current_backlight_multi     .115    ; *CCPR1L+current_backlight_offset
#DEFINE	current_backlight_offset    .216
#DEFINE	current_speed_eco           .1914
#DEFINE	current_speed_normal        .4027
#DEFINE	current_speed_fastest       .5050
#DEFINE current_ir_reciever         .139
#DEFINE current_compass             .28

; Brightness thresholds (between zero (off) and 255 (max. power))
#DEFINE	ambient_light_max_high_36V	.170
#DEFINE	ambient_light_max_high_cr  	.240
#DEFINE	ambient_light_max_high_15V  .140
#DEFINE	ambient_light_min_high		.35
#DEFINE	ambient_light_max_medium 	.125
#DEFINE	ambient_light_min_medium 	.25
#DEFINE	ambient_light_max_eco		.70
#DEFINE	ambient_light_min_eco		.10         ; Must be the lowest value!

; IR Link timeout
#DEFINE ir_timeout_value            .64         ; multiples of 62,5ms

; Sp control constants
#DEFINE surface_sp                  .50         ; in cbar

; Gaslist hard-coded limits
#DEFINE gaslist_min_o2              .7          ; Minimum O2 [%]
#DEFINE gaslist_max_change_depth    .99         ; Max. change depth [m]
#DEFINE gaslist_sp_stepsize         .10         ; Steps for Setpoint Setup [cbar]
#DEFINE gaslist_sp_max              .160        ; Max. Setpoint [cbar]
#DEFINE gaslist_sp_min              .50         ; Min. Setpoint [cbar]

; Compass display
#DEFINE compass_fast_treshold       .9          ; show new heading instantly if new and old > compass_fast_treshold
#DEFINE compass_averaging           .10         ; numbers of extra averaging

; Hardware defines
#DEFINE battery_gauge_available        hardware_flag,0     ; OSTC has rechargeable battery with battery management chip
#DEFINE ambient_sensor      hardware_flag,1     ; OSTC has an ambient sensor
#DEFINE analog_o2_input     hardware_flag,2     ; OSTC has analog inputs and S8 digital
#DEFINE optical_input       hardware_flag,3     ; OSTC has an digital optical input
#DEFINE ble_available       hardware_flag,4     ; OSTC has an BLE module
;unused
;unused
;unused

;---------------------------- Macros ------------------------------------

TSTOSS  macro opt_reg           ; TeST Option Skip if not Zero
    movff   opt_reg,WREG
    tstfsz  WREG,A
    bra     $+4
    endm

;---------------------------- Bank0 DATA ------------------------------------
isr_backup  equ		    0x60
isr_backup  udata_ovr   isr_backup    ; Reserved space for interupt data

;---- Backup for general registers
isr_prod    res 2

;---- MS5541 subroutines
amb_pressure	res	2
temperature		res	2

C1              res 2           ; Decoded calibration data
C2              res 2
C3              res 2
C4              res 2           ; Here: C4-250
C5              res 2           ; Here: Reference Temperature UT1 = 8*C5 + 10000 (u16 range 10.000 .. +42.760)
C6              res 2
D1              res 2           ; raw pressure
D2              res 2           ; raw temperature
xdT				res	2
xdT2			res	2
OFF				res	2
SENS			res	2
dLSB            res 1           ;Pressure sensor interface 
dMSB            res 1
clock_count     res 1
sensor_state_counter res 1		; counts to eight for state maschine
amb_pressure_avg res 2
temperature_avg	res	2
minimum_temperature	res 2		; minimum temperature
last_temperature	res 2
last_pressure	res 2
last_pressure_velocity	res 2   ; For velocity
last_surfpressure res 2
last_surfpressure_15min res 2
last_surfpressure_30min res 2
rel_pressure	res 2
sim_pressure	res 2   ; hold simulated pressure in mbar if in Simulator mode
max_pressure	res 2
avg_rel_pressure	res 2
avg_rel_pressure_total res 2
;---- Data for ISR math subroutines
isr_xC          res 4
isr_xA          res 2
isr_xB          res 2

;---- Data for ISR routines
isr1_temp       res 1           ; ISR temp variable, used in ms5541.asm, isr_rtcc, isr_battery_gauge
isr2_temp       res 1           ; ISR temp variable, used isr_battery_gauge

;---- Display brightness
max_CCPR1L		res	1			; Max. brightness value for CCPR1L

; Battery gauge (nAs, nC)
battery_gauge	res	6			; 48Bit -> 78Ah max...

; IR/S8-Link
ir_s8_buffer        res .18     ; temporally used in get_calibration_data for the raw (packed) calibration data
ir_s8_counter       res 1
ir_S8_timeout       res 1       ; Timeout for valid data

; Compass raw data
compass_DX          res 2
compass_DY          res 2
compass_DZ          res 2

accel_DX            res 2
accel_DY            res 2
accel_DZ            res 2

; Compass FILTERED data
compass_DX_f        res 2
compass_DY_f        res 2
compass_DZ_f        res 2

accel_DX_f          res 2
accel_DY_f          res 2
accel_DZ_f          res 2

; Compass calibration data
compass_CX_f        res 2
compass_CY_f        res 2
compass_CZ_f        res 2

; Compass more data
compass_heading     res 2       ; Corrected heading (in 1°) : -180 .. 180
compass_heading_old res 2       ; Old heading (For smoother display)
compass_heading_shown res 2     ; Displayed heading
;compass_roll        res 2       ; Rotation around the X axis
;compass_pitch       res 2       ; Rotation around the Y axis

compass_a           res 2       ; Tmp data for Q15 arithmetics
compass_b           res 2
compass_r           res 3

CNS_start           res 2       ; CNS value at beginning of dive
GF_start            res 1       ; GF value at beginning of dive

speed_setting        	res 1           ; =1: Eco, =2: Normal, =3: Fastest

;---- Data for graphical compass
xRD             res 2       ; virtual compass ruler offset
xRDr            res 2       ; virtual compass ruler offset - right end
xRD180          res 2       ; virtual compass ruler offset for the -180 marker
xLO             res 1       ; lo backup to prevent trashing
xHI             res 1       ; hi backup to prevent trashing
xCM             res 1       ; compass bearing relative position
compass_bearing res 2       ; this is where the bearing menu stores the actual heading for bearing

;---------------------------- Common DATA ------------------------------------

common          equ         0x100       ; Alias for "banksel common"
common          udata_ovr   common      ; Bank1 general variables

;---- Time and Date
secs         	   		res 1           
mins            		res 1
hours           		res 1
day             		res 1
month           		res 1
year            		res 1
surface_interval		res 2

flag1            		res 1
flag2           		res 1
flag3           		res 1
flag4           		res 1
flag5           		res 1
flag6           		res 1
flag7           		res 1
flag8           		res 1
flag9           		res 1
flag10           		res 1
flag11           		res 1
flag12           		res 1
flag13                  res 1
flag14			res 1

hardware_flag           res 1           ; hardware descriptor flag

temp1			res 1
temp2			res 1

;---- Interface to wait macros
wait_temp       res 1
waitms_temp     res 1

;---- Interface to graphic subroutines (aa_wordprocessor, color_processor, ...)
win_leftx2      res 1
win_top         res 1
win_width       res 2
win_height      res 1
win_bargraph    res 1
win_color1      res 1
win_color2      res 1
win_font        res 1

;---- Draw subroutines
tft_temp4       res 1
tft_temp3       res 1
tft_temp2       res 1
tft_temp1       res 1

;---- data conversion subroutines
lo              res 1
hi              res 1
up				res 1

;---- RS232 temp variables
uart1_temp      res 1
uart2_temp      res 1
uart3_temp      res 1

;---- Data for math subroutines
divA            res 2
divB            res 1
xC              res 4
xA              res 2
xB              res 2
sub_c           res 2
sub_a           res 2
sub_b           res 2

;---- Data for conversion subroutines
cvt_flags       res 1
ignore_digits   res 1

#define leftbind            cvt_flags,0
#define ignore_digit3       cvt_flags,1
#define ignore_digit4       cvt_flags,2
#define ignore_digit5       cvt_flags,3
#DEFINE enable_screen_dumps cvt_flags,4 ; =1: Ignore vin_usb, wait for "l" command (Screen dump
#DEFINE screen_type         cvt_flags,5 ; =1: display1, =0; display0
#DEFINE	compass_type	    cvt_flags,6 ; =1: compass1, =0: compass0
#DEFINE	analog_switches	    cvt_flags,7 ; =1: Analog switches available
;---- Misc.
nofly_time              res 2   		; No Fly time in Minutes (Calculated after Dive)
timeout_counter 		res 1
timeout_counter2		res 1
timeout_counter3		res 1
batt_voltage			res 2			; Battery voltage in mV
batt_percent            res 1           ; Battery in percent (1-100)
ambient_light			res 2			; ambient_light level
lo_temp					res 1
hi_temp					res 1
desaturation_time		res 2  
convert_value_temp      res 3           ; used in menu_battery_state_convert_date
active_gas				res 1
active_diluent          res 1           ; As a backup when switching back from Bailout to CCR
warning_counter			res 1			; Counts amount of warning in divemode
warning_counter_backup	res 1			; Backup of warning_counter
warning_page            res 1           ; current # of warning page

; Remind history for menu processor (and such):
menustack               res 5           ; menu stack
menupos					res 1           ; Logbook and divemode menu
menupos2				res 1           ; For dive mode simulator and pre-menu
menupos3				res 1           ; For Customviews
menupos4				res 1           ; For divemode menu

; Apnoe stuff that must be in bank common
apnoe_mins				res 1
apnoe_secs				res 1
apnoe_max_pressure		res 2

; Divemode
divemins				res 2
divesecs				res 1
samplingrate            res 1
samplesecs_value		res 1
divisor_temperature		res 1
divisor_deco			res 1
divisor_gf				res 1
divisor_ppo2_sensors	res 1
divisor_decoplan		res 1
divisor_cns				res 1
divisor_tank            res 1
average_depth_hold      res 4
average_depth_hold_total res 4
total_divetime_seconds	res 2
average_divesecs        res 2   ; For the resettable stopwatch
decodata				res 2
apnoe_timeout_counter 	res 1
apnoe_surface_mins 		res 1
apnoe_surface_secs		res 1

; Profile storing
AlarmType			res 1
samplesecs			res 1
EventByte			res 1
EventByte2			res 1
ProfileFlagByte		res 1

; External flash
ext_flash_address		res 3	; 24bit Address
ext_flash_log_pointer	res 3	; 24bit Address for logbook profile storing
ext_flash_dive_counter  res 3   ; 24bit Counter for dive length (increased in write_byte_ext_flash_plus)

; I2C
i2c_temp            res 2       ; Temp

; O2 Sensor data
o2_mv_sensor1       res 2       ; in 0.1mV steps
o2_mv_sensor2       res 2       ; in 0.1mV steps
o2_mv_sensor3       res 2       ; in 0.1mV steps
o2_ppo2_sensor1     res 1       ; Sensor1 ppO2 (in 0.01bar steps)
o2_ppo2_sensor2     res 1       ; Sensor2 ppO2 (in 0.01bar steps)
o2_ppo2_sensor3     res 1       ; Sensor3 ppO2 (in 0.01bar steps)
hud_status_byte     res 1
;Status Byte/
#DEFINE hud_connection_ok       hud_status_byte,0	;Bit0 = 1 -> HUD connection ok
;Bit1 = 1 -> HUD is calibrated
;Bit2 = 1 -> HUD Battery is low (<3000mV)
#DEFINE	sensor1_active          hud_status_byte,3	;=1: Sensor1 is active
#DEFINE	sensor2_active          hud_status_byte,4   ;=1: Sensor2 is active
#DEFINE	sensor3_active          hud_status_byte,5   ;=1: Sensor3 is active
;Bit6 unused in stand-alone HUD
;Bit7 unused in stand-alone HUD

sensor_setpoint     res 1       ; sensor ppo2 in 0.01bar for deco routine

better_gas_number	res 1       ; number (1-5) of the "better gas" in divemode, =0: no better gas available
customview_divemode res 1       ; keeps last custom view
customview_surfmode res 1
safety_stop_countdown	res 1	; counts seconds of safety stop

;---------------------------- TMP DATA ---------------------------------------
; Space for various overlayed data from color, word, menu processors, etc.
; 

tmp             udata_ovr   0x1A0       ; Bank1 shared TMP space
tmp             equ         0x1A0       ; Alias for CBLOCK directive.
                res         0x05F       ; RESERVED area.

;---------------------------- Common2 DATA ------------------------------------

common2          equ         0xA00       ; Alias for "banksel common"
common2          udata_ovr   common2     ; Bank1 general variables

s8_rawdata_sensor1   res 3  ; 24bit A/D raw data from S8 HUD sensor 1
s8_rawdata_sensor2   res 3  ; 24bit A/D raw data from S8 HUD sensor 2
s8_rawdata_sensor3   res 3  ; 24bit A/D raw data from S8 HUD sensor 3

hud_battery_mv      res 2   ; hud/ppo2 monitor battery voltage in mV
start_day           res 1   ; At start of dive (For logbook)
start_month         res 1   ; At start of dive (For logbook)
start_year          res 1   ; At start of dive (For logbook)
start_hours         res 1   ; At start of dive (For logbook)
start_mins          res 1   ; At start of dive (For logbook)

old_velocity        res 4   ; Stores the last 4 speeds (8 seconds) in m/min
opt_gas_type_backup res 5   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
opt_dil_type_backup res 5   ; 0=Disabled, 1=First, 2=Normal
opt_OC_bail_gas_change_backup  res 5   ; Gas change depths OC/Bailout

internal_battery_capacity	res 2	; For internal battery gauging
battery_capacity		res 2	; For battery gauge IC
battery_offset			res 2	; For battery gauge IC

analog_sw1_raw		res 2	; idle values (average)
analog_sw2_raw		res 2	; idle values (average)
analog_counter		res 1	; for averaging
analog_sw1		res 1	; analog value for switch 1
analog_sw2		res 1	; analog value for switch 2
battery_type		res 1	; =0:1.5V, =1:3,6V Saft, =2:LiIon 3,7V/0.8Ah, =3:LiIon 3,7V/3.1Ah, =4: LiIon 3,7V/2.3Ah
uptime			res 4	; Uptime [s]
lastdive_time		res 4	; Time since last dive [s]
battery_acumulated_charge   res 2   ; Raw values in battery gauge IC
gauge_status_byte	res 1	 ; Gauge IC status byte
button_polarity		res 1	; 0xFF (Both normal), 0x00 (Both inverted), 0x01 (Left inverted only), 0x02 (Right inverted only) 

;-----------------------------------------------------------------------------
; Reserve BANK2 for general purpose buffer (strings, images, etc).
; NOTE: Needs to be aligned with a bank (LOW(buffer)==0).
buffer          udata_ovr   0x200
buffer          res .256

opt_table       equ         0xE00
opt_table       udata_ovr   opt_table

;---- Dive Options
opt_gas_O2_ratio        res 5   ; Needed because deco gaslist use N2
opt_dil_O2_ratio        res 5   ; Must be in line with opt_gas_O2_ratio
opt_gas_He_ratio        res 5   ; He ratio Gas1-5
opt_dil_He_ratio        res 5   ; Must be in line with opt_gas_He_ratio
opt_gas_type            res 5   ; 0=Disabled, 1=First, 2=Travel, 3=Deco
opt_dil_type            res 5   ; 0=Disabled, 1=First, 2=Normal
opt_dive_mode           res 1   ; 0=OC, 1=CC, 2=Gauge, 3=Apnea, 4=PSCR
opt_ppO2_max            res 1   ; PPO2 Max for MOD calculation and color coding in divemode
opt_ccr_mode            res 1   ; =0: Fixed SP, =1: Sensor
opt_ppO2_min            res 1   ; PPO2 min for Sensors and color coding in divemode
opt_OC_bail_gas_change  res 5   ; Gas change depths OC/Bailout
;---- Managing Settings

opt_brightness          res 1   ; =0: Eco, =1:Medium, =2:Full
opt_salinity            res 1	; 0-5%
#DEFINE opt_name_length    .60  ; Custom text string 5 rows with 12 chars
opt_name                res opt_name_length
opt_language            res 1   ; Current language. 0=EN, 1=DE, 2=FR, 3=SP
opt_units               res 1   ; 0:m/°C, 1:ft/°F
opt_dateformat			res	1	; =0:MMDDYY, =1:DDMMYY, =2:YYMMDD
opt_last_stop           res 1   ; =3:3m, =4:4m, =5:5m, =6:6m
opt_aGF_high            res 1   ; Alternative GF HIGH
opt_aGF_low             res 1   ; Alternative GF LOW
opt_GF_high             res 1   ; GF HIGH
opt_GF_low              res 1   ; GF LOW
opt_enable_aGF          res 1   ; =1: aGF can be selected underwater
opt_compass_gain        res 1   ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
opt_sampling_rate       res 1   ; =1: 10s, =0: 2s
opt_dive_color_scheme   res 1   ; 0-3
opt_pressure_adjust     res 1   ; SIGNED int (two's complement), -20/+20mbar max.
opt_enable_safetystop   res 1   ; =1: A safety stop is shown
opt_calibration_O2_ratio    res 1   ; %O2 of calibration gas
opt_x_s1                res	2	; calibration factor (Not stored in EEPROM)
opt_x_s2                res	2	; calibration factor (Not stored in EEPROM)
opt_x_s3                res	2	; calibration factor (Not stored in EEPROM)
opt_sensor_fallback     res 1   ; =1: Fallback to SP1 when sensor is lost
opt_flip_screen         res 1   ; =1: Flip the screen
opt_cR_button_left      res 1   ; Left button sensitivity (cR hardware)
opt_cR_button_right     res 1   ; Right button sensitivity (cR hardware)
opt_modwarning          res 1   ; =1:do a red blinking warning, =0:default behaviour
opt_vsitextv2           res 1   ; =1:use the depth dependent ascend rate limits
opt_vsigraph            res 1   ; =1:draw the graphical VSI bar
opt_showppo2            res 1   ; =1:always show the ppO2 value in the warning position
opt_temperature_adjust  res 1   ; SIGNED int (two's complement), -2.0/+2.0 °C max.
opt_safety_stop_length  res 1   ; [s]
opt_safety_stop_start   res 1   ; [cbar]
opt_safety_stop_end     res 1   ; [cbar]
opt_safety_stop_reset   res 1   ; [cbar]
opt_diveTimeout		res 1	; Timeout for Divemode [min]
opt_PSCR_show_ppO2	res 1	; =1: Show PSCR ppO2 (Instead of OC ppO2)
opt_PSCR_drop		res 1	; PSCR Drop [%]
opt_PSCR_lungratio	res 1	; PSCR Lung Ratio [1/x]

;-----------------------------EEPROM DATA ------------------------------------
; Automatic reset of all options when this is changed:
#define    eeprom_opt_serial   0x0005   ; Version 0.5

;-----------------------------------------------------------------------------
; Flags
#DEFINE	switch_left				flag1,0
#DEFINE	switch_right			flag1,1			
#DEFINE neg_flag				flag1,2	; e.g. sub16 (sub_c = sub_a - sub_b)
#DEFINE pressure_refresh 		flag1,3	; =1: A new pressure/temperature is available
#DEFINE	sleepmode				flag1,4	; =1: In Sleepmode
#DEFINE	tft_is_dimming			flag1,5	; =1: The TFT is dimming, ignore light sensor
#DEFINE	display_velocity		flag1,6	; =1: show velocity
#DEFINE	no_sensor_int			flag1,7	; =1: block any further access to pressure sensor

#DEFINE	rs232_recieve_overflow	flag2,0	; =1: An RS232 timeout overflow occoured
#DEFINE	stored_gas_changed		flag2,1	; =1: Stored Gas changed
#DEFINE	high_altitude_mode		flag2,2	; =1: Unit was manually turned on with ambient pressure <880mbar
#DEFINE	FLAG_apnoe_mode			flag2,3	; =1: Apnoe mode selected
#DEFINE	restore_deco_data		flag2,4	; =1: Restore deco data (After simulation)
#DEFINE premenu					flag2,5	; =1: Premenu/Divemenu selected
#DEFINE	menubit					flag2,6	; menu
#DEFINE	simulatormode_active	flag2,7	; =1: Simulator mode active, override pressure sensor readings

#DEFINE divemode_menu           flag3,0	; =1: Divemode menu is shown
#DEFINE	onesecupdate			flag3,1	; =1 after any second
#DEFINE twosecupdate			flag3,2	; =1: New two-second update
#DEFINE	toggle_customview		flag3,3	; =1: Next customview
#DEFINE	oneminupdate			flag3,4	; =1 after any minute
#DEFINE	divemode				flag3,5	; =1: in Divemode
#DEFINE	battery_is_36v          flag3,6	; =1: 3,6V Battery is in use
#DEFINE	warning_active          flag3,7	; =1: A warning is active in divemode or surfacemode

#DEFINE	better_gas_available	flag4,0	; =1: A better gas is available
#DEFINE	blinking_better_gas		flag4,1	; =1: Gas is blinking
#DEFINE menuview                flag4,2 ; =1: A menuview is shown in divemode (e.g. "Menu?")
#DEFINE quarter_second_update   flag4,3 ; =1: A new 1/4 second has begun
#DEFINE divemode_gaschange      flag4,4 ; =1: The gas will change very soon
#DEFINE	decostop_active			flag4,5	; =1: Decompression stop shown
#DEFINE depth_greater_100m      flag4,6 ; =1: Current Depth > 100m
#DEFINE	realdive				flag4,7	; =1: A real dive during divemode

#DEFINE	dive_warning_displayed	flag5,0	; =1: The warning sign is shown
#DEFINE	reset_average_depth		flag5,1	; =1: Reset the average depth
#DEFINE	store_sample			flag5,2	; =1: Store a new sample
#DEFINE	divemode2				flag5,3	; =1: Dive longer then one minute
#DEFINE	FLAG_active_descent		flag5,4	; Used in Apnoe mode
#DEFINE	event_occured			flag5,5	; =1: An event occured, store it!
#DEFINE	divemode_menu_active	flag5,6 ; =1: The divemode menu is shown
#DEFINE	temp_changed			flag5,7	; =1: The temperature changed

#DEFINE gas6_changed            flag6,0	; =1: Gas 6 has been selected/changed underwater
#DEFINE	onehourupdate			flag6,1	; =1: A new hour has just begun
#DEFINE	settime_setdate			flag6,2	; =1: In the Set Time or Set Date Menu
#DEFINE setpoint_changed        flag6,3	; =1: Setpoint has been changed
#DEFINE second_row_warning      flag6,4	; =1: The second row contains a warning
#DEFINE FLAG_ccr_mode           flag6,5	; =1: CCR mode (Fixed ppO2 or Sensor) active
#DEFINE dive_hud1_displayed     flag6,6 ; =1: The Sensor1 HUD reading is displayed
#DEFINE dive_hud2_displayed     flag6,7 ; =1: The Sensor2 HUD reading is displayed

#DEFINE dive_hud3_displayed     flag7,0 ; =1: The Sensor3 HUD reading is displayed
#DEFINE no_more_divesecs        flag7,1 ; =1: Do no longer show seconds in divemode
#DEFINE FLAG_gauge_mode         flag7,2 ; =1: In Gauge mode
#DEFINE ignore_last_edited_gas  flag7,3 ; Used in gaslist_cleanup_list
#DEFINE ccr_diluent_setup       flag7,4 ; =1: Setting up Diluents ("Gas6-10")
#DEFINE menu_show_sensors       flag7,5 ; =1: Update HUD data in menu
#DEFINE short_gas_decriptions   flag7,6 ; =1: Use short versions of gaslist_strcat_gas_mod and gaslist_strcat_setpoint
#DEFINE max_depth_greater_100m  flag7,7 ; =1: Max. Depth > 100m

#DEFINE is_bailout              flag8,0 ; =1: Bailout
#DEFINE is_bailout_menu         flag8,1 ; =1: Bailout
#DEFINE toggle_gf               flag8,2 ; =1: Toggle GF/aGF
#DEFINE use_agf                 flag8,3 ; =1: Use aGF (instead of GF)
#DEFINE battery_removed_in_usb  flag8,4 ; =1: The battery has been removed in USB
#DEFINE adc_running             flag8,5 ; =1: The ADC is in use
#DEFINE comm_service_enabled    flag8,6 ; =1: COMM Service mode unlocked
#DEFINE compass_enabled         flag8,7 ; =1: The compass and accelerometer chip is active

#DEFINE compass_fast_mode       flag9,0 ; =1: The compass is in fast mode
#DEFINE in_color_menu           flag9,1 ; =1: In the color scheme menu
#DEFINE bailoutgas_event        flag9,2 ; =1: bailout was selected or a gaschange during bailout
#DEFINE win_invert              flag9,3 ; =1: Invert font output
#DEFINE show_safety_stop	flag9,4	; =1: Show the safety stop
#DEFINE	safety_stop_active      flag9,5	; =1: The safety stop is currently displayed
#DEFINE new_s8_data_available   flag9,6 ; =1: New data frame recieved
#DEFINE print_compass_label     flag9,7 ; =1: print the graphical comapss label

#DEFINE s8_digital              flag10,0; =1: Digital I/O
#DEFINE menu_show_sensors2      flag10,1; =1: Update mV data in calibration menu
#DEFINE use_O2_sensor1          flag10,2; =1: Use this sensor for deco
#DEFINE use_O2_sensor2          flag10,3; =1: Use this sensor for deco
#DEFINE use_O2_sensor3          flag10,4; =1: Use this sensor for deco
#DEFINE setpoint_fallback       flag10,5; =1: Fallback to SP1 due to external O2 sensor failure
#DEFINE	blinking_setpoint       flag10,6; =1: SP is blinking
#DEFINE flip_screen             flag10,7; =1: Screen is flipped by 180°

#DEFINE sensor1_calibrated_ok   flag11,0; =1: This sensor has been calibrated ok
#DEFINE sensor2_calibrated_ok   flag11,1; =1: This sensor has been calibrated ok
#DEFINE sensor3_calibrated_ok   flag11,2; =1: This sensor has been calibrated ok
#DEFINE voting_logic_sensor1    flag11,3; =1: This sensor is within the voting logic threshold
#DEFINE voting_logic_sensor2    flag11,4; =1: This sensor is within the voting logic threshold
#DEFINE voting_logic_sensor3    flag11,5; =1: This sensor is within the voting logic threshold
#DEFINE cc_active               flag11,6; =1: Constant Current active (cR Hardware)
#DEFINE cv_active               flag11,7; =1: Constant Voltage active (cR Hardware)

#DEFINE compass_bearing_set     flag12,0    ; bearing set
#DEFINE compass_bearing_eq      flag12,1    ; bearing is in direction? do not show << or >> 
#DEFINE compass_bearing_lft     flag12,2    ; bearing is to the left/<< if set, otherwise to the right/>>
#DEFINE compass_bearing_vis     flag12,3    ; bearing is visible? (either ahead or behind/-180°)
#DEFINE compass_bearing_ahd     flag12,4    ; bearing is ahead or behind?
#DEFINE blinking_depth_warning  flag12,5    ; set by the color_depth. =1: warning color
#DEFINE blinking_depth_prev     flag12,6    ; set by the TFT_depth.   =1: prev display had warning color
#DEFINE blinking_depth_toggle   flag12,7    ; toggle to blink

#DEFINE neg_flag_velocity       flag13,0    ; neg_flag backup for velocity logic
#DEFINE	analog_sw1_pressed	flag13,1    ; =1: Analog switch 1 pressed
#DEFINE	analog_sw2_pressed	flag13,2    ; =1: Analog switch 2 pressed
#DEFINE sp2_switched            flag13,3    ; =1: This setpoint has been autoselected already
#DEFINE sp3_switched            flag13,4    ; =1: This setpoint has been autoselected already
#DEFINE sp4_switched            flag13,5    ; =1: This setpoint has been autoselected already
#DEFINE sp5_switched            flag13,6    ; =1: This setpoint has been autoselected already
#DEFINE	use_old_batt_flag	flag13,7    ; =1: load old battery information after power-on reset
		
#DEFINE	FLAG_pscr_mode		flag14,0    ; =1: OSTC is in PSCR mode

;=============================================================================
; C-code Routines
; PART 2
    extern deco_calc_CNS_decrease_15min    
    extern deco_calc_CNS_fraction
    extern deco_calc_desaturation_time
    extern deco_calc_hauptroutine
    extern deco_calc_tissue
    extern deco_calc_percentage
    extern deco_calc_wo_deco_step_1_min
    extern deco_calc_dive_interval
    extern deco_clear_CNS_fraction
    extern deco_clear_tissue
    extern deco_pull_tissues_from_vault
    extern deco_push_tissues_to_vault
	