;=============================================================================
;
;    File text_english.asm
;
;    English texts reference file.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================

; Basic texts
	TCODE	tNo,        "No"            ; No
	TCODE	tYes,       "Yes"           ; Yes = No + 1

; Surface-mode texts
    TCODE   tBeta,      "beta"         	; beta
	TCODE	tMenu,		"<Menu" 		; <Menu
    TCODE   tView,      "View>"         ; View>
    TCODE   tHeading,   "Heading:"      ; Heading:

; Divemode Menu
    TCODE   tDivemenu_Gaslist,  "Gaslist"       ; Gaslist
    TCODE   tDivemenu_ResetAvr, "Reset Avr."    ; Reset Avr.
    TCODE   tDivemenu_Setpoint, "Setpoint"      ; Setpoint
    TCODE   tDivemenu_UseSensor,"Use Sensor"    ; Use Sensor
    TCODE   tDivemenu_ToggleGF, "Toggle GF"     ; Toggle GF

; Main menu
    TCODE   tNext,      "<Next"         ; <Next
    TCODE   tEnter,     "Enter>"        ; Enter>

    TCODE   tMainMenu,  "Main Menu"     ; MainMenu
    TCODE   tLogbook,   "Logbook"       ; Logbook
    TCODE   tGasSetup,  "OC Gas Setup"  ; OC Gas Setup
    TCODE   tSimulator, "Simulator"     ; Simulator
    TCODE   tSetTime,   "Set Time"		; Set Time
	TCODE	tSetDate,	"Set Date"		; Set Date
	TCODE	tSetTimeDate,"Set Time & Date"; Set Time & Date
    TCODE   tDispSets,  "Display Settings" ; Display Settings
    TCODE   tExit,      "Exit"          ; Exit
	TCODE	tResetMenu, "Reset Menu"	; Reset Menu
	TCODE	tDiveModeMenu,"Deco Mode"	; Deco Mode
	TCODE	tInfoMenu,	"Information"	; Information
    TCODE   tCCRSetup,  "CCR Setup"     ; CCR Setup
    TCODE   tDiluentSetup,"Diluent Setup"   ; Diluent Setup
    TCODE   tFixedSetpoints,"Fixed Setpoints" ; Fixed Setpoints
    TCODE   tCCRSensor, "CCR Sensor"    ; CCR Sensor

; Gas menu
    TCODE   tGaslist,   "OC Gas List"
    TCODE   tGaslistCC, "CC Diluents"
    TCODE   tGasEdit,   "Edit Gas"
    TCODE   tType,      "Type: "
    TCODE   tGasDisabled,"Disabled"  ; Disabled
    TCODE   tGasFirst,   "First"     ; First
    TCODE   tGasTravel,  "Travel"    ; Travel
    TCODE   tGasDeco,    "Deco"      ; Deco
    TCODE   tDilDisabled,"Disabled"  ; Disabled
    TCODE   tDilFirst,   "First"     ; First
    TCODE   tDilNorm,   "Normal"     ; Normal
    TCODE   tAir,       "Air  "         ; Enum: values must follows (5 chars)
    TCODE   tO2,        "O2   "         ; tAir + 5
    TCODE   tO2Plus,    "O2 +"
    TCODE   tO2Minus,   "O2 -"
    TCODE   tHePlus,    "He +"
    TCODE   tHeMinus,   "He -"
    TCODE   tMOD,       "MOD:"
    TCODE   tEAD,       "EAD:"
    TCODE   tGasDepth,  "Change depth"
    TCODE   tDepthPlus, "Depth +"
    TCODE   tDepthMinus,"Depth -"
    TCODE   tDepthReset,"Reset to MOD:"
    TCODE   tSetup_mix, "Setup Mix"
    TCODE   tCCRMode,   "CCR Mode:"             ; CCR Mode:
    TCODE   tCCRModeFixedSP,  "Fixed SP"        ; Fixed SP
    TCODE   tCCRModeSensor,   "Sensor"          ; Sensor
    TCODE   tSP,            "SP"                ; SP (SetPoint)
    TCODE   tSPPlus,    "pO2+"                  ; pO2+

; New battery menu
	TCODE   tNewBattTitle,      "New Battery?"
	TCODE	tNewBattOld,		"Keep old"
	TCODE	tNewBattNew36,		"New 3.6V"
	TCODE	tNewBattNew15,		"New 1.5V"

; Gaslist management
    TCODE   tGas,       "Gas"           ; Gas
    TCODE   tNx,        "Nx "          	; Nx   (3 chars)
    TCODE   tTx,        "Tx "          	; Tx   (3 chars)
    TCODE   tGasErr,    "Err"           ; Err  (3 chars)
        
; Communication Menu
    TCODE   tUsbMode,  "Start USB"
	TCODE	tUsbTitle,	"USB Mode"
	TCODE	tUsbStarting,  "Starting..."
	TCODE	tUsbStartDone, "Done.      "
	TCODE	tUsbServiceMode, "Service mode enabled"
	TCODE	tUsbClosed,		"Port closed"
	TCODE	tUsbExit,		"Exited"
	TCODE	tUsbDownloadMode, "Download mode enabled"
    TCODE   tUsbLlBld,      "Low-level Bootloader"

; Dive Settings
    TCODE   tDvMode,    "Dive Mode:"
    TCODE   tDvOC,      "OC"
    TCODE   tDvCC,      "CC"
    TCODE   tDvGauge,   "Gauge"
    TCODE   tDvApnea,   "Apnea"
    TCODE   tDkMode,    "Decotype:"
    TCODE   tZHL16,     "ZH-L16"
    TCODE   tZHL16GF,   "ZH-L16+GF"
    TCODE   tPPO2Max,   "ppO2 Max:"
    TCODE   tPPO2Min,   "ppO2 Min:"
    TCODE   tLastDecostop, "Last Deco:"
    TCODE   tDecoparameters, "Deco Parameters"
    TCODE   tGF_low,    "GF low:"
    TCODE   tGF_high,   "GF high:"
    TCODE   tSaturationMult,    "Saturation:"
    TCODE   tDesaturationMult,  "Desaturation:"
    TCODE   tFTTSMenu,  "Future TTS:"            ; Future TTS
    TCODE   taGFMenu,   "Alternative GF"         ; Alternative GF
    TCODE   taGF_low,   "aGF low:"               ; aGF low
    TCODE   taGF_high,  "aGF high:"              ; aGF high
    TCODE   taGF_enable,"aGF Selectable:"        ; aGF Selectable
    TCODE   tDiveaGF_active,"aGF!"          ; aGF!

; Display Settings
    TCODE   tBright,    "Brightness:"
    TCODE   tEco,       "Eco"
    TCODE   tMedium,    "Medium"
    TCODE   tHigh,      "High"
    TCODE   tDvSalinity,"Salinity:"             ; Salinity

; Setup Menu
    TCODE   tSystSets,  "Settings"
    TCODE   tLanguage,  "Language:"
    TCODE   tEnglish,   "English"
    TCODE   tGerman,    "German"
    TCODE   tFrench,    "French"
    TCODE   tItalian,   "Italian"
    TCODE   tCompassMenu,"Compass calibration"  ; Compass calibration
    TCODE   tCompassGain,"Compass Gain:"         ; Compass gain:
    TCODE   tcharx,     "x"

    TCODE   tUnits,     "Units:"
    TCODE   tMetric,     " m/°C"            ; Enum menu
    TCODE   tImperial,   "ft/°F"
	
    TCODE   tDefName,   "HW OSTC3"
	TCODE	tbar,		"bar"				; bar

; Units for all menu
    TCODE   tMeters,    "m"
	TCODE	tFeets,		"ft"
	TCODE	tFeets1,	"f"
    TCODE   tMinutes,   "'"
    TCODE   tPercent,   "%"

; Date
	TCODE	tDateFormat, "Date:"
	TCODE	tDateformat, "MMDDYY"
	TCODE	tDateformat1,"DDMMYY"
	TCODE	tDateformat2,"YYMMDD"

; Simulator menu
    TCODE   tInter, 	"Simulator"         ; Dive Simulator
    TCODE   tPlan,  	"Deco Planner"      ; Deco Planner

; Decoplanner submenu
    TCODE   tBtTm,  	"Bot. Time:"        ; Bot. Time: (10 chars)
    TCODE   tMxDep, 	"Max Depth:"        ; Max Depth: (10 chars)
    TCODE   tIntvl, 	"Interval :"        ; Interval : (10 chars)
    TCODE   tDeco,  	"Calculate Deco"    ; Calculate Deco
    TCODE   tDivePlan,  "Dive Plan:"        ; Dive Plan:
    TCODE   tNoDeco,    "No Deco"           ; No Deco
    TCODE   tMore,      "More..."           ; More...
    TCODE   tCNS,       "CNS"               ; CNS

; Information menu
    TCODE   tFirmware,  "Firmware: "        ; Firmware: (space)
    TCODE   tSerial,    "Serial  : "        ; Serial  : (space)
	TCODE	tTotalDives,"Total Dives: "		; Total Dives:
    TCODE   tBatteryV,  "Battery: "         ; Battery:
    
; Divemode screen
    TCODE   tNDL,       "NDL"
    TCODE   tTTS,   	"TTS"
	TCODE	tVelMetric,	 "m/min"
	TCODE	tVelImperial,"ft/m "
	TCODE	tGasSelect,  "Select Gas"       ; Select Gas
	TCODE	tSelectAir,	 "Air "     		; Air
	TCODE	tSelectO2,	 "O2  "             ; O2
    TCODE   tSelectNx,   "Nx"               ; Nx
    TCODE   tSelectTx,   "Tx"               ; Tx
    TCODE   tDepth,      "Depth"            ; Depth
    TCODE   tMaxDepth,   "Max. Depth"       ; Max. Depth
    TCODE   tDivetime,   "Divetime"         ; Divetime
    TCODE   tDiveHudMask1,  "Sensor 1"
    TCODE   tDiveHudMask2,  "Sensor 2"
    TCODE   tDiveHudMask3,  "Sensor 3"
    TCODE   tDiveTotalAvr,  "Total Avr"
    TCODE   tDiveStopwatch, "Stopwatch"
    TCODE   tDiveStopAvr,   "Stopped Avr"
    TCODE   tApnoeTotal,    "Total Divetime:"; Total Divetime
    TCODE   tApnoeMax,      "Last Descent"  ; Last descent
    TCODE   tApnoeSurface,  "Surface Time"  ; Surface Time
    TCODE   tDiveDecoplan,  "Decoplan"      ; Decoplan
    TCODE   tDiveClock,     "Clock"         ; Clock
    TCODE   tDiveEAD_END,   "EAD/END"       ; EAD/END
    TCODE   tDiveTissues,   "Tissues"       ; Tissues
    TCODE   tEND,           "END:"          ; END:
    TCODE   tHe,            "He"            ; He
    TCODE   tN2,            "N2"            ; N2
    TCODE   tDiveBailout,   "Bailout"       ; Bailout
    TCODE   tGFactors,      "GF Values"     ; GF Values
    TCODE   taGFactors,     "aGF Values"    ; aGF Values
    TCODE   tGFInfo,        "Current GF"    ; GF Info

; Divemode menu
    TCODE   tDivePreMenu,   "Menu?"         ; Menu?

; Simulator menu
	TCODE	tQuitApnea,	 "Quit?"            ; Quit Apnea mode?
	TCODE	tQuitSim,	 "Quit?"            ; Quit Simulation?
	TCODE	tDescent1m,	 "Sim:-1m"          ; Descent 1m
	TCODE	tAscent1m,	 "Sim:+1m"          ; Ascent 1m

; Logbook
	TCODE	tCNS2,		"CNS:"
	TCODE	tAVR,		"Avr:"
	TCODE	tGF,		"GF:"
	TCODE	tSAT,		"Sat:"					; Sat:

; Logbook units
	TCODE	tLogTunitC,	"°C"	
	TCODE	tLogTunitF,	"°F"
	TCODE	tKGL,		"kg/l"
	TCODE	tMBAR,		"mbar"

; Logbook menu
	TCODE	tNextLog, 	"Next Page"
	TCODE	tExitLog, 	"Exit Logbook"

; Reset menu
	TCODE	tReboot,	"Reboot"			;Reboot
	TCODE	tResetMenu2,"Are you sure?"		;Are you sure?
	TCODE	tAbort,     "Abort"             ;Abort
	TCODE	tResetSettings, "Reset all" 	;Reset all
	TCODE	tResetDeco,	"Reset Deco"		;Reset Deco
    TCODE	tResetBattery,	"Reset Battery"		;Reset Battery
    TCODE	tResetLogbook,	"Reset Logbook"		;Reset Logbook

; Set Time Menu/Set Date Menu
	TCODE	tSetHours,	"Set Hours"			; Set Hours
	TCODE	tSetMinutes,"Set Minutes"		; Set Minutes
	TCODE	tSetSeconds,"Clear Seconds"		; Clear seconds
	TCODE	tSetDay,	"Set Day"			; Set Day
	TCODE	tSetMonth,	"Set Month"			; Set Month
	TCODE	tSetYear,	"Set Year"			; Set Year

; After update texts
	TCODE	tNewFirmware1, "Update successful!"	; Update successful!
	TCODE	tNewFirmware2, "New Firmware: "		; New Firmware: 

; Logbook Offset Menu
	TCODE	tLogOffset,		"Logbook offset"	; Logbook offset
	TCODE	tLogOffsetp1,	"+1"				; +1
	TCODE	tLogOffsetp10,	"+10"				; +10
	TCODE	tLogOffsetm1,	"-1"				; -1
	TCODE	tLogOffsetm10,	"-10"				; -10

; Sampling rate
    TCODE   tSamplingrate,  "Sampling rate:"    ; Sampling rate:
    TCODE   tSampling2s,    "2s"                ; 2s
    TCODE   tSampling10s,   "10s"               ; 10s

; Compass directions
    TCODE   tN,             "N"                 ; N(orth),338°-22°
    TCODE   tNE,            "NE"                ; North-East,23°-67°
    TCODE   tE,             "E"                 ; E(east),68°-112°
    TCODE   tSE,            "SE"                ; South-East,113°-157°
    TCODE   tS,             "S"                 ; S(outh),158°-202°
    TCODE   tSW,            "SW"                ; South-West,203°-247°
    TCODE   tW,             "W"                 ; W(West),248°-292°
    TCODE   tNW,            "NW"                ; North-West,293°-337°

; Color Scheme menu
    TCODE   tColorScheme,   "Colour scheme"     ; Colour scheme
    TCODE   tColorSetDive,  "Divemode:"         ; Divemode:
    TCODE   tColorSetName0, "Standard"          ; Standard
    TCODE   tColorSetName1, "Red"               ; Red
    TCODE   tColorSetName2, "Green"             ; Green
    TCODE   tColorSetName3, "Blue"              ; Blue
