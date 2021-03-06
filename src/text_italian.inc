;=============================================================================
;
;    File text_italian.asm
;
;    Italian texts translation file.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================

; Basic texts
	TCODE	tNo,        "No"            ; No
	TCODE	tYes,       "Si"            ; Yes = No + 1

; Surface-mode texts
    TCODE   tBeta,      "beta"         	; beta
	TCODE	tMenu,		"<Menu" 		; <Menu
    TCODE   tView,      "Vedi>"         ; View>
    TCODE   tHeading,   "Direzione:"    ; Heading:
    TCODE   tLastDive,	"Last Dive"	; Last Dive (Max 10 chars)

; Divemode Menu
    TCODE   tDivemenu_Gaslist,  "Lista Gas"       ; Gaslist
    TCODE   tDivemenu_ResetAvg, "Azzera Media"  ; Reset Avg.
    TCODE   tDivemenu_Setpoint, "Setpoint"      ; Setpoint
    TCODE   tDivemenu_UseSensor,"Usa Sensori"    ; Use Sensor
    TCODE   tDivemenu_ToggleGF, "Alterna GF"     ; Toggle GF
    TCODE   tDivemenu_Marker,   "Set Marker"    ; Set Marker
    TCODE   tDivemenu_LostGas,  "Gas perduto"    ; Lost Gas

; Main menu
    TCODE   tNext,      "<Prossimo"         ; <Next
    TCODE   tEnter,     "Enter>"        ; Enter>

    TCODE   tMainMenu,  "Menu Principale"     ; MainMenu
    TCODE   tLogbook,   "Logbook"       ; Logbook
    TCODE   tGasSetup,  "Imposta Gas OC"  ; OC Gas Setup
    TCODE   tSetTime,   "Imposta Ora"		; Set Time
	TCODE	tSetDate,	"Imposta Data"		; Set Date
	TCODE	tSetTimeDate,"Imp. Ora e Data"; Set Time & Date
    TCODE   tDispSets,  "Visualizzazione" ; Display Settings
    TCODE   tExit,      "Uscita"          ; Exit
	TCODE	tResetMenu, "Menu Reset"	; Reset Menu
	TCODE	tDiveModeMenu,"Modalita' Deco"	; Deco Mode
	TCODE	tInfoMenu,	"Informazioni"	; Information
    TCODE   tCCRSetup,  "Imposta CCR"     ; CCR Setup
    TCODE   tDiluentSetup,"Imposta Diluente"   ; Diluent Setup
    TCODE   tFixedSetpoints,"Setpoints Fissi" ; Fixed Setpoints
    TCODE   tCCRSensor, "Sensori CCR"    ; CCR Sensor
    TCODE   tCalibrateMenu, "Calibration"   ; Calibration
    TCODE   tCalibrationGas, "Cal. Gas O2:"  ; Cal. Gas O2:
    TCODE   tCalibrate, "Calibrate"     ; Calibrate

; Gas menu
    TCODE   tGaslist,   "Lista Gas OC"
    TCODE   tGaslistCC, "Lista Gas CC"   ; CC Diluents
    TCODE   tGasEdit,   "Modifica Gas"
    TCODE   tType,      "Tipo: "
    TCODE   tGasDisabled,"Disabilitato"  ; Disabled
    TCODE   tGasFirst,   "Primo"     ; First
    TCODE   tGasTravel,  "Viaggio"    ; Travel
    TCODE   tGasDeco,    "Deco"      ; Deco
    TCODE   tDilDisabled,"Disabilitato"  ; Disabled
    TCODE   tDilFirst,   "Primo"     ; First
    TCODE   tDilNorm,   "Normale"     ; Normal
    TCODE   tAir,       "Aria  "         ; Enum: values must follows (5 chars)
    TCODE   tO2,        "O2   "         ; tAir + 5
    TCODE   tO2Plus,    "O2 +"
    TCODE   tO2Minus,   "O2 -"
    TCODE   tHePlus,    "He +"
    TCODE   tHeMinus,   "He -"
    TCODE   tMOD,       "MOD:"
    TCODE   tEAD,       "EAD:"
    TCODE   tGasDepth,  "Cambia profondita'"
    TCODE   tDepthPlus, "Profondita' +"
    TCODE   tDepthMinus,"Profondita' -"
    TCODE   tDepthReset,"Reset a MOD:"
    TCODE   tSetup_mix, "Imposta Miscela"
    TCODE   tCCRMode,   "Modo CCR:"             ; CCR Mode:
    TCODE   tCCRModeFixedSP,  "SP Fissi"        ; Fixed SP
    TCODE   tCCRModeSensor,   "Sensori"          ; Sensor
    TCODE   tCCRModeAutoSP,   "Auto SP"         ; Auto SP
    TCODE   tSP,            "SP"                ; SP (SetPoint)
    TCODE   tSPPlus,    "ppO2+"                  ; pO2+
    TCODE   tSensorFallback, "Fallback:"        ; Fallback:
    TCODE   tppO2,      "ppO2:"                 ; ppO2:

; New battery menu
	TCODE   tNewBattTitle,      "Nuova Batteria?"
	TCODE	tNewBattOld,		"Mantieni vecchio"
	TCODE	tNewBattNew36,		"Nuova 3,6V Saft"
	TCODE	tNewBattNew15,		"Nuova 1,5V AA"
	TCODE	tNewBattAccu,		"3,6V AA ricaricabile"
 	TCODE	tNew18650,		"Internal 18650"
	TCODE	tNew16650,		"Internal 16650"

; Gaslist management
    TCODE   tGas,       "Gas"           ; Gas
    TCODE   tGasErr,    "Err"           ; Err  (3 chars)

; Communication Menu
	TCODE	tUsbTitle,	"Modalita' USB"
    TCODE   tBleTitle,  "Modalita' Bluetooth"
	TCODE	tUsbStarting,  "Avvio..."
	TCODE	tUsbStartDone, "Fatto."
	TCODE	tUsbServiceMode, "Modo Servizio abil"
	TCODE	tUsbClosed,		"Porta chiusa"
	TCODE	tUsbExit,		"Uscita"
	TCODE	tUsbDownloadMode, "Modo Download abil"
    TCODE   tUsbLlBld,      "Livello-basso Bootloader"

; Dive Settings
    TCODE   tDvMode,    "Modo Dive:"
    TCODE   tDvOC,      "OC"
    TCODE   tDvCC,      "CC"
    TCODE   tDvGauge,   "Gauge"
    TCODE   tDvApnea,   "Apnea"
    TCODE   tDvPSCR,	"PSCR"
    TCODE   tDkMode,    "Algorit.:"
    TCODE   tZHL16,     "ZH-L16"
    TCODE   tZHL16GF,   "ZH-L16+GF"
    TCODE   tPPO2Max,   "ppO2 Max:"
    TCODE   tPPO2DECO,	"ppO2 Deco:"
    TCODE   tPPO2MIN,   "ppO2 Min:"
    TCODE   tLastDecostop, "Ultima Deco:"
    TCODE   tDecoparameters, "Parametri Deco"
    TCODE   tGF_low,    "GF basso:"
    TCODE   tGF_high,   "GF alto:"
    TCODE   tSaturationMult,    "Saturazione:"
    TCODE   tDesaturationMult,  "Desaturazione:"
    TCODE   tFTTSMenu,  "TTS Futuro:"            ; Future TTS
    TCODE   taGFMenu,   "GF Alternativo"         ; Alternative GF
    TCODE   taGF_low,   "aGF basso:"               ; aGF low
    TCODE   taGF_high,  "aGF alto:"              ; aGF high
    TCODE   taGF_enable,"aGF Selezionabile:"        ; aGF Selectable
    TCODE   tDiveaGF_active,"aGF!"          ; aGF!
    TCODE   tppO2settings,"Menu ppO2"        ; ppO2 Settings (max. 18 chars)
    TCODE   tsafetystopmenu,"Usa tappa sicur:"       ; Safety Stop: (max. 16 chars)

; Display Settings
    TCODE   tBright,    "Luminosita':"
    TCODE   tEco,       "Eco"
    TCODE   tMedium,    "Media"
    TCODE   tHigh,      "Alta"
    TCODE   tDvSalinity,"Salinita':"            ; Salinity
    TCODE   tFlip,      "Ruotare schermo:"      ; Rotate Screen
    TCODE   tMODwarning,"MOD warning:"          ; MOD warning
    TCODE   tShowppO2,  "Always show ppO2:"     ; Always show ppO2:
    TCODE   tTimeoutDive,"Dive Timeout:"	; Dive Timeout

; VSI display Settings
    TCODE   tVSItext2,  "Variable speed:"     ; Variable speed:
    TCODE   tVSIgraph,  "Speed graph:"        ; Speed graph:

; Setup Menu
    TCODE   tSystSets,  "Impostazioni"
    TCODE   tLanguage,  "Lingua:"
    TCODE   tEnglish,   "Inglese"
    TCODE   tGerman,    "Tedesco"
    TCODE   tFrench,    "Francese"
    TCODE   tItalian,   "Italiano"
    TCODE   tCompassMenu,"Calibra Bussola"  ; Compass calibration
    TCODE   tCompassGain,"Suscettibilita:"  ; Compass gain:

    TCODE   tUnits,     "Unita':"
    TCODE   tMetric,     " m/�C"            ; Enum menu
    TCODE   tImperial,   "ft/�F"

    TCODE   tDefName,   "HW OSTC"
	TCODE	tbar,		"bar"				; bar

    TCODE   tButtonleft,"Bottone sinistra:"      ; Left button
    TCODE   tButtonright,"Bottone destra:"    ; Right button

; Units for all menu
    TCODE   tMeters,    "m"
	TCODE	tFeets,		"ft"
	TCODE	tFeets1,	"f"
    TCODE   tMinutes,   "'"
    TCODE   tPercent,   "%"
    TCODE   tLitersMinute, "l/min"

; Date
	TCODE	tDateFormat, "Data:"
	TCODE	tDateformat, "MMGGAA"
	TCODE	tDateformat1,"GGMMAA"
	TCODE	tDateformat2,"AAMMGG"

; Simulator menu
    TCODE   tInter, 	"Start Simulatore"     ; Start Simulator
    TCODE   tPlan,  	"Simulatore"           ; Simulator

; Decoplanner submenu
    TCODE   tBtTm,  	"Tempo Fondo :"        ; Bot. Time: (10 chars)
    TCODE   tBtTm_short,"Tempo:"               ; Bot. Time: (max. 6 chars)
    TCODE   tMxDep, 	"Profond. Max:"        ; Max Depth: (10 chars)
    TCODE   tIntvl, 	"Int.Superf. :"        ; Interval : (10 chars)
    TCODE   tDeco,  	"Calcola Deco"         ; Calculate Deco
    TCODE   tDivePlan,  "Pianificat:"          ; Dive Plan:
    TCODE   tNoDeco,    "No Deco"              ; No Deco
    TCODE   tMore,      "Dettagli.."           ; More...
    TCODE   tGasUsage,  "Gas Usage"            ; Gas Usage
    TCODE   tSetBotUse, "Bottom Gas: "      ; Bottom Gas: (space)
    TCODE   tSetDecoUse,"Deco Gas: "        ; Deco Gas: (space)


; Information menu
    TCODE   tFirmware,  "Firmware: "        ; Firmware: (space)
    TCODE   tSerial,    "Seriale : "        ; Serial  : (space)
	TCODE	tTotalDives,"Totale Immersioni:"		; Total Dives:
    TCODE   tBatteryV,  "Batteria: "        ; Battery:
    TCODE   tUptime,	"Uptime: "	    ; Uptime: 

; Divemode screen
    TCODE   tNDL,       "NDL"
    TCODE   tTTS,   	"TTS"
	TCODE	tVelMetric,	 "m/min"
	TCODE	tVelImperial,"ft/m "
	TCODE	tGasSelect,  "Selez. Gas"       ; Select Gas
	TCODE	tSelectAir,	 "Aria "     		; Air
	TCODE	tSelectO2,	 "O2  "           	; O2
    TCODE   tSelectNx,   "Nx"               ; Nx
    TCODE   tSelectTx,   "Tx"               ; Tx
    TCODE   tDepth,      "Prof."            ; Depth
    TCODE   tMaxDepth,   "Prof. Max"        ; Max. Depth - max 10chars!
    TCODE   tDivetime,   "Divetime"         ; Divetime
    TCODE   tDiveHudMask1,  "Sensor 1"
    TCODE   tDiveHudMask2,  "Sensor 2"
    TCODE   tDiveHudMask3,  "Sensor 3"
    TCODE   tDiveTotalAvg,  "Media"
    TCODE   tDiveStopwatch, "Cronometro"
    TCODE   tDiveStopAvg,   "Media Inter"   ; 11chars max
    TCODE   tApnoeTotal,    "Totale"        ; Total (Six chars, right alligned)
    TCODE   tApnoeMax,      "Ultima Discesa"  ; Last descend
    TCODE   tApnoeSurface,  "Tempo Superficie"  ; Surface Time
    TCODE   tDiveDecoplan,  "Piano Deco"      ; Decoplan
    TCODE   tDiveClock,     "Orologio"         ; Clock
    TCODE   tDiveEAD_END,   "EAD/END"       ; EAD/END
    TCODE   tDiveTissues,   "Tessuti"       ; Tissues
    TCODE   tEND,           "END:"          ; END:
    TCODE   tHe,            "He"            ; He
    TCODE   tN2,            "N2"            ; N2
    TCODE   tDiveBailout,   "Bailout"       ; Bailout
    TCODE   tGFactors,      "Valori GF"     ; GF Values
    TCODE   taGFactors,     "Valori aGF"    ; aGF Values
    TCODE   tGFInfo,        "Info GF"       ; GF Info
    TCODE   tCeiling,       "Ceiling"       ; Ceiling
    TCODE   tDiveSafetyStop,"Stop"         ; Stop (Four chars, right alligned)
    TCODE   tDiveFallback,  "Fallback!"     ; Fallback! (max. nine chars)
    TCODE   tSensorCheck,   "Sensor Check"  ; Sensor Check

; Divemode menu
    TCODE   tDivePreMenu,   "Menu?"         ; Menu?
    TCODE   tSetHeading,    "Direz."        ; Bearing (Max. seven chars)

; Simulator menu
	TCODE	tQuitSim,	 "Uscire?"            ; Quit Simulation?

; Logbook
	TCODE	tCNS2,		"CNS:"
	TCODE	tAVG,		"Avg:"
	TCODE	tGF,		"GF:"
	TCODE	tSAT,		"Sat:"					; Sat:

; Logbook units
	TCODE	tLogTunitC,	"�C"
	TCODE	tLogTunitF,	"�F"
	TCODE	tKGL,		"kg/l"
	TCODE	tMBAR,		"mbar"

; Logbook menu
	TCODE	tNextLog, 	"Pagina Seguente"

; Reset menu
	TCODE	tReboot,	"Riavvio"			;Reboot
	TCODE	tResetMenu2,"Sei sicuro?"		;Are you sure?
	TCODE	tAbort,     "Annulla"             ;Abort
	TCODE	tResetSettings, "Azzera Tutto" 	;Reset all
	TCODE	tResetDeco,	"Azzera Deco"		;Reset Deco
    TCODE	tResetBattery,	"Azzera Batteria"		;Reset Battery
    TCODE	tResetLogbook,	"Azzera Logbook"		;Reset Logbook

; Set Time Menu/Set Date Menu
	TCODE	tSetHours,	"Imposta Ora"			; Set Hours
	TCODE	tSetMinutes,"Imposta Minuti"		; Set Minutes
	TCODE	tSetSeconds,"Azzera Secondi"		; Clear seconds
	TCODE	tSetDay,	"Imp Giorno"			; Set Day
	TCODE	tSetMonth,	"Imp Mese"			; Set Month
	TCODE	tSetYear,	"Imp Anno"			; Set Year

; Logbook Offset Menu
	TCODE	tLogOffset, "Offset Logbook"	; Logbook offset
	TCODE	tLogOffsetp1,	"+1"				; +1
	TCODE	tLogOffsetp10,	"+10"				; +10
	TCODE	tLogOffsetm1,	"-1"				; -1
	TCODE	tLogOffsetm10,	"-10"				; -10

; Sampling rate
    TCODE   tSamplingrate,  "Campionamento:"    ; Sampling rate:
    TCODE   tSampling2s,    "2s"                ; 2s
    TCODE   tSampling10s,   "10s"               ; 10s

; Compass directions
    TCODE   tN,             "N "                ; N(orth),338�-22�
    TCODE   tNE,            "NE"                ; North-East,23�-67�
    TCODE   tE,             "E "                ; E(east),68�-112�
    TCODE   tSE,            "SE"                ; South-East,113�-157�
    TCODE   tS,             "S "                ; S(outh),157�-202�
    TCODE   tSW,            "SO"                ; South-West,203�-247�
    TCODE   tW,             "O "                ; W(West),248�-292�
    TCODE   tNW,            "NO"                ; North-West,293�-337�

; Color Scheme menu
    TCODE   tColorScheme,   "Schema di colori"  ; Colour scheme
    TCODE   tColorSetDive,  "Immersione:"       ; Divemode:
    TCODE   tColorSetName0, "Standard"          ; Standard
    TCODE   tColorSetName1, "Rosso"             ; Red
    TCODE   tColorSetName2, "Verde"             ; Green
    TCODE   tColorSetName3, "Blu"               ; Blue

    ; PSCR Menu and Settings
    TCODE   tPSCRMenu,	    "PSCR Menu"		; PSCR Menu
    TCODE   tPSCR_O2_drop,  "O2 drop:"		; O2 drop
    TCODE   tPSCR_lungratio,"Lung ratio:"	; lung ratio
