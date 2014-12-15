;=============================================================================   >> OCTC3 V1.13b > ClaudeDive update proposed. 
;
;    File text_french.asm   
;
;    French texts translation file. 
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================

; Basic texts
	TCODE	tNo,    	"Non"        	; No
	TCODE	tYes,   	"Oui"         	; Yes = No + 1

; Surface-mode texts
    TCODE   tBeta,      "béta"          ; beta
	TCODE	tMenu,		"<Menu" 		; <Menu
    TCODE   tView,      "Vues>"         ; View>
    TCODE   tHeading,   "Cap:"      ; Heading:

; Divemode Menu
    TCODE   tDivemenu_Gaslist,  "Liste Gaz"      ; Gas List
    TCODE   tDivemenu_ResetAvr, "RaZChrono"    ; Reset Avr.
    TCODE   tDivemenu_Setpoint, "Setpoint"      ; Setpoint
    TCODE   tDivemenu_UseSensor,"Cellules"    ; Use Sensor
    TCODE   tDivemenu_ToggleGF, "Bascul.GF"     ; Toggle GF

; Main menu
    TCODE   tNext,      "<Suivant"        ; <Next
    TCODE   tEnter,     "Entrer>"        ; Enter>

    TCODE   tMainMenu,  "Menu Principal"; Main Menu
    TCODE   tLogbook,   "Carnet de Plongée" ; Logbook
    TCODE   tGasSetup,  "Paramètres Gaz OC"  ; OC Gas Setup
    TCODE   tSetTime,   "Réglage Heure"	; Set Time
	TCODE	tSetDate,	"Réglage Date"	; Set Date
	TCODE	tSetTimeDate,"Date & Heure" ; Set Time & Date
    TCODE   tDispSets,  "Préfér. Réglages"	; Display Settings
    TCODE   tExit,      "Retour"        ; Exit
	TCODE	tResetMenu, "Menu RaZ"      ; Reset Menu
	TCODE	tDiveModeMenu,"Menu Déco"	; Deco Mode
	TCODE	tInfoMenu,	"Informations"	; Information
    TCODE   tCCRSetup,  "Paramètres CCR"     ; CCR Setup
    TCODE   tDiluentSetup,"Liste Diluant"   ; Diluent Setup
    TCODE   tFixedSetpoints,"Setpoints fixes" ; Fixed Setpoints
    TCODE   tCCRSensor, "Cellules CCR"    ; CCR Sensor
    TCODE   tCalibrateMenu, "Calibration"   ; Calibration
    TCODE   tCalibrationGas, "Cal. Gas O2:"  ; Cal. Gas O2:
    TCODE   tCalibrate, "Calibrate"     ; Calibrate

; Gas menu
    TCODE   tGaslist,   "Liste Gaz OC"		  ; OC Gas List
    TCODE   tGaslistCC, "Liste Gaz CC"         ; CC Diluents
    TCODE   tGasEdit,   "Préférence Gaz"     ; Edit Gas
    TCODE   tType,      "Type: "
    TCODE   tGasDisabled,"Désactivé"  ; Disabled
    TCODE   tGasFirst,   "Premier"     ; First
    TCODE   tGasTravel,  "Travel"    ; Travel
    TCODE   tGasDeco,    "Déco"      ; Deco
    TCODE   tDilDisabled,"Désactivé"  ; Disabled
    TCODE   tDilFirst,   "Premier"     ; First
    TCODE   tDilNorm,   "Normal"     ; Normal
    TCODE   tAir,       "Air  "         ; Enum: values must follows (5 chars)
    TCODE   tO2,        "O2   "         ; tAir + 5
    TCODE   tO2Plus,    "O2 +"
    TCODE   tO2Minus,   "O2 -"
    TCODE   tHePlus,    "He +"
    TCODE   tHeMinus,   "He -"
    TCODE   tMOD,       "MOD:"
    TCODE   tEAD,       "EAD:"
    TCODE   tGasDepth,  "Prof. Changement"         ; Change Depth
    TCODE   tDepthPlus, "Prof. +"
    TCODE   tDepthMinus,"Prof. -"
    TCODE   tDepthReset,"MOD par défaut:"
    TCODE   tSetup_mix, "Config. Gaz"         ; Setup mix
    TCODE   tCCRMode,   "Mode CCR:"             ; CCR Mode:
    TCODE   tCCRModeFixedSP,  "SP Fixe"        ; Fixed SP
    TCODE   tCCRModeSensor,   "Cellule"          ; Sensor
    TCODE   tSP,            "SP"                ; SP (SetPoint)
    TCODE   tSPPlus,    "pO2+"                  ; pO2+
    TCODE   tSensorFallback, "Fallback:"        ; Fallback:
    TCODE   tppO2,      "ppO2:"                 ; ppO2:

; New batteries menu
	TCODE   tNewBattTitle,      "Nouvelle Batterie?"
	TCODE	tNewBattOld,		"Conserver"
	TCODE	tNewBattNew36,		"Réinit. 3,6V"  ; New 3,6V
	TCODE	tNewBattNew15,		"Réinit. 1,5V"  ; New 1,5V

; Gaslist management
    TCODE   tGas,       "Gaz"           ; Gas
    TCODE   tGasErr,    "Err"           ; Err  (3 chars)

; Communication Menu
    TCODE   tUsbMode,  "USB"
	TCODE	tUsbTitle,		"Mode USB"
	TCODE	tUsbStarting,  	"Début..."
	TCODE	tUsbStartDone, 	"Ok."
	TCODE	tUsbServiceMode,"Mode Maintenance"
	TCODE	tUsbClosed,		"Port Fermé"
	TCODE	tUsbExit,		"Terminé"
	TCODE	tUsbDownloadMode,"Mode Téléchargement"
    TCODE   tUsbLlBld,      "Chargeur Bas-Niv."

; Dive Settings
    TCODE   tDvMode,    "Mode:"
    TCODE   tDvOC,      "OC"
    TCODE   tDvCC,      "CC"
    TCODE   tDvGauge,   "Profondi."  
    TCODE   tDvApnea,   "Apnée"
    TCODE   tDkMode,    "Modèle:"
    TCODE   tZHL16,     "ZH-L16"
    TCODE   tZHL16GF,   "ZH-L16+GF"
    TCODE   tPPO2Max,   " pO2 Max:"
    TCODE   tPPO2Min,   " pO2 Min:"
    TCODE   tLastDecostop, "Dern.Palier:"
    TCODE   tDecoparameters, "Paramètres Déco"
    TCODE   tGF_low,    "GF Bas:"
    TCODE   tGF_high,   "GF Haut:"
    TCODE   tSaturationMult,    "Saturation:"
    TCODE   tDesaturationMult,  "Désaturation:"
    TCODE   tFTTSMenu,  "Future DTR:"            ; Future TTS
    TCODE   taGFMenu,   "GF Alternatif"         ; Alternative GF
    TCODE   taGF_low,   "aGF Bas:"               ; aGF Low
    TCODE   taGF_high,  "aGF Haut:"              ; aGF High
    TCODE   taGF_enable,"aGF Possible:"        ; aGF Selectable
    TCODE   tDiveaGF_active,"aGF!"          ; aGF!
    TCODE   tppO2settings,"Menu ppO2"        ; ppO2 Settings (max. 18 chars)
    TCODE   tsafetystopmenu,"Palier sécurité:"   ; Safety Stop: (max. 16 chars)

; Display Settings
    TCODE   tBright,    "Luminosité:"
    TCODE   tEco,       "Eco"
    TCODE   tMedium,    "Moyenne"
    TCODE   tHigh,      "Haute"
    TCODE   tDvSalinity,"Salinité:"             ; Salinity
    TCODE   tFlip,      "Pivoter l'écran:"      ; Rotate Screen

; Setup Menu
    TCODE   tSystSets,  "Réglages"
    TCODE   tLanguage,  "Langue:"
    TCODE   tEnglish,   "Anglais"
    TCODE   tGerman,    "Allemand"
    TCODE   tFrench,    "Français"
    TCODE   tItalian,   "Italien"
    TCODE   tCompassMenu,"Calibration Compas"  ; Compass Calibration
    TCODE   tCompassGain,"Sensibilité:"        ; Compass Gain:

    TCODE   tUnits,     "Unités:"
    TCODE   tMetric,     " m/°C"            ; Enum menu
    TCODE   tImperial,   "ft/°F"

    TCODE   tDefName,   "HW OSTC"
	TCODE	tbar,		"bar"				; bar

; Units for all menu
    TCODE   tMeters,    "m"
	TCODE	tFeets,		"ft"
	TCODE	tFeets1,	"f"
    TCODE   tMinutes,   "'"
    TCODE   tPercent,   "%"

; Date
	TCODE	tDateFormat, "Date:"
	TCODE	tDateformat, "MMJJAA"
	TCODE	tDateformat1,"JJMMAA"
	TCODE	tDateformat2,"AAMMJJ"

; Simulator menu
    TCODE   tInter, 	"Start Simulateur"  ; Start Simulator
    TCODE   tPlan,  	"Simulateur"        ; Simulator

; Decoplanner submenu
    TCODE   tBtTm,  	"Temps Fond:"       ; Bot. Time: (10 chars)
    TCODE   tBtTm_short,"Temps:"            ; Bot. Time: (max. 6 chars)
    TCODE   tMxDep, 	"Prof. Max:"        ; Max Depth: (10 chars)
    TCODE   tIntvl, 	"Intervalle:"       ; Interval :  (10 chars)
    TCODE   tDeco,  	"Calcul Déco"       ; Calculate Deco
    TCODE   tDivePlan,  "Runtime:"          ; Dive Plan:
    TCODE   tNoDeco,    "No Déco"           ; No Deco
    TCODE   tMore,      "Suite..."          ; More...
    TCODE   tCNS,       "CNS"               ; CNS

; Information menu
    TCODE   tFirmware,  "Logiciel: "        ; Firmware: (space)
    TCODE   tSerial,    "N. Série: "        ; Serial  : (space)
	TCODE	tTotalDives,"Plongées tot.: "   ; Total Dives: (space)
    TCODE   tBatteryV,  "Batterie: "        ; Battery:

; Divemode screen
    TCODE   tNDL,       "NDL"
    TCODE   tTTS,       "DTR"
	TCODE	tVelMetric,	 "m/min"
	TCODE	tVelImperial,"ft/m "
	TCODE	tGasSelect,  "Activer Gaz"      ; Select Gas
	TCODE	tSelectAir,	 "Air "             ; Air
	TCODE	tSelectO2,	 "O2  "             ; O2
    TCODE   tSelectNx,   "Nx"               ; Nx
    TCODE   tSelectTx,   "Tx"               ; Tx
    TCODE   tDepth,      "Prof."            ; Depth
    TCODE   tMaxDepth,   "Prof. Max."       ; Max. Depth
    TCODE   tDivetime,   "Durée T."         ; Divetime
    TCODE   tDiveHudMask1,   "cell. 1"
    TCODE   tDiveHudMask2,   "cell. 2"
    TCODE   tDiveHudMask3,   "cell. 3"
    TCODE   tDiveTotalAvr,  "Prof. Moy."
    TCODE   tDiveStopwatch, "Chrono"
    TCODE   tDiveStopAvr,   "P.Moy.Chrono"
    TCODE   tApnoeTotal,    "Totale"        ; Total (Six chars, right alligned)
    TCODE   tApnoeMax,      "Dern.Plongée"  ; Last Decend
    TCODE   tApnoeSurface,  "Tps.Surface"   ; Surface Time
    TCODE   tDiveDecoplan,  "Runtime"       ; Decoplan
    TCODE   tDiveClock,     "Heure"         ; Clock
    TCODE   tDiveEAD_END,   "EAD/END"       ; EAD/END
    TCODE   tDiveTissues,   "Tissus"        ; Tissues
    TCODE   tEND,           "END:"          ; END:
    TCODE   tHe,            "He"            ; He
    TCODE   tN2,            "N2"            ; N2
    TCODE   tDiveBailout,   "Bailout"       ; Bailout
    TCODE   tGFactors,      "Valeurs GF"    ; GF Values
    TCODE   taGFactors,     "Valeurs aGF"   ; aGF Values
    TCODE   tGFInfo,        "   GF Info"    ; GF Info
    TCODE   tCeiling,       "Ceiling"       ; Ceiling
    TCODE   tDiveSafetyStop," Stop"         ; Stop (Five chars, right alligned)
    TCODE   tDiveFallback,  "Fallback!"     ; Fallback! (max. nine chars)

; Divemode menu
    TCODE   tDivePreMenu,   "Menu?"         ; Menu?

; Simulator menu
	TCODE	tQuitSim,	 "Fin?"            ; Quit Simulation?
	TCODE	tDescent1m,	 "Sim:-1m"          ; Descent 1m
	TCODE	tAscent1m,	 "Sim:+1m"          ; Ascent 1m
    TCODE	tplus5min,	 "Sim:+5'"          ; +5 mins

; Logbook
	TCODE	tCNS2,		"SNC:"
	TCODE	tAVR,		"Moy:"
	TCODE	tGF,		"GF:"
	TCODE	tSAT,		"Sat:"					; Sat:

; Logbook units
	TCODE	tLogTunitC,	"°C"	
	TCODE	tLogTunitF,	"°F"
	TCODE	tKGL,		"kg/l"
	TCODE	tMBAR,		"mbar"

; Logbook menu
	TCODE	tNextLog, 	"Page Suivante"
	TCODE	tExitLog, 	"Sortir Carnet"
	
; Reset menu
	TCODE	tReboot,	"Redémarrage"            ;Reboot
	TCODE	tResetMenu2,"Confirmer?"       ;Are You Sure?
	TCODE	tAbort,     "Quitter"         ;Abort
	TCODE	tResetSettings, "RaZ Complète"  ;Reset All
	TCODE	tResetDeco,	"RaZ Déco"          ;Reset Deco
    TCODE	tResetBattery,	"RaZ Batterie"		;Reset Battery
    TCODE	tResetLogbook,	"RaZ Carnet"		;Reset Logbook

; Set Time Menu/Set Date Menu
	TCODE	tSetHours,	"Ajust. Heures"     ; Set Hours
	TCODE	tSetMinutes,"Ajust. Minutes"    ; Set Minutes
	TCODE	tSetSeconds,"RaZ Secondes"      ; Clear Seconds
	TCODE	tSetDay,	"Ajust. Jours"      ; Set Day
	TCODE	tSetMonth,	"Ajust. Mois"       ; Set Month
	TCODE	tSetYear,	"Ajust. Année"      ; Set Year

; After update texts
	TCODE	tNewFirmware1, "Mise à Jour OK!"	; Update successful!
	TCODE	tNewFirmware2, "Nouvelle Version: "		; New Firmware: 

; Logbook Offset Menu
	TCODE	tLogOffset,		"Num. 1er plongée"  ; Logbook offset
	TCODE	tLogOffsetp1,	"+1"				; +1
	TCODE	tLogOffsetp10,	"+10"				; +10
	TCODE	tLogOffsetm1,	"-1"				; -1
	TCODE	tLogOffsetm10,	"-10"				; -10

; Sampling rate
    TCODE   tSamplingrate,  "Fréq. Mesures:"    ; Sampling rate:
    TCODE   tSampling2s,    "2s"                ; 2s
    TCODE   tSampling10s,   "10s"               ; 10s

; Compass directions
    TCODE   tN,             "N"                 ; N(orth),338°-22°
    TCODE   tNE,            "NE"                ; North-East,23°-67°
    TCODE   tE,             "E"                 ; E(east),68°-112°
    TCODE   tSE,            "SE"                ; South-East,113°-157°
    TCODE   tS,             "S"                 ; S(outh),157°-202°
    TCODE   tSW,            "SO"                ; South-West,203°-247°
    TCODE   tW,             "O"                 ; W(West),248°-292°
    TCODE   tNW,            "NO"                ; North-West,293°-337°

; Color Scheme menu
    TCODE   tColorScheme,   "Jeu de Couleurs"   ; Colour scheme
    TCODE   tColorSetDive,  "Mode Plongée:"     ; Divemode:
    TCODE   tColorSetName0, "Blanc"             ; White
    TCODE   tColorSetName1, "Rouge"             ; Red
    TCODE   tColorSetName2, "Vert"              ; Green
    TCODE   tColorSetName3, "Bleu"              ; Blue



