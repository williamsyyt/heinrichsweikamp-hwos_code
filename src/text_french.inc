;=============================================================================
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
	TCODE	tMenu,		"<Menu"			; <Menu
    TCODE   tView,      "vues>"         ; View>
    TCODE   tHeading,   "Heading:"      ; Heading:

; Divemode Menu
    TCODE   tDivemenu_Gaslist,  "liste gaz"      ; Gaslist
    TCODE   tDivemenu_ResetAvr, "Resetchrono"    ; Reset Avr.
    TCODE   tDivemenu_Setpoint, "Setpoint"      ; Setpoint
    TCODE   tDivemenu_UseSensor,"cellules"    ; Use Sensor
    TCODE   tDivemenu_ToggleGF, "bascul.GF"     ; Toggle GF

; Main menu
    TCODE   tNext,      "<Suiv."        ; <Next
    TCODE   tEnter,     "Entrer>"        ; Enter>

    TCODE   tMainMenu,  "Menu Principal"; MainMenu
    TCODE   tLogbook,   "Carnet de Plongée" ; Logbook
    TCODE   tGasSetup,  "Configuration Gaz OC"  ; OC Gas Setup
    TCODE   tSimulator, "Simulateur"    ; Simulator
    TCODE   tSetTime,   "Réglage Heure"	; Set Time
	TCODE	tSetDate,	"Réglage Date"	; Set Date
	TCODE	tSetTimeDate,"Date & Heure" ; Set Time & Date
    TCODE   tDispSets,  "Param. Affichage"	; Display Settings
    TCODE   tExit,      "Retour"        ; Exit
	TCODE	tResetMenu, "Menu RaZ"      ; Reset Menu
	TCODE	tDiveModeMenu,"Menu Déco"	; Deco Mode
	TCODE	tInfoMenu,	"Information"	; Information
    TCODE   tCCRSetup,  "Paramètres CCR"     ; CCR Setup
    TCODE   tDiluentSetup,"Config. Diluant"   ; Diluent Setup
    TCODE   tFixedSetpoints,"Setpoints fixes" ; Fixed Setpoints
    TCODE   tCCRSensor, "Cellules CCR"    ; CCR Sensor

; Gas menu
    TCODE   tGaslist,   "Liste gaz OC"
    TCODE   tGasEdit,   "Préf. Gaz"     ; Edit Gas
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
    TCODE   tMOD,       "PMU:"
    TCODE   tEAD,       "PEA:"
    TCODE   tGasDepth,  "Prof. changmt"         ; Change Depth
    TCODE   tDepthPlus, "Prof. +"
    TCODE   tDepthMinus,"Prof. -"
    TCODE   tDepthReset,"PMU par défaut:"
    TCODE   tSetup_mix, "Confi. Mix"            ; Setup mix
    TCODE   tCCRMode,   "Mode CCR:"             ; CCR Mode:
    TCODE   tCCRModeFixedSP,  "SP Fixe"        ; Fixed SP
    TCODE   tCCRModeSensor,   "Cellule"          ; Sensor
    TCODE   tSP,            "SP"                ; SP (SetPoint)
    TCODE   tSPPlus,    "pO2+"                  ; pO2+

; New batteries menu
	TCODE   tNewBattTitle,      "Nouvelle Batterie?"
	TCODE	tNewBattOld,		"Conserver"
	TCODE	tNewBattNew36,		"Réinit. 3,6V"  ; New 3,6V
	TCODE	tNewBattNew15,		"Réinit. 1,5V"  ; New 1,5V

; Gaslist management
    TCODE   tGas,       "Gaz"           ; Gas
    TCODE   tNx,        "Nx "           ; Nx    (3 chars)
    TCODE   tTx,        "Tx "          	; Tx   (3 chars)
    TCODE   tGasErr,    "Err"           ; Err  (3 chars)

; Communication Menu
    TCODE   tUsbMode,  "USB"
	TCODE	tUsbTitle,		"Mode USB"
	TCODE	tUsbStarting,  	"Début..."
	TCODE	tUsbStartDone, 	"Ok.     "
	TCODE	tUsbServiceMode,"Mode maintenance"
	TCODE	tUsbClosed,		"Port fermé"
	TCODE	tUsbExit,		"Terminé"
	TCODE	tUsbDownloadMode,"Mode téléchargement"
    TCODE   tUsbLlBld,      "Chargeur bas-niv."

; Dive Settings
    TCODE   tDvMode,    "Mode:"
    TCODE   tDvOC,      "OC"
    TCODE   tDvCC,      "CC"
    TCODE   tDvGauge,   "Profondi."
    TCODE   tDvApnea,   "Apnee"
    TCODE   tDkMode,    "Modèle:"
    TCODE   tZHL16,     "ZH-L16"
    TCODE   tZHL16GF,   "ZH-L16+GF"
    TCODE   tPPO2Max,   "ppO2 Max:"
    TCODE   tPPO2Min,   "ppO2 Min:"
    TCODE   tLastDecostop, "Dern.palier:"
    TCODE   tDecoparameters, "Param. Déco"
    TCODE   tGF_low,    "GF bas:"
    TCODE   tGF_high,   "GF haut:"
    TCODE   tSaturationMult,    "Saturation:"
    TCODE   tDesaturationMult,  "Désaturation:"
    TCODE   tFTTSMenu,  "Future DTR:"            ; Future TTS
    TCODE   taGFMenu,   "GF Alternatif"         ; Alternative GF
    TCODE   taGF_low,   "aGF bas:"               ; aGF low
    TCODE   taGF_high,  "aGF haut:"              ; aGF high
    TCODE   taGF_enable,"aGF Possible:"        ; aGF Selectable
    TCODE   tDiveaGF_active,"aGF!"          ; aGF!

; Display Settings
    TCODE   tBright,    "Luminosité:"
    TCODE   tEco,       "Eco"
    TCODE   tMedium,    "Moyenne"
    TCODE   tHigh,      "Haute"
    TCODE   tDvSalinity,"Salinité:"             ; Salinity

; Setup Menu
    TCODE   tSystSets,  "Configuration"
    TCODE   tLanguage,  "Langue:"
    TCODE   tEnglish,   "Anglais"
    TCODE   tGerman,    "Allemand"
    TCODE   tFrench,    "Français"
    TCODE   tItalian,   "Italien"
    TCODE   tCompassMenu,"Calibration boussole"  ; Compass calibration
    TCODE   tCompassGain,"Sensibilité"          ; Compass gain
    TCODE   tcharx,     "x"

    TCODE   tUnits,     "Unités:"
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
	TCODE	tDateformat, "MMJJAA"
	TCODE	tDateformat1,"JJMMAA"
	TCODE	tDateformat2,"AAMMJJ"

; Simulator menu
    TCODE   tInter, 	"Simulateur"        ; Dive Simulator
    TCODE   tPlan,  	"Planification"     ; Deco Planner

; Decoplanner submenu
    TCODE   tBtTm,  	"Tps. fond:"        ; Bot. Time: (10 chars)
    TCODE   tMxDep, 	"Prof. max:"        ; Max Depth: (10 chars)
    TCODE   tIntvl, 	"Intervalle:"       ; Interval :  (10 chars)
    TCODE   tDeco,  	"Calcul..."         ; Calculate Deco
    TCODE   tDivePlan,  "Runtime:"          ; Dive Plan:
    TCODE   tNoDeco,    "SansDéco"          ; No Deco
    TCODE   tMore,      "Suite..."          ; More...
    TCODE   tCNS,       "CNS"               ; CNS

; Information menu
    TCODE   tFirmware,  "Logiciel: "        ; Firmware: (space)
    TCODE   tSerial,    "N. Série: "        ; Serial  : (space)
	TCODE	tTotalDives,"Plongées tot.: "   ; Total Dives: (space)

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
    TCODE   tDivetime,   "DuréeTot."         ; Divetime
    TCODE   tDiveHudMask1,   "cell. 1"
    TCODE   tDiveHudMask2,   "cell. 2"
    TCODE   tDiveHudMask3,   "cell. 3"
    TCODE   tDiveTotalAvr,  "Prof. Moy."
    TCODE   tDiveStopwatch, "Chrono"
    TCODE   tDiveStopAvr,   "P.MoyChrono"
    TCODE   tApnoeTotal,    "   Durée Total"; Total Divetime
    TCODE   tApnoeMax,      "Dern.plongée"  ; Last decend
    TCODE   tApnoeSurface,  "Tps.Surface"   ; Surface Time
    TCODE   tDiveDecoplan,  "Runtime"       ; Decoplan
    TCODE   tDiveClock,     "Heure"         ; Clock
    TCODE   tDiveEAD_END,   "PEA/END"       ; EAD/END
    TCODE   tDiveTissues,   "Tissus"        ; Tissues
    TCODE   tEND,           "END:"          ; END:
    TCODE   tHe,            "He"            ; He
    TCODE   tN2,            "N2"            ; N2
    TCODE   tDiveBailout,   "Bailout"       ; Bailout
    TCODE   tGFactors,      "Valeurs GF"    ; GF Values
    TCODE   taGFactors,     "Valeurs aGF"   ; aGF Values
    TCODE   tGFInfo,        "GF Info"       ; GF Info

; Divemode menu
    TCODE   tDivePreMenu,   "Menu?"         ; Menu?

; Simulator menu
	TCODE	tQuitApnea,	 "Fin?"            ; Quit Apnea mode?
	TCODE	tQuitSim,	 "Fin?"            ; Quit Simulation?
	TCODE	tDescent1m,	 "Sim:-1m"          ; Descent 1m
	TCODE	tAscent1m,	 "Sim:+1m"          ; Ascent 1m

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
	TCODE	tNextLog, 	"Page suiv."
	TCODE	tExitLog, 	"Sortir carnet"
	
; Reset menu
	TCODE	tReboot,	"Redemarrage"            ;Reboot
	TCODE	tResetMenu2,"Confirmer ?"       ;Are you sure?
	TCODE	tAbort,     "Quitter"         ;Abort
	TCODE	tResetSettings, "RaZ complète"  ;Reset all
	TCODE	tResetDeco,	"RaZ Deco"          ;Reset Deco
    TCODE	tResetBattery,	"RaZ Batterie"		;Reset Battery
    TCODE	tResetLogbook,	"RaZ Carnet"		;Reset Logbook

; Set Time Menu/Set Date Menu
	TCODE	tSetHours,	"Ajust. Heures"     ; Set Hours
	TCODE	tSetMinutes,"Ajust. Minutes"    ; Set Minutes
	TCODE	tSetSeconds,"RaZ Secondes"      ; Clear seconds
	TCODE	tSetDay,	"Ajust. Jours"      ; Set Day
	TCODE	tSetMonth,	"Ajust. Mois"       ; Set Month
	TCODE	tSetYear,	"Ajust. Année"      ; Set Year

; After update texts
	TCODE	tNewFirmware1, "Mise a jours ok !"	; Update successful!
	TCODE	tNewFirmware2, "Nouvelle version: "		; New Firmware: 

; Logbook Offset Menu
	TCODE	tLogOffset,		"Num. 1er plongée"  ; Logbook offset
	TCODE	tLogOffsetp1,	"+1"				; +1
	TCODE	tLogOffsetp10,	"+10"				; +10
	TCODE	tLogOffsetm1,	"-1"				; -1
	TCODE	tLogOffsetm10,	"-10"				; -10

; Sampling rate
    TCODE   tSamplingrate,  "Freq. Mesures:"    ; Sampling rate:
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
    TCODE   tColorScheme,   "Jeu de couleurs"   ; Colour scheme
    TCODE   tColorSetDive,  "Mode plongée:"     ; Divemode:
    TCODE   tColorSetName0, "Norme"             ; Standard
    TCODE   tColorSetName1, "Rouge"             ; Red
    TCODE   tColorSetName2, "Vert"              ; Green
    TCODE   tColorSetName3, "Bleu"              ; Blue



