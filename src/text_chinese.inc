;=============================================================================
;
;    File text_chinese.asm
;
;    Chinese texts reference file.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================

; Basic texts
	TCODE	tNo,        "否"            ; No
	TCODE	tYes,       "是"           ; Yes = No + 1

; Surface-mode texts
    TCODE   tBeta,      "测试版"         	; beta
    TCODE   tMenu,	"<菜单" 	; <Menu
    TCODE   tView,      "查看>"         ; View>
    TCODE   tHeading,   "航向:"      ; Heading:
    TCODE   tLastDive,	"上一潜"	; Last Dive (Max 10 chars)

; Divemode Menu
    TCODE   tDivemenu_Gaslist,  "气体列表"       ; Gaslist
    TCODE   tDivemenu_ResetAvg, "重置平均深度"    ; Reset Avg.
    TCODE   tDivemenu_Setpoint, "设置点"      ; Setpoint
    TCODE   tDivemenu_UseSensor,"使用传感器"    ; Use Sensor
    TCODE   tDivemenu_ToggleGF, "切换GF设置"     ; Toggle GF
    TCODE   tDivemenu_Marker,   "设置标记"    ; Set Marker
    TCODE   tDivemenu_LostGas,  "气体丢失"      ; Lost Gas

; Main menu
    TCODE   tNext,      "<下页"         ; <Next
    TCODE   tEnter,     "选择>"        ; Enter>

    TCODE   tMainMenu,  "主菜单"     ; MainMenu
    TCODE   tLogbook,   "日志"       ; Logbook
    TCODE   tGasSetup,  "OC气体设置"  ; OC Gas Setup
    TCODE   tSetTime,   "时间设置"		; Set Time
    TCODE	tSetDate,	"日期设置"		; Set Date
    TCODE	tSetTimeDate,"日期时间设置"; Set Time & Date
    TCODE   tDispSets,  "显示设置" ; Display Settings
    TCODE   tExit,      "退出"          ; Exit
	TCODE	tResetMenu, "菜单重置"	; Reset Menu
	TCODE	tDiveModeMenu,"减压模式"	; Deco Mode
	TCODE	tInfoMenu,	"信息"	; Information
    TCODE   tCCRSetup,  "CCR设置"     ; CCR Setup
    TCODE   tDiluentSetup,"稀释气体设置"   ; Diluent Setup
    TCODE   tFixedSetpoints,"选定设置点" ; Fixed Setpoints
    TCODE   tCCRSensor, "CCR传感器"    ; CCR Sensor
    TCODE   tCalibrateMenu, "校准"   ; Calibration
    TCODE   tCalibrationGas, "O2校准"  ; Cal. Gas O2:
    TCODE   tCalibrate, "校准"     ; Calibrate

; Gas menu
    TCODE   tGaslist,   "开放系统气体列表"
    TCODE   tGaslistCC, "密闭循环稀释气体"
    TCODE   tGasEdit,   "气体设置"
    TCODE   tType,      "输入: "
    TCODE   tGasDisabled,"关闭"  ; Disabled
    TCODE   tGasFirst,   "首选气体"     ; First
    TCODE   tGasTravel,  "阶段气体"    ; Travel
    TCODE   tGasDeco,    "减压气体"      ; Deco
    TCODE   tDilDisabled,"关闭"  ; Disabled
    TCODE   tDilFirst,   "首选稀释气体"     ; First
    TCODE   tDilNorm,   "标准稀释气体"     ; Normal
    TCODE   tAir,       "空气  "         ; Enum: values must follows (5 chars)
    TCODE   tO2,        "纯氧  "         ; tAir + 5
    TCODE   tO2Plus,    "O2 +"
    TCODE   tO2Minus,   "O2 -"
    TCODE   tHePlus,    "He +"
    TCODE   tHeMinus,   "He -"
    TCODE   tMOD,       "MOD:"
    TCODE   tEAD,       "EAD:"
    TCODE   tGasDepth,  "设置深度"
    TCODE   tDepthPlus, "深度 +"
    TCODE   tDepthMinus,"深度 -"
    TCODE   tDepthReset,"重置MOD深度:"
    TCODE   tSetup_mix, "设置混合比"
    TCODE   tCCRMode,   "CCR模式:"             ; CCR Mode:
    TCODE   tCCRModeFixedSP,  "选定设置点"        ; Fixed SP
    TCODE   tCCRModeSensor,   "传感器"          ; Sensor
    TCODE   tCCRModeAutoSP,   "自动设置点"         ; Auto SP
    TCODE   tSP,            "设置点"                ; SP (SetPoint)
    TCODE   tSPPlus,    "氧分压+"                  ; pO2+
    TCODE   tSensorFallback, "偏差隔离:"        ; Fallback:
    TCODE   tppO2,      "氧分压:"                 ; ppO2:

	TCODE   tNewBattTitle,      "新电池?"
	TCODE	tNewBattOld,		"保留旧电池"
	TCODE	tNewBattNew36,		"新3.6V锂电池"
	TCODE	tNewBattNew15,		"新1.5V AA电池"
	TCODE	tNewBattAccu,		"3.6V锂AA电池"
	TCODE	tNew18650,		"内置18650锂电池"
	TCODE	tNew16650,		"内置16650锂电池"


; Gaslist management
    TCODE   tGas,       "气体"           ; Gas
    TCODE   tGasErr,    "Err"           ; Err  (3 chars)
        
; Communication Menu
	TCODE	tUsbTitle,	"USB模式"
    TCODE   tBleTitle,  "蓝牙模式"
	TCODE	tUsbStarting,  "传输..."
	TCODE	tUsbStartDone, "完成"
	TCODE	tUsbServiceMode, "检测模式已激活"
	TCODE	tUsbClosed,		"端口关闭"
	TCODE	tUsbExit,		"已退出"
	TCODE	tUsbDownloadMode, "下载模式已激活"
    TCODE   tUsbLlBld,      "底层引导载入"

; Dive Settings
    TCODE   tDvMode,    "潜水模式:"
    TCODE   tDvOC,      "开放水肺系统"
    TCODE   tDvCC,      "密闭式循环系统"
    TCODE   tDvGauge,   "仪表模式"
    TCODE   tDvApnea,   "自由潜模式"
    TCODE   tDvPSCR,	"pSCR"
    TCODE   tDkMode,    "减压模型:"
    TCODE   tZHL16,     "ZH-L16"
    TCODE   tZHL16GF,   "ZH-L16+GF"
    TCODE   tPPO2Max,   "最大氧分压:"
    TCODE   tPPO2DECO,	"减压氧分压:"
    TCODE   tPPO2MIN,   "最小氧分压:"
    TCODE   tLastDecostop, "最浅停留深度:"
    TCODE   tDecoparameters, "减压参数"
    TCODE   tGF_low,    "GF low:"
    TCODE   tGF_high,   "GF high:"
    TCODE   tSaturationMult,    "饱和:"
    TCODE   tDesaturationMult,  "不饱和:"
    TCODE   tFTTSMenu,  "返回水面时间:"            ; Future TTS
    TCODE   taGFMenu,   "备用GF"         ; Alternative GF
    TCODE   taGF_low,   "备用GF low:"               ; aGF low
    TCODE   taGF_high,  "备用GF high:"              ; aGF high
    TCODE   taGF_enable,"可选备用GF:"        ; aGF Selectable
    TCODE   tDiveaGF_active,"备用GF!"               ; aGF!
    TCODE   tppO2settings,"氧分压设置"        ; ppO2 Settings
    TCODE   tsafetystopmenu,"安全停留:"       ; Safety Stop:

; Display Settings
    TCODE   tBright,    "亮度:"
    TCODE   tEco,       "节能"
    TCODE   tMedium,    "标准"
    TCODE   tHigh,      "高亮"
    TCODE   tDvSalinity,"盐度:"             ; Salinity
    TCODE   tFlip,      "屏幕翻转:"        ; Rotate Screen
    TCODE   tMODwarning,"MOD报警:"          ; MOD warning
    TCODE   tShowppO2,  "一直显示氧分压:"     ; Always show ppO2:
    TCODE   tTimeoutDive,"潜水中止时限:"	; Dive Timeout

; VSI display Settings
    TCODE   tVSItext2,  "最大上升速度:"     ; Variable speed:
    TCODE   tVSIgraph,  "上升速度图形:"        ; Speed graph:

; Setup Menu
    TCODE   tSystSets,  "设置"
    TCODE   tLanguage,  "语言:"
    TCODE   tEnglish,   "英语"
    TCODE   tGerman,    "德语"
    TCODE   tFrench,    "法语"
    TCODE   tItalian,   "意大利语"
    TCODE   tChineseS,   "简体中文"	
    TCODE   tCompassMenu,"指北针校准"  ; Compass calibration
    TCODE   tCompassGain,"指北针增益:"         ; Compass gain:

    TCODE   tUnits,     "单位:"
    TCODE   tMetric,     " 米/°C"            ; Enum menu
    TCODE   tImperial,   "英尺/°F"
	
    TCODE   tDefName,   "HW OSTC"
	TCODE	tbar,		"bar"				; bar

    TCODE   tButtonleft,"左键:"      ; Left button
    TCODE   tButtonright,"右键:"    ; Right button

; Units for all menu
    TCODE   tMeters,    "米"
	TCODE	tFeets,		"英尺"
	TCODE	tFeets1,	"英尺"
    TCODE   tMinutes,   "'"
    TCODE   tPercent,   "%"
    TCODE   tLitersMinute, "升/分钟"

; Date
	TCODE	tDateFormat, "日期:"
	TCODE	tDateformat, "月日年"
	TCODE	tDateformat1,"日月年"
	TCODE	tDateformat2,"年月日"

; Simulator menu
    TCODE   tInter, 	"启动模拟"  ; Start Simulator
    TCODE   tPlan,  	"潜水模拟"        ; Simulator

; Decoplanner submenu
    TCODE   tBtTm,  	"滞底时间:"        ; Bot. Time: (10 chars)
    TCODE   tBtTm_short,"滞底:"             ; Bot. Time: (max. 6 chars)
    TCODE   tMxDep, 	"最大深度:"        ; Max Depth: (10 chars)
    TCODE   tIntvl, 	"水面间隔 :"        ; Interval : (10 chars)
    TCODE   tDeco,  	"减压计算"    ; Calculate Deco
    TCODE   tDivePlan,  "潜水计划:"        ; Dive Plan:
    TCODE   tNoDeco,    "免减压"           ; No Deco
    TCODE   tMore,      "更多..."           ; More...
    TCODE   tGasUsage,  "气体使用"         ; Gas Usage
    TCODE   tSetBotUse, "滞底气体: "      ; Bottom Gas: (space)
    TCODE   tSetDecoUse,"减压气体: "        ; Deco Gas: (space)


; Information menu
    TCODE   tFirmware,  "固件: "        ; Firmware: (space)
    TCODE   tSerial,    "序列号  : "        ; Serial  : (space)
    TCODE   tTotalDives,"总潜数: "	    ; Total Dives:
    TCODE   tBatteryV,  "电池: "         ; Battery:
    TCODE   tUptime,	"运行时间: "	    ; Uptime: 
    
; Divemode screen
    TCODE   tNDL,       "免减压极限"
    TCODE   tTTS,   	"返回水面时间"
	TCODE	tVelMetric,	 "米每分钟"
	TCODE	tVelImperial,"英尺每分钟"
	TCODE	tGasSelect,  "选择气体"       ; Select Gas
	TCODE	tSelectAir,	 "空气 "     		; Air
	TCODE	tSelectO2,	 "纯氧 "             ; O2
    TCODE   tSelectNx,   "Nx"               ; Nx
    TCODE   tSelectTx,   "Tx"               ; Tx
    TCODE   tDepth,      "深度"            ; Depth
    TCODE   tMaxDepth,   "最大深度"       ; Max. Depth - max 10chars!
    TCODE   tDivetime,   "潜水时间"         ; Divetime
    TCODE   tDiveHudMask1,  "传感器 1"
    TCODE   tDiveHudMask2,  "传感器 2"
    TCODE   tDiveHudMask3,  "传感器 3"
    TCODE   tDiveTotalAvg,  "平均深度"
    TCODE   tDiveStopwatch, "计时器"
    TCODE   tDiveStopAvg,   "计时均深"   ; 11chars max
    TCODE   tApnoeTotal,    " 总计"        ; Total (Six chars, right alligned)
    TCODE   tApnoeMax,      "最后下潜"  ; Last descent
    TCODE   tApnoeSurface,  "水面时间"  ; Surface Time
    TCODE   tDiveDecoplan,  "减压计划"      ; Decoplan
    TCODE   tDiveClock,     "时钟"         ; Clock
    TCODE   tDiveEAD_END,   "EAD/END"       ; EAD/END
    TCODE   tDiveTissues,   "组织隔腔"       ; Tissues
    TCODE   tEND,           "END:"          ; END:
    TCODE   tHe,            "He"            ; He
    TCODE   tN2,            "N2"            ; N2
    TCODE   tDiveBailout,   "逃生"       ; Bailout
    TCODE   tGFactors,      "GF值"     ; GF Values
    TCODE   taGFactors,     "备用GF值"    ; aGF Values
    TCODE   tGFInfo,        "当前GF值"    ; GF Info
    TCODE   tCeiling,       "上限"       ; Ceiling
    TCODE   tDiveSafetyStop,"停留"         ; Stop (Four chars, right alligned)
    TCODE   tDiveFallback,  "偏差隔离!"     ; Fallback! (max. nine chars)
    TCODE   tSensorCheck,   "传感器检测"  ; Sensor Check

; Divemode menu
    TCODE   tDivePreMenu,   "菜单?"         ; Menu?
    TCODE   tSetHeading,    "标记航向"       ; Bearing (Max. seven chars)

; Simulator menu
	TCODE	tQuitSim,	 "退出?"            ; Quit Simulation?

; Logbook
	TCODE	tCNS2,		"CNS:"
	TCODE	tAVG,		"平均深度:"
	TCODE	tGF,		"GF:"
	TCODE	tSAT,		"Sat:"					; Sat:

; Logbook units
	TCODE	tLogTunitC,	"°C"	
	TCODE	tLogTunitF,	"°F"
	TCODE	tKGL,		"kg/l"
	TCODE	tMBAR,		"mbar"

; Logbook menu
	TCODE	tNextLog, 	"下个日志"

; Reset menu
	TCODE	tReboot,	"重启"			;Reboot
	TCODE	tResetMenu2,"确认重置?"		;Are you sure?
	TCODE	tAbort,     "取消"             ;Abort
	TCODE	tResetSettings, "全部重置" 	;Reset all
	TCODE	tResetDeco,	"减压重置"		;Reset Deco
    TCODE	tResetBattery,	"电池重置"		;Reset Battery
    TCODE	tResetLogbook,	"日志重置"		;Reset Logbook

; Set Time Menu/Set Date Menu
	TCODE	tSetHours,	"设置小时"			; Set Hours
	TCODE	tSetMinutes,"设置分钟"		; Set Minutes
	TCODE	tSetSeconds,"清除秒"		; Clear seconds
	TCODE	tSetDay,	"设置日期"			; Set Day
	TCODE	tSetMonth,	"设置月份"			; Set Month
	TCODE	tSetYear,	"设置年份"			; Set Year

; Logbook Offset Menu
	TCODE	tLogOffset,		"日志计数调整"	; Logbook offset
	TCODE	tLogOffsetp1,	"+1"				; +1
	TCODE	tLogOffsetp10,	"+10"				; +10
	TCODE	tLogOffsetm1,	"-1"				; -1
	TCODE	tLogOffsetm10,	"-10"				; -10

; Sampling rate
    TCODE   tSamplingrate,  "采样率:"    ; Sampling rate:
    TCODE   tSampling2s,    "2秒"                ; 2s
    TCODE   tSampling10s,   "10秒"               ; 10s

; Compass directions
    TCODE   tN,             "北 "                ; N(orth),338°-22°
    TCODE   tNE,            "东北"                ; North-East,23°-67°
    TCODE   tE,             "东 "                ; E(east),68°-112°
    TCODE   tSE,            "东南"                ; South-East,113°-157°
    TCODE   tS,             "南 "                ; S(outh),158°-202°
    TCODE   tSW,            "西南"                ; South-West,203°-247°
    TCODE   tW,             "西 "                ; W(West),248°-292°
    TCODE   tNW,            "西北"                ; North-West,293°-337°

; Color Scheme menu
    TCODE   tColorScheme,   "配色"     ; Colour scheme
    TCODE   tColorSetDive,  "潜水模式:"         ; Divemode:
    TCODE   tColorSetName0, "标准"          ; Standard
    TCODE   tColorSetName1, "红"               ; Red
    TCODE   tColorSetName2, "绿"             ; Green
    TCODE   tColorSetName3, "蓝"              ; Blue
    
; PSCR Menu and Settings
    TCODE   tPSCRMenu,	    "pSCR"		; PSCR Menu
    TCODE   tPSCR_O2_drop,  "氧含量降低:"		; O2 drop
    TCODE   tPSCR_lungratio,"肺容量比:"	; lung ratio
    
