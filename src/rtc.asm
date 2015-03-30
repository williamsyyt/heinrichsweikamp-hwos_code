;=============================================================================
;
;   File rtc.asm
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-08 : [mH] moving from OSTC code

#include "ostc3.inc"
#include "math.inc"

sensors     CODE

	global	rtc_init
rtc_init:
	movlw	.1
	movwf	secs
	movlw	.59
	movwf	mins
	movlw	.12
	movwf	hours
	movlw	.30
	movwf	day
	movlw	.3
	movwf	month
	movlw	.15
	movwf	year
	rcall	rtc_set_rtc			; writes mins,sec,hours,day,month and year to rtc module
	return
	
	global	rtc_set_rtc
rtc_set_rtc:
	banksel 0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
	movlw 	0x55
	movwf 	EECON2
	movlw 	0xAA
	movwf 	EECON2
	bsf 	RTCCFG,RTCWREN		; Unlock sequence for RTCWREN
	bsf		RTCCFG,RTCPTR1
	bsf		RTCCFG,RTCPTR0		; year
	movff	year,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALL				; year
	movwf	RTCVALH				; dummy write
	movff	day,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALL				;day
	movff	month,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALH				;month
	movff	hours,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALL				;hours
	movlw	d'0'
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALH				;weekday
	movff	secs,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALL				;secs
	movff	mins,WREG
	rcall	rtc_dec2bcd			; IN: temp1 in WREG, OUT: WREG in BCD, also sets to bank16h!
	movwf	RTCVALH				;minutes
	movlw 	0x55
	movwf 	EECON2
	movlw 	0xAA
	movwf 	EECON2
	bcf 	RTCCFG,RTCWREN		; Lock sequence for RTCWREN
	banksel	common
	return

rtc_dec2bcd:
	banksel	temp1
	movwf	temp1				; Input in dec
	setf	temp2				; 10s
	
rtc_dec2bcd2:
	incf	temp2,F             ; Count 10's
	movlw	d'10'
	subwf	temp1,F
	btfss	STATUS,N
	bra		rtc_dec2bcd2
	movlw	d'10'
	addwf	temp1,F				; 1s
    swapf   temp2,W             ; swap to bit 7-4 -> WREG
	addwf	temp1,W				; Result in BCD
	banksel 0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
	return

	END