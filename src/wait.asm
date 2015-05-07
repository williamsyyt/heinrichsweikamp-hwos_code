;=============================================================================
;
;   File wait.asm
;
;  Wait routines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2004-01-31 : [chsw] Initial version.
;   2007-05-11 : Updated (OSTC code).
;   2011-10-18 : [mH] Timings tested with oscilloscope
	
#include "hwos.inc"

basic       CODE

; ==========================================================
; 	WAIT 1 MILLISECOND   (Not exact: 1,008ms +/- 30,5µs + worst case ISR latency)
; ==========================================================

            global  WAITMSX
WAITMSX		movwf	waitms_temp
			
WAITMSX2	setf	TMR5H
			movlw	.255-.32 			;32 x 31,5µs = 1,008ms
			movwf	TMR5L
			bcf		PIR5,TMR5IF			; Clear flag
WAITMSX3	btfss	PIR5,TMR5IF
			bra		WAITMSX3			; Wait loop
			decfsz	waitms_temp,F
			bra		WAITMSX2
			return

;=============================================================================

            END