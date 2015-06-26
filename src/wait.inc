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
;   2011-06-24 : [MH]  Added clock speeds.
	
;=============================================================================

WAITMS		macro	waitms_temp
			movlw	waitms_temp
			call    WAITMSX
			endm
            extern  WAITMSX

; asm routines in hwos.asm
	extern	speed_eco			; SPEED_ECO: 4MHz
	extern	speed_normal		; SPEED_NORMAL: 16MHz
	extern	speed_fastest		; SPEED_FASTEST: 64MHz 

