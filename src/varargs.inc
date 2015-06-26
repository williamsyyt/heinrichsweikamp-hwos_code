;=============================================================================
;
;    File varargs.inc
;
;    Utilities to pass multiple arguments in compact code stream.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-05-27 : [jDG] Creation.
;

VARARGS_BEGIN   macro
        movff   TOSL, TBLPTRL
        movff   TOSH, TBLPTRH
        movff   TOSU, TBLPTRU
        endm

VARARGS_GET8    macro   register
        tblrd*+
        movff   TABLAT, register
        endm

VARARGS_GET16   macro   register
        tblrd*+
        movff   TABLAT, register+0
        tblrd*+
        movff   TABLAT, register+1
        endm

VARARGS_GET24   macro   register
        tblrd*+
        movff   TABLAT, register+0
        tblrd*+
        movff   TABLAT, register+1
        tblrd*+
        movff   TABLAT, register+2
        endm

VARARGS_ALIGN   macro
        local   no_tblptr_align

        btfss   TBLPTRL,0
        bra     no_tblptr_align
        incf    TBLPTRL
        movlw   0
        addwfc  TBLPTRH
        addwfc  TBLPTRU
no_tblptr_align:
        endm

VARARGS_END macro
        ; Compute string length (modulo 256):
        movf    TOSL,W
        subwf   TBLPTRL,W
        
		; Then 24bit add to return address
		addwf	TOSL,F
		movlw	0			            ; Clear WREG, but keep carry
		addwfc	TOSH,F
		addwfc	TOSU,F
		endm

