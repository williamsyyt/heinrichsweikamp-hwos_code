;=============================================================================
;
;   File isr.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-06 : [mH] moving from OSTC code

SAFE_2BYTE_COPY MACRO  from, to
        local   retry
retry:
        movff   from+1,WREG             ; High byte in W.
        movff   WREG,to+1               ; and destination.
        movff   from+0,to+0             ; Copy low byte.
        movff   from+1,TABLAT           ; another bank-safe read.
        xorwf   TABLAT,W                ; High byte changed ?
        bnz     retry
        ENDM
