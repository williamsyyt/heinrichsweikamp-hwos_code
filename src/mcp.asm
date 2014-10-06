;=============================================================================
;
;   File mcp.asm
;
;   Basic routines for RX circuity
;
;   Copyright (c) 2012, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2012-08-12 : [mH] Creation


#include "ostc3.inc"
#include "wait.inc"

mcp code

    global  mcp_sleep
mcp_sleep:
    bcf     INTCON3,INT3IE          ; Disable INT3
    bcf     mcp_power               ; RX off
    btfsc   mcp_power
    bra     $-4
    return



        END
