;=============================================================================
;
;   File icons.asm
;
;   Tables for all OSTC3 icons.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-05-24 : [jDG] Creation with icons for demo menus.

icons_data   CODE

; Dive mode warning icon
	global	dive_warning2_block
#include "../src/Icons/dive_warning2.inc"	;45x39 px
; small warning icon
;	global	warning_block
;#include "../src/Icons/warning.inc"       ;25x22 px
	global	ostc3_logo_block
; OSTC3 scribble
#include "../src/Icons/ostc3_logo.inc"    ;220x61 px

        END
