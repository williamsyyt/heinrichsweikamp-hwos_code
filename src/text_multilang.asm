;=============================================================================
;
;   File text_multilang.asm
;
;   Implementation text in various selectable languages.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-06-12 : [jDG] Creation...

#include "text_multilang.inc"

; Because text are indexed by 12bits value in FSR register, they can't
; just be anywhere. It is safe to make them start in address 0xHHH000.
texts       code    0x009000

;=============================================================================
            global  text_english_base
text_english_base:

;---- PASS 1 : generate description block ------------------------------------
tcode_idx   set     0
LANG        set     0
#define     TCODE TCODE_1
#include    "text_english.inc"
#undefine   TCODE

;---- PASS 2 : generate text contens -----------------------------------------
tcode_idx   set     0
#define     TCODE TCODE_2
#include    "text_english.inc"
#undefine   TCODE

;=============================================================================
            global  text_german_base
text_german_base:

;---- PASS 1 : generate description block ------------------------------------
tcode_idx   set     0
LANG        set     1
#define     TCODE TCODE_1
#include    "text_german.inc"
#undefine   TCODE

;---- PASS 2 : generate text contens -----------------------------------------
tcode_idx   set     0
#define     TCODE TCODE_2
#include    "text_german.inc"
#undefine   TCODE

;=============================================================================
            global  text_french_base
text_french_base

;---- PASS 1 : generate description block ------------------------------------
tcode_idx   set     0
LANG        set     2
#define     TCODE TCODE_1
#include    "text_french.inc"
#undefine   TCODE

;---- PASS 2 : generate text contens -----------------------------------------
tcode_idx   set     0
#define     TCODE TCODE_2
#include    "text_french.inc"
#undefine   TCODE

;=============================================================================
            global  text_italian_base
text_italian_base

;---- PASS 1 : generate description block ------------------------------------
tcode_idx   set     0
LANG        set     3
#define     TCODE TCODE_1
#include    "text_italian.inc"
#undefine   TCODE

;---- PASS 2 : generate text contens -----------------------------------------
tcode_idx   set     0
#define     TCODE TCODE_2
#include    "text_italian.inc"
#undefine   TCODE

;=============================================================================

        END
