;=============================================================================
;
;   File strings.asm
;
;   Implementation code various string functions.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2010-12-02 : [jDG] Creation...
;
; See strings.inc for doc and public calling macros.

#include "ostc3.inc"
#include "varargs.inc"

        extern  aa_wordprocessor

basic   CODE
;=============================================================================
; Variants that call word_processor at the end:
        global  strcpy_block_print
strcpy_block_print:
        lfsr    FSR2, buffer
        global  strcat_block_print
strcat_block_print:
        clrf    PRODL,A
        bra     strings_common

; Variants that do not call word_processor at end:
        global  strcpy_block
strcpy_block:
        lfsr    FSR2, buffer
        global  strcat_block
strcat_block:
        setf    PRODL,A
        
; Common part: append the string from PROM return address:
strings_common:
        VARARGS_BEGIN

        rcall   strcat_prom

        VARARGS_ALIGN
        VARARGS_END
		
		btfsc   PRODL,0,A               ; Should we print afterward ?
		return                          ; NO: then return straight away.
		goto    aa_wordprocessor        ; ELSE: print it...

;=============================================================================
; Copy multi-lingual text from FSR1 12bit pointers, to buffer.
;
; Input: FSR1 = 12bit pointer to multi-lingual text.
; Output: FSR2 points to closing null byte in buffer.
; Trashed: TBLPTR, TABLAT.
        global  strcpy_text
strcpy_text:
        rcall   text_get_tblptr
        bra     strcpy_prom

; Copy then print multi-lingual text from FSR1 12bit pointers, to buffer.
;
; Input: FSR1 = 12bit pointer to multi-lingual text.
; Output: FSR2 points to closing null byte in buffer.
; Trashed: TBLPTR, TABLAT.
        global  strcpy_text_print
strcpy_text_print:
        rcall   text_get_tblptr
        bra     strcpy_prom_print

; Append multi-lingual text from FSR1 12bit pointers, to buffer at FRS2.
;
; Input: FSR1 = 12bit pointer to multi-lingual text.
;        FSR2 = Current position in buffer.
; Output: FSR2 points to closing null byte in buffer.
; Trashed: TBLPTR, TABLAT.
        global  strcat_text
strcat_text:
        rcall   text_get_tblptr
        bra     strcat_prom

; Append then print multi-lingual text from FSR1 12bit pointers, to buffer at FRS2.
;
; Input: FSR1 = 12bit pointer to multi-lingual text.
;        FSR2 = Current position in buffer.
; Output: FSR2 points to closing null byte in buffer.
; Trashed: TBLPTR, TABLAT.
        global  strcat_text_print
strcat_text_print:
        rcall   text_get_tblptr
        bra     strcat_prom_print

;=============================================================================
; Get pointer to multilingual texl in TBLPTR
;
; Input:  FSR1 = 12bits of text handle.
;         opt_language = current language.
; Output: TBLPTR = 24bits of PROM address.
;
        global  text_get_tblptr
text_get_tblptr:
        extern  text_english_base
        movlw   UPPER(text_english_base); Complete 12bits to 24bits address.
        movwf   TBLPTRU
        movlw   HIGH(text_english_base)
        andlw   0xF0
        iorwf   FSR1H,W
        movwf   TBLPTRH
        movff   FSR1L,TBLPTRL

        movff   opt_language,WREG       ; Get lang
        bz      text_get_english        ; 0 == English
        dcfsnz  WREG                    ; 1 == German
        bra     text_get_german
        dcfsnz  WREG                    ; 2 == French
        bra     text_get_french
        dcfsnz  WREG                    ; 3 == Italian
        bra     text_get_italian
; Other ??? Keep english...

; Read 2-byte pointer to string
text_get_english:
        tblrd*+
        movff   TABLAT,WREG
        tblrd*+
        movff   WREG,TBLPTRL
        movff   TABLAT,TBLPTRH
        return

; Add correction for German table:
text_get_german:
        extern  text_german_base
        movlw   LOW(text_german_base)
        addwf   TBLPTRL
        movlw   HIGH(text_german_base)
        addwfc  TBLPTRH
        movlw   UPPER(text_german_base)
        addwfc  TBLPTRU

        movlw   LOW(text_english_base)
        subwf   TBLPTRL
        movlw   HIGH(text_english_base)
        subwfb  TBLPTRH
        movlw   UPPER(text_english_base)
        subwfb  TBLPTRU
        bra     text_get_english
        
        ; Add correction for French table:
text_get_french:
        extern  text_french_base
        movlw   LOW(text_french_base)
        addwf   TBLPTRL
        movlw   HIGH(text_french_base)
        addwfc  TBLPTRH
        movlw   UPPER(text_french_base)
        addwfc  TBLPTRU

        movlw   LOW(text_english_base)
        subwf   TBLPTRL
        movlw   HIGH(text_english_base)
        subwfb  TBLPTRH
        movlw   UPPER(text_english_base)
        subwfb  TBLPTRU
        bra     text_get_english
        
        ; Add correction for italian table:
text_get_italian:
        extern  text_italian_base
        movlw   LOW(text_italian_base)
        addwf   TBLPTRL
        movlw   HIGH(text_italian_base)
        addwfc  TBLPTRH
        movlw   UPPER(text_italian_base)
        addwfc  TBLPTRU

        movlw   LOW(text_english_base)
        subwf   TBLPTRL
        movlw   HIGH(text_english_base)
        subwfb  TBLPTRH
        movlw   UPPER(text_english_base)
        subwfb  TBLPTRU
        bra     text_get_english

;=============================================================================
; Copy a null-terminated string from TBLPTR to buffer.
;
; Input:  TBLPTR : string pointer into PROM.
; Output: string in buffer, FSR2 pointer on the closing null byte.
;
        global  strcpy_prom
strcpy_prom:
        lfsr    FSR2,buffer

; Append a null-terminated string from TBLPTR to buffer.
;
; Input:  TBLPTR : string pointer into PROM.
;         FRS2   : current character position.
; Output: string in buffer, FSR2 pointer on the closing null byte.
;
        global  strcat_prom
strcat_prom:
        tblrd*+
        movf    TABLAT,W
        movwf   POSTINC2
        bnz     strcat_prom
        movf    POSTDEC2,W               ; rewind one char in string buffer.
        return

;=============================================================================
; Variant that calls word processor right-away...
        global  strcpy_prom_print
        global  strcat_prom_print

strcpy_prom_print:
        lfsr    FSR2,buffer
strcat_prom_print:
        rcall   strcat_prom
        goto    aa_wordprocessor

;=============================================================================

        global      start_tiny_block
start_tiny_block:
        clrf        WREG
        movff       WREG, win_font      ; Need a bank-safe move here !
        bra         start_common

        global      start_small_block
start_small_block:
        movlw       1
        movff       WREG, win_font      ; Need a bank-safe move here !
        bra         start_common

        global      start_std_block
start_std_block:
        movlw       2
        movff       WREG, win_font      ; Need a bank-safe move here !
        bra         start_common

        global      start_medium_block
start_medium_block:
        movlw       3
        movff       WREG, win_font      ; Need a bank-safe move here !
        bra         start_common

        global      start_large_block
start_large_block:
        movlw       4
        movff       WREG, win_font      ; Need a bank-safe move here !
;        bra         start_common
start_common:
        VARARGS_BEGIN
            VARARGS_GET8    win_leftx2
            VARARGS_GET8    win_top
        VARARGS_END
        lfsr    FSR2,buffer             ; point to buffer here
        return

        END