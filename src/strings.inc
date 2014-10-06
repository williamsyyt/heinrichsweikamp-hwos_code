;=============================================================================
;
;   File strings.asm
;
;   Implementation code various string functions.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2010-12-02 : [jDG] Creation...
;

;=============================================================================
; Copy a short embebed string at start of the string buffer (letter)
; Input:    (nothing)
; Output:   chars are copied into letter, starting at the beginning.
;           FSR2 point to the first unused char (the NULL termination).
;           
; Trashed:  WREG, TBLPTR, TABLAT, PRODL
; Note:     This are all bank-safe call.
;
        extern  strcpy_block
STRCPY  macro   string
        call    strcpy_block
        DB  string, 0
        endm

;=============================================================================
; A variant of STRCPY that appends chars to the current FSR2 pointer.
; Input/Output/Trashed : see STRCPY.
        extern  strcat_block
STRCAT  macro   string
        call    strcat_block
        DB  string, 0
        endm

;=============================================================================
; A variant of STRCAT when there is just on char to output
; Input/Output/Trashed : none.
PUTC    macro   char
        movlw   char
        movwf   POSTINC2
        endm

;=============================================================================
; A variant of STRCPY that send the string to the word processor afterwards.
; Input/Output: see STRCPY.
; Trashed: See STRCPY + word_processor. In particular, switch RAM to Bank1 
;
        extern  strcpy_block_print
STRCPY_PRINT    macro string
        call    strcpy_block_print
        DB  string, 0
        endm

; Trashed: See STRCPY + word_processor. In particular, switch RAM to Bank1 
        extern  strcat_block_print
STRCAT_PRINT    macro string
        call    strcat_block_print
        DB  string, 0
        endm

;=============================================================================
; Subroutines to operate on strings from PROM code
;
; Input:  TBLPTR : string pointer into PROM.
; Output: string in buffer, FSR2 pointer on the closing null byte.

        extern  strcpy_prom, strcat_prom
        extern  strcpy_prom_print, strcat_prom_print

;=============================================================================
; Subroutines and macros to operate on multilingual text
;
        extern  strcpy_text
STRCPY_TEXT macro   txt
        extern  txt
        lfsr    FSR1, txt
        call    strcpy_text
        endm

        extern  strcpy_text_print
STRCPY_TEXT_PRINT macro   txt
        extern  txt
        lfsr    FSR1, txt
        call    strcpy_text_print
        endm

        extern  strcat_text
STRCAT_TEXT macro   txt
        extern  txt
        lfsr    FSR1, txt
        call    strcat_text
        endm

        extern  strcat_text_print
STRCAT_TEXT_PRINT macro   txt
        extern  txt
        lfsr    FSR1, txt
        call    strcat_text_print
        endm

;=============================================================================
; A shortcut for the macros WIN_TOP + WIN_LEFT + WIN_FONT + WIN_INVERT.
; The idea is to replace a 4x6=24 bytes sequence by a more compact 6bytes one.
;
; Trashed: TBLPTR, TABLAT, WREG.
; Note:    This are all bank-safe call.
        extern  start_tiny_block
WIN_TINY macro x, y
        call    start_tiny_block
        DB      x, y
        endm

        extern  start_tiny_invert_block
WIN_TINY_INVERT macro x, y
        call    start_tiny_invert_block
        DB      x, y
        endm

        extern  start_small_block
WIN_SMALL macro x, y
        call    start_small_block
        DB      x, y
        endm

        extern  start_small_invert_block
WIN_SMALL_INVERT macro x, y
        call    start_small_invert_block
        DB      x, y
        endm

        extern  start_std_block
WIN_STD macro x, y
        call    start_std_block
        DB      x, y
        endm

        extern  start_std_invert_block
WIN_STD_INVERT macro x, y
        call    start_std_invert_block
        DB      x, y
        endm

        extern  start_medium_block
WIN_MEDIUM   macro   x, y
        call    start_medium_block
        DB      x, y
        endm

        extern  start_medium_invert_block
WIN_MEDIUM_INVERT   macro   x, y
        call    start_medium_invert_block
        DB      x, y
        endm

       extern  start_large_block
WIN_LARGE   macro   x, y
       call    start_large_block
       DB      x, y
       endm

        extern  start_large_invert_block
WIN_LARGE_INVERT   macro   x, y
        call    start_large_invert_block
        DB      x, y
        endm

;=============================================================================
; Shortcuts for compact display programmings.
TEXT_TINY macro   x, y, txt
        WIN_TINY   x,y
        STRCPY_TEXT_PRINT txt
        endm

TEXT_SMALL  macro   x, y, txt
        WIN_SMALL   x,y
        STRCPY_TEXT_PRINT txt
        endm

TEXT_SMALL_INVERT  macro   x, y, txt
        WIN_SMALL_INVERT   x,y
        STRCPY_TEXT_PRINT txt
        endm

TEXT_MEDIUM  macro   x, y, txt
        WIN_MEDIUM   x,y
        STRCPY_TEXT_PRINT txt
        endm

TEXT_LARGE  macro   x, y, txt
        WIN_LARGE   x,y
        STRCPY_TEXT_PRINT txt
        endm

STRING_TINY macro   x, y, string
        WIN_SMALL   x,y
        STRCPY_PRINT string
        endm

STRING_SMALL  macro   x, y, string
        WIN_SMALL   x,y
        STRCPY_PRINT string
        endm

STRING_MEDIUM  macro   x, y, string
        WIN_MEDIUM   x,y
        STRCPY_PRINT string
        endm

STRING_LARGE  macro   x, y, string
        WIN_LARGE   x,y
        STRCPY_PRINT string
        endm

