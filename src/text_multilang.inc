;=============================================================================
;
;    File text_multilang.inc
;
;    Implementation text in various selectable languages.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-06-12 : [jDG] Creation...
;
; Text definitions (text_french.asm example):
;	TCODE	tYes,   "Oui"               ; Yes
;   TCODE   tNo,    "Non"               ; No
;   TCODE   tLogbk, "Carnet de plongées"  ; Logbook
;
; Text direct usage:
;   lfsr    FSR1,tYes                   ; Load a 12bit text index
;   call    strcpy_text                 ; Copy to string buffer.
;   PUTC    '/'
;   lfsr    FSR1,tNo
;   call    strcat_text_print           ; Append, and call word processor.
;
; RATIONALS:
;  - The macro should define a label so that text files can be reordered
;    to keep consistency while adding more option (and make translator life easier).
;
;  - The text positions is keept in menu blocks for the menu processor.
;
;  - library function take text number from the FSR1 register, because a
;    lfsr instruction loads a 12bits constant at once.
;
;=============================================================================
;

; Pass 1: generate jump table, and define labels.
TCODE_1 macro   label, text
tcode_idx set   tcode_idx+1
    If LANG == 0
        global  label
label:
    Endif
        dw      t#v(LANG)_#v(tcode_idx)
        endm
;
; Pass 2: generates string table.
TCODE_2 macro   label, text
tcode_idx set   tcode_idx+1
t#v(LANG)_#v(tcode_idx):
        db      text, 0
        endm
;
;=============================================================================
; strcpy_text : copy a multiling text into string buffer.
;
; Input:   FSR1 :  text index
; Output:  Buffer: filled with the text.
;          FSR2:   pointer to end of copied text (the null char).
; Trashed: WREG.
        extern  strcpy_text

;=============================================================================
; strcpy_text_print : Same as above, but calls word processor afterward.
;
; Input:   FSR1 :  text index
; Output:  Buffer: filled with the text.
;          FSR2:   pointer to end of copied text (the null char).
; Trashed: WREG.
        extern  strcpy_text_print

;=============================================================================
; strcat_text : Append a multiling text to FSR2 pointer.
;
; Input:   FSR1 :  text index
;          FSR2 :  Current position (in Buffer)
; Output:  FSR2:   pointer to end of copied text (the null char).
; Trashed: WREG.
        extern  strcat_text

;=============================================================================
; strcat_text_print : Same as above, but calls word processor afterward.
;
; Input:   FSR1 :  text index
;          FSR2 :  Current position (in Buffer)
; Output:  FSR2:   pointer to end of copied text (the null char).
; Trashed: WREG.
        extern  strcat_text_print

