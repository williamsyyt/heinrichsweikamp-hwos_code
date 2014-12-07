;==========================================================================
; Font index exported ven. janv. 13 2012 by FontIO.cpp 110 2011-06-07 11:26:40Z gascuel $ 
;==========================================================================
aa_font48_chars		EQU	.18
aa_font48_firstChar	EQU	'.'
aa_font48_lastChar	EQU	'?'
;
aa_font48_idx:
			DW	aa_font48_bits + 0x0000	; . dot
			DW	aa_font48_bits + 0x001F	; / slash
			DW	aa_font48_bits + 0x0025	; 0 0
			DW	aa_font48_bits + 0x00A2	; 1 1
			DW	aa_font48_bits + 0x00EF	; 2 2
			DW	aa_font48_bits + 0x0180	; 3 3
			DW	aa_font48_bits + 0x0220	; 4 4
			DW	aa_font48_bits + 0x028B	; 5 5
			DW	aa_font48_bits + 0x0312	; 6 6
			DW	aa_font48_bits + 0x03BF	; 7 7
			DW	aa_font48_bits + 0x0430	; 8 8
			DW	aa_font48_bits + 0x04F1	; 9 9
			DW	aa_font48_bits + 0x05A1	; : colon
			DW	aa_font48_bits + 0x05D6	; ; semicolon
			DW	aa_font48_bits + 0x060B	; < less
			DW	aa_font48_bits + 0x066B	; = equal
			DW	aa_font48_bits + 0x06B7	; > greater
			DW	aa_font48_bits + 0x073D	; ? question
			DW	aa_font48_bits + 0x0743	; END OF INDEX
