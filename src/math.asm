;=============================================================================
;
;   File math.asm
;
;   Math subroutines
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-03 : [mH] moving from OSTC code


#include    "ostc3.inc"                  ; Mandatory header

basic   CODE
;=============================================================================
	global	convert_time
convert_time:							; converts hi:lo in minutes to hours (hi) and minutes (lo)
	movff	lo,xA+0						; divide by 60...
	movff	hi,xA+1						; 
	movlw	d'60'						; 
	movwf	xB+0						; 
	clrf	xB+1						; 
	rcall	div16x16					; xA/xB=xC with xA as remainder
	movff	xC+0,hi						; Hours
	movff	xA+0,lo						; =remaining minutes (0.....59)
	return

	global	div16
div16:
; divA=divA/2^divB (divB: 8Bit only!)
	bcf		STATUS,C
	rrcf	divA+1
	rrcf	divA+0
	decfsz	divB
	bra		div16
	return

	global	sub16
sub16:
;  sub_c = sub_a - sub_b (with signed values)
	bcf		neg_flag
	movf   	sub_b+0, W             	; Get Value to be subtracted
	subwf  	sub_a+0, W             	; Do the High Byte
	movwf  	sub_c+0
	movf   	sub_b+1, W              ; Get the Value to be Subbed
	subwfb 	sub_a+1, W
	movwf  	sub_c+1

	btfss	STATUS,N                ; Negativ result ?
	return							; NO: result positive done.

	bsf		neg_flag				; MARK result negative

    comf    sub_c+1                 ; 16bit sign change.
    negf    sub_c+0
    btfsc   STATUS,C                ; Carry to propagate ?
    incf    sub_c+1,F               ; YES: do it.

    return        

	global	subU16
subU16:
;  sub_c = sub_a - sub_b (with UNSIGNED values)
	bcf		neg_flag
	movf   	sub_b+0, W             	; Get Value to be subtracted
	subwf  	sub_a+0, W             	; Do the High Byte
	movwf  	sub_c+0
	movf   	sub_b+1, W              ; Get the Value to be Subbed
	subwfb 	sub_a+1, W
	movwf  	sub_c+1
	btfsc	STATUS,C                ; Borrow to propagate ? (B == /CARRY)
	return							; NO: result positive done.
	bsf		neg_flag				; MARK result negative
    comf    sub_c+1                 ; 16bit sign change.
    negf    sub_c+0
    btfsc   STATUS,C                ; Carry to propagate ?
    incf    sub_c+1,F               ; YES: do it.
    return        

	global	mult16x16
mult16x16:		;xA*xB=xC
	clrf    xC+2        	  ;  Clear the High-Order Bits
	clrf    xC+3
	movf    xA, w               ;  Do the "L" Multiplication first
	mulwf   xB
	movf    PRODL, w            ;  Save result
	movwf   xC
	movf    PRODH, w
	movwf   xC+1
	movf    xA, w               ;  Do the "I" Multiplication
	mulwf   xB+1
	movf    PRODL, w            ;  Save the Most Significant Byte First
	addwf   xC+1, f
	movf    PRODH, w
	addwfc  xC+2, f    	 		 ;  Add to the Last Result
	movf    xA+1, w               ;  Do the "O" Multiplication
	mulwf   xB
	movf    PRODL, w            ;  Add the Lower Byte Next
	addwf   xC+1, f
	movf    PRODH, w            ;  Add the High Byte First
	addwfc  xC+2, f
	btfsc   STATUS, C           ;  Add the Carry
	incf    xC+3, f
	movf    xA+1, w               ;  Do the "F" Multiplication
	mulwf   xB+1
	movf    PRODL, w
	addwf   xC+2, f
	movf    PRODH, w
	addwfc  xC+3, f
	return

	global	div16x16
div16x16:						;xA/xB=xC with xA+0 as remainder 	
								;uses divB as temp variable
		clrf	xC+0
		clrf	xC+1
        MOVF    xB+0,W       	; Check for zero
        IORWF   xB+1,W     		; 
        BTFSC   STATUS,Z        ; Check for zero
        RETLW   H'FF'           ; return 0xFF if illegal
        MOVLW   1               ; Start count at 1
        MOVWF   divB	       ; Clear Count
div16x16_1:
    	BTFSC   xB+1,7     		; High bit set ?
        bra	    div16x16_2      ; Yes then continue
        INCF    divB,F     		; Increment count

		bcf		STATUS,C
		rlcf	xB+0,F
		rlcf	xB+1,F
        bra	    div16x16_1
div16x16_2:
								; Shift result left
		bcf		STATUS,C
		rlcf	xC+0,F
		rlcf	xC+1,F

 			; Reduce Divisor		

        MOVF    xB,W         ; Get low byte of subtrahend
        SUBWF   xA,F         ; Subtract DST(low) - SRC(low)
        MOVF    xB+1,W       ; Now get high byte of subtrahend
        BTFSS   STATUS,C     ; If there was a borrow, rather than
        INCF    xB+1,W       ; decrement high byte of dst we inc src
        SUBWF   xA+1,F       ; Subtract the high byte and we're done


        BTFSC   STATUS, C       ; Did it reduce?        
        bra	    div16x16_3      ; No, so it was less than

		movf	xB+0,W			; Reverse subtraction
		addwf	xA+0,F
		movf	xB+1,W
		addwfc	xA+1,F

        bra	    div16x16_4      ; Continue the process
div16x16_3:
	     BSF     xC+0,0        	; Yes it did, this gets a 1 bit
div16x16_4:
	     DECF    divB,F 		    ; Decrement N_COUNT
        BTFSC   STATUS,Z        ; If its not zero then continue
        return

		bcf		STATUS,C
		rrcf	xB+1,F
		rrcf	xB+0,F

        bra    div16x16_2              ; Next bit.

	global	div32x16
div32x16:  ; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder
	; Setup
	movlw		.32		; setup shift counter
	movwf		divB
	movff		xC+3,xA+1		; move ACCb to ACCf
	movff		xC+2,xA+0
	movff		xC+1,sub_a+1	; move ACCc to ACCe
	movff		xC+0,sub_a+0
	clrf		xC+3
	clrf		xC+2
	clrf		xC+1
	clrf		xC+0
	clrf		sub_b+1
	clrf		sub_b+0
div32x16_2:
	bcf			STATUS,C
	rlcf		sub_a+0,F
	rlcf		sub_a+1,F
	rlcf		xA+0,F
	rlcf		xA+1,F
	rlcf		sub_b+0,F
	rlcf		sub_b+1,F
	movf		xB+1,W
	subwf		sub_b+1,W	; check if a>d
	btfss		STATUS,Z
	bra			div32x16_3
	movf		xB+0,W
	subwf		sub_b+0,W	; if msb equal then check lsb
div32x16_3:
	btfss		STATUS,C	; carry set if d>a
	bra			div32x16_4
	movf		xB+0,W		; d-a into d
	subwf		sub_b+0,F
	btfss		STATUS,C
	decf		sub_b+1,F
	movf		xB+1,W
	subwf		sub_b+1,F
	bsf			STATUS,C	; shift a 1 into b (result)
div32x16_4:
	rlcf		xC+0,F
	rlcf		xC+1,F
	rlcf		xC+2,F
	rlcf		xC+3,F
	decfsz		divB,F	; loop until all bits checked
	bra			div32x16_2
	return

;;=============================================================================
;; u16 * u16 --> 32bit multiply (xA * xB --> xC)
;; Used in interupt service routines, to compute temperature and pressure.
;;
;	global	isr_mult16x16
;isr_mult16x16:
;	clrf    isr_xC+2        	  ;  Clear the High-Order Bits
;	clrf    isr_xC+3
;	movf    isr_xA, w               ;  Do the "L" Multiplication first
;	mulwf   isr_xB
;	movf    PRODL, w            ;  Save result
;	movwf   isr_xC+0
;	movf    PRODH, w
;	movwf   isr_xC+1
;	movf    isr_xA+0, w               ;  Do the "I" Multiplication
;	mulwf   isr_xB+1
;	movf    PRODL, w            ;  Save the Most Significant Byte First
;	addwf   isr_xC+1, f
;	movf    PRODH, w
;	addwfc  isr_xC+2, f    	 		 ;  Add to the Last Result
;	movf    isr_xA+1, w               ;  Do the "O" Multiplication
;	mulwf   isr_xB
;	movf    PRODL, w            ;  Add the Lower Byte Next
;	addwf   isr_xC+1, f
;	movf    PRODH, w            ;  Add the High Byte First
;	addwfc  isr_xC+2, f
;	btfsc   STATUS, C           ;  Add the Carry
;	incf    isr_xC+3, f
;	movf    isr_xA+1, w               ;  Do the "F" Multiplication
;	mulwf   isr_xB+1
;	movf    PRODL, w
;	addwf   isr_xC+2, f
;	movf    PRODH, w
;	addwfc  isr_xC+3, f
;	return

;=============================================================================
; 24bit shift, repeted WREG times.
; Because we shift less than 8bits, and keep only C[2:1], we don't care what
; bit is inserted...
;
	global	isr_shift_C31
isr_shift_C31:
	rrcf    isr_xC+3,F                  ; Shift the three bytes...
	rrcf    isr_xC+2,F
	rrcf    isr_xC+1,F
	decfsz  WREG
	bra     isr_shift_C31
	return

;=============================================================================
; s16 * s16 --> 32bit multiply (xA * xB --> xC)
; Signed multiplication.
; Code from... the Pic18F documentation ;-)
	global	isr_unsigned_mult16x16
isr_unsigned_mult16x16:
        MOVF    isr_xA+0, W             ; Lowest is simply a[0] * b[0]
        MULWF   isr_xB+0
        MOVFF   PRODL, isr_xC+0
        MOVFF   PRODH, isr_xC+1
        ;
        MOVF    isr_xA+1, W             ; And highest a[1] * b[1]
        MULWF   isr_xB+1
        MOVFF   PRODL, isr_xC+2
        MOVFF   PRODH, isr_xC+3
        ;
        MOVF    isr_xA+0, W             ; Intermediates do propagate:
        MULWF   isr_xB+1
        MOVF    PRODL, W
        ADDWF   isr_xC+1, F             ; Add cross products
        MOVF    PRODH, W
        ADDWFC  isr_xC+2, F             ; with propagated carry
        CLRF    WREG
        ADDWFC  isr_xC+3, F             ; on the three bytes.
        ;
        MOVF    isr_xA+1, W             ; And the second one, similarly.
        MULWF   isr_xB+0
        MOVF    PRODL, W
        ADDWF   isr_xC+1, F             ; Add cross products
        MOVF    PRODH, W
        ADDWFC  isr_xC+2, F
        CLRF    WREG
        ADDWFC  isr_xC+3, F
        return

	global	isr_signed_mult16x16
isr_signed_mult16x16:
        rcall   isr_unsigned_mult16x16

        ; Manage sign extension of operand B
        BTFSS   isr_xB+1,7              ; Is B negatif ?
        BRA     isr_signed_mult_checkA  ; No: check ARG1
        MOVF    isr_xA+0, W             ; Yes: add -65536 * A
        SUBWF   isr_xC+2, F
        MOVF    isr_xA+1, W
        SUBWFB  isr_xC+3, F
        ; And of operand A
isr_signed_mult_checkA
        BTFSS   isr_xA+1, 7             ; Is A negatif ?
        RETURN                          ; No: done
        MOVF    isr_xB+0, W
        SUBWF   isr_xC+2, F
        MOVF    isr_xB+1, W
        SUBWFB  isr_xC+3, F
        RETURN

 END