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

mcp_writebyte_1st macro char
	movlw 		char
	rcall		mcp_write_one_byte
	endm

mcp_writebyte_2nd macro char
	movlw 		char
	rcall		mcp_write_one_byte2
	endm

	; Writes mcp_temp+0 to config reg
mcp_write_config macro	char
	movlw		char
	rcall		mcp_write_config_reg
	endm

mcp code

mcp_write_one_byte:
	movwf		mcp_temp+0				; save one byte
	bcf			TRISG,0					; CLK output
	nop
	bcf			mcp_clk					; clk=0
	bsf			mcp_ncs					; cs=1
	nop
	bcf			mcp_ncs					; cs=0
	bcf			TRISB,3					; mcp_lf_data output
	movlw		.8
	movwf		mcp_temp+1				; Bit counter
mcp_write_one_byte_loop:
	btfss		mcp_temp+0,7
	bcf			mcp_lf_data
	btfsc		mcp_temp+0,7
	bsf			mcp_lf_data
	bsf			mcp_clk					; clk=1
	rlncf		mcp_temp+0,F			; shift byte left no carry
	bcf			mcp_clk					; clk=0
	decfsz		mcp_temp+1,F			; 8Bit done?
	bra			mcp_write_one_byte_loop	; Not yet...
	return

mcp_write_one_byte2:
	movwf		mcp_temp+0				; save one byte
	movlw		.8
	movwf		mcp_temp+1				; Bit counter
mcp_write_one_byte_loop2:
	btfss		mcp_temp+0,7
	bcf			mcp_lf_data
	btfsc		mcp_temp+0,7
	bsf			mcp_lf_data
	bsf			mcp_clk					; clk=1
	rlncf		mcp_temp+0,F			; shift byte left no carry
	bcf			mcp_clk					; clk=0
	decfsz		mcp_temp+1,F			; 8Bit done?
	bra			mcp_write_one_byte_loop2; Not yet...
	bsf			TRISB,3					; mcp_lf_data input again
	bsf			mcp_ncs					; cs=1
	bsf			TRISG,0					; CLK input
	return

mcp_readbyte:
	bcf			TRISG,0					; CLK output
	nop
	bcf			mcp_clk					; clk=0
	bcf			mcp_ncs					; cs=0
	movlw		.7
	movwf		mcp_temp+1				; Bit counter
	nop
	nop
mcp_readloop:
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0
	decfsz		mcp_temp+1,F			; 7 Bit done?
	bra			mcp_readloop			; Not yet...

	movlw		.8
	movwf		mcp_temp+1				; Bit counter
mcp_readloop2:
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0
	btfss		mcp_lf_data
	bcf			mcp_temp+0,7
	btfsc		mcp_lf_data
	bsf			mcp_temp+0,7
	rlncf		mcp_temp+0				; MSB first
	decfsz		mcp_temp+1,F			; 8 Bit done?
	bra			mcp_readloop2			; Not yet...

; Dummy clk for parity bit
	bsf			mcp_clk					; clk=1
	nop
	bcf			mcp_clk					; clk=0
	bsf			mcp_ncs					; cs=1
	bsf			TRISG,0					; CLK input
	return

mcp_write_config_reg:					; Writes mcp_temp+0 to config #WREG
	movwf		mcp_temp+2				; Save config#
	bcf			TRISG,0					; CLK output
	clrf		mcp_temp+3				; for parity
	bcf			mcp_clk					; clk=0
	bsf			mcp_ncs					; cs=1
	nop
	bcf			mcp_ncs					; cs=0
	bcf			TRISB,3					; mcp_lf_data output
	bsf			mcp_lf_data
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0			; Write command done.

	; Now, 4Bit register address
	movlw		.4
	movwf		mcp_temp+1				; Bit counter
mcp_write_config_reg1:
	btfss		mcp_temp+2,3
	bcf			mcp_lf_data
	btfsc		mcp_temp+2,3
	bsf			mcp_lf_data
	bsf			mcp_clk					; clk=1
	rlncf		mcp_temp+2,F			; shift byte left no carry
	bcf			mcp_clk					; clk=0
	decfsz		mcp_temp+1,F			; 4Bit done?
	bra			mcp_write_config_reg1		; Not yet...

	; 8Bit data
	movlw		.8
	movwf		mcp_temp+1				; Bit counter
mcp_write_config_reg2:
	btfss		mcp_temp+0,7
	bcf			mcp_lf_data
	btfsc		mcp_temp+0,7
	bsf			mcp_lf_data
	btfsc		mcp_temp+0,7
	incf		mcp_temp+3,F			; count 1's...
	bsf			mcp_clk					; clk=1
	rlncf		mcp_temp+0,F			; shift byte left no carry
	bcf			mcp_clk					; clk=0
	decfsz		mcp_temp+1,F			; 8Bit done?
	bra			mcp_write_config_reg2		; Not yet...

	; 1bit parity
	btfss		mcp_temp+3,0
	bsf			mcp_lf_data				; Set row parity bit
	btfsc		mcp_temp+3,0
	bcf			mcp_lf_data				; clear row parity bit
	bsf			mcp_clk					; clk=1
	bcf			mcp_clk					; clk=0			; Parity bit done.

	bsf			TRISB,3					; mcp_lf_data input again
	bsf			mcp_ncs					; cs=1
	bsf			TRISG,0					; CLK input
	return

    global  mcp_reset
mcp_reset:                              ; reset RX chip# (Normal mode)
; Make sure row parity bit is correct
; yyyaaaa01234567P
; yyy: Command
; aaaa: Address
; 0-7: Data
; P: Parity bit. Set/Clear that 0-7+P are odd number of ones
; Current config:
	banksel	buffer
	movlw	b'10100100'			; Config0: LCZ disabled, Wakeup => High = 2ms, Low = 2ms
	movwf	buffer+0
    movlw	b'00000000'			; Config1: +20pF LCX Normal mode
;    movlw	b'01000000'			; Config1: +20pF LCX carrier out mode
	movwf	buffer+1
 	movlw	b'00000000'         ; Config2: +25pF LCY
	movwf	buffer+2
	movlw	b'00000000'			; Config3
	movwf	buffer+3
	movlw	b'00000000'			; Config4
	movwf	buffer+4
;	movlw	b'00001111'			; Config5 33%
;	movlw	b'00101111'			; Config5 14%
	movlw	b'10011111'			; Config5 60%
	movwf	buffer+5
    bra     mcp_reset_common

    global  mcp_reset_rssi
mcp_reset_rssi:							; reset RX chip# for RSSI mode
; Make sure row parity bit is correct
; yyyaaaa01234567P
; yyy: Command
; aaaa: Address
; 0-7: Data
; P: Parity bit. Set/Clear that 0-7+P are odd number of ones
; Current config:
	banksel	buffer
	movlw	b'10101000'			; Config0: LCZ disabled, Wakeup => High = 2ms, Low = 2ms
	movwf	buffer+0
    movlw	b'10000000'			; Config1: +20pF LCX and RSSI Mode
	movwf	buffer+1
	movlw	b'00000000'			; Config2: +25pF LCY
	movwf	buffer+2
	movlw	b'00000000'			; Config3
	movwf	buffer+3
	movlw	b'00000000'			; Config4
	movwf	buffer+4
	movlw	b'11010000'			; Config5 60%
	movwf	buffer+5
mcp_reset_common:
    banksel TRISB
    bcf     TRISB,2
    bcf     mcp_ncs             ; CS=1
    nop
    bsf     mcp_power           ; Power-up
    nop
    btfss   mcp_power
    bra     mcp_reset_common
    WAITMS .10
; Compute column parity byte
    banksel buffer
	movf	buffer+0,W
	xorwf	buffer+1,W
	xorwf	buffer+2,W
	xorwf	buffer+3,W
	xorwf	buffer+4,W
	xorwf	buffer+5,W
	xorlw	0xFF
	movwf	buffer+6			; <- Column parity byte
	banksel mcp_temp+0

	mcp_writebyte_1st	b'10100000'	; Reset Command
	mcp_writebyte_2nd	b'00000000'	; Dummy byte

	mcp_writebyte_1st	b'00100000'	; Clamp off
	mcp_writebyte_2nd	b'00000000'	; Dummy byte

	movff	buffer+0,mcp_temp+0		; Data byte
	mcp_write_config	.0
	movff	buffer+1,mcp_temp+0		; Data byte
	mcp_write_config	.1
	movff	buffer+2,mcp_temp+0		; Data byte
	mcp_write_config	.2
	movff	buffer+3,mcp_temp+0		; Data byte
	mcp_write_config	.3
	movff	buffer+4,mcp_temp+0		; Data byte
	mcp_write_config	.4
	movff	buffer+5,mcp_temp+0		; Data byte
	mcp_write_config	.5
	movff	buffer+6,mcp_temp+0		; Data byte (Column parity byte)
	mcp_write_config	.6

;	mcp_writebyte_1st	b'11000000'	; Read from Config0
;	mcp_writebyte_2nd	b'00000000'	; Dummy clks + Odd Parity Bit (Bit0)
;	call	mcp_readbyte			; read into mcp_temp+0
    bsf     INTCON3,INT3IE          ; Enable INT3
    bsf     INTCON2,INTEDG3         ; INT3 on rising edge

    ; Setup Timer 0
    movlw   TMR0H_VALUE
    movwf   TMR0H
	bcf		INTCON,TMR0IF			; Clear flag
	clrf	TMR0L
	return

    global  mcp_sleep
mcp_sleep:
    bcf     INTCON3,INT3IE          ; Disable INT3
    bcf     mcp_power               ; RX off
    btfsc   mcp_power
    bra     $-4
    return



        END
