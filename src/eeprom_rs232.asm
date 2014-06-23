;=============================================================================
;
;   File eeprom_rs232.asm
;
;   Internal EEPROM, RS232
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-06 : [mH] moving from OSTC code

#include "ostc3.inc"
#include "start.inc"
#include "tft.inc"
#include "wait.inc"
#include "strings.inc"
#include "convert.inc"
#include "adc_lightsensor.inc"
#include "math.inc"

;=============================================================================
eeprom   code    0xF00000+0x10
; Skip SERIAL number. Should not be overwritten.
    global eeprom_serial_save, eeprom_opt_backup
eeprom_serial_save  res 2
eeprom_opt_backup   res 0x3E

;=============================================================================
basic    CODE

	global write_int_eeprom_1
write_int_eeprom_1:
	movwf	EEADR
	bra		write_eeprom                ; writes and "returns" after write

	global read_int_eeprom_1
read_int_eeprom_1:
	movwf	EEADR
	bra		read_eeprom					; reads and "returns" after write

;=============================================================================
; reads from internal eeprom
; Input:   EEADRH:EEADR = EEPROM address.
; Output:  EEDATA.
; Trashed: NONE.
	global	read_eeprom
read_eeprom: 							
	bcf		EECON1,EEPGD
	bcf		EECON1,CFGS
	bsf		EECON1,RD
	return

;=============================================================================
; writes into internal eeprom
; Input:   EEADRH:EEADR = EEPROM address.
;          EEDATA = byte to write.
; Trashed: WREG.
	global	write_eeprom
write_eeprom:							
	bcf		EECON1,EEPGD
	bcf		EECON1,CFGS
	bsf		EECON1,WREN

	bcf		INTCON,GIE					; even the RTC will be delayed for the next 5 instructions...
	movlw	0x55		
	movwf	EECON2
	movlw	0xAA
	movwf	EECON2
	bsf		EECON1,WR
	bsf		INTCON,GIE					; ...but the flag for the ISR routines were still set, so they will interrupt now!

write_eep2:
	btfsc	EECON1,WR		
	bra 	write_eep2					; wait about 4ms...
	bcf		EECON1,WREN
	return

    global  disable_ir
disable_ir:
    banksel TXSTA2
	clrf    TXSTA2
	clrf    RCSTA2
    banksel common
    bcf     ir_power                ; IR off
    bcf     mcp_power               ; Power-down intrumentation amp
    bsf     s8_npower               ; Power-down S8 HUD
    return

    global  enable_ir
enable_ir:
;init serial port2 (TRISG2)
    btfsc   c3_hardware
    bra     enable_s8           ; Start S8

    banksel BAUDCON2
	movlw	b'00100000'			; BRG16=0           ; inverted for IR
	movwf	BAUDCON2
    banksel TXSTA2
	movlw 	b'00100000'			; BRGH=0, SYNC=0
	movwf 	TXSTA2
	movlw 	.102                ; SPBRGH:SPBRG = .102  : 2403 BAUD @ 16MHz
	movwf 	SPBRG2
	clrf	SPBRGH2
	movlw 	b'10010000'
	movwf 	RCSTA2
    banksel common
    bsf     ir_power            ; Power-up IR
    btfss   ir_power
    bra     $-6
    return

enable_s8:
    ; Check for Digital/Analog
    bsf     s8_npower           ; Power-down S8 HUD
    WAITMS  d'1'                ; Very short delay
    bsf     mcp_power           ; Power-up intrumentation amp
    btfss   mcp_power
    bra     $-6
    banksel TXSTA2
	clrf    TXSTA2
	clrf    RCSTA2
    banksel common

    ; It may be digital, check for voltage when isolator is powered
    bcf     s8_npower           ; Power S8 HUD
    WAITMS  d'1'                ; Very short delay

    btfsc   PORTG,2             ; RX2=1?
    bra     enable_s8_2         ; Yes, digital
    WAITMS  d'30'
    btfsc   PORTG,2             ; RX2=1?
    bra     enable_s8_2         ; Yes, digital
    
    ; Not found, set to analog (fail-safe)

enable_s8_analog:
    ; S8 Analog
    bsf     s8_npower           ; Power-down S8 HUD
    bcf     s8_digital          ; Clear flag
    return

enable_s8_2:                    ; S8 Digital
    banksel BAUDCON2
    movlw	b'00000000'			; BRG16=0           ; normal for S8
	movwf	BAUDCON2
    banksel TXSTA2
	movlw 	b'00100000'			; BRGH=0, SYNC=0
	movwf 	TXSTA2
    movlw 	.25                 ; SPBRGH:SPBRG = .25   : 9615 BAUD @ 16MHz
	movwf 	SPBRG2
	clrf	SPBRGH2
	movlw 	b'10010000'
	movwf 	RCSTA2
    banksel common
    bcf     s8_npower               ; Power S8 HUD
    bsf     s8_digital              ; Set flag
    return

;=============================================================================
	global	enable_rs232
enable_rs232:
	bcf		TRISC,6					; Output
	bsf		TRISC,7					; Input
	call	speed_normal			; 16MHz
enable_rs232_2:
    movlw	T2CON_NORMAL
    cpfseq  T2CON
    bra     enable_rs232_2          ; Wait until speed is normal
;init serial port1 (TRISC6/7)
	clrf	RCSTA1
	clrf	TXSTA1
	movlw	b'00001000'			; BRG16=1
	movwf	BAUDCON1
	movlw 	b'00100100'			; BRGH=1, SYNC=0
	movwf 	TXSTA1
	movlw 	.34					; SPBRGH:SPBRG =  .34 : 114285 BAUD @ 16MHz (+0,79% Error to 115200 BAUD)
	movwf 	SPBRG1
	clrf	SPBRGH1
	movlw 	b'10010000'
	movwf 	RCSTA1
	return

	global	disable_rs232
disable_rs232:
	clrf	RCSTA1
	clrf	TXSTA1					; UART disable
	bsf		TRISC,6					; Input
	bsf		TRISC,7					; Input
	return

	global	rs232_wait_tx
rs232_wait_tx:
	btfsc	TXSTA1,TRMT			; Transmit Shift Register empty?
	return						; Yes, return!

	btfss	TXSTA1,TRMT			; RS232 Busy?
	bra		rs232_wait_tx		; yes, wait...
	return						; Done.

    global  rs232_wait_tx2
rs232_wait_tx2:
    banksel TXSTA2
	btfsc	TXSTA2,TRMT			; Transmit Shift Register empty?
    bra     rs232_wait_tx2_2    ; Yes, return!

	btfss	TXSTA2,TRMT			; RS232 Busy?
	bra		rs232_wait_tx2		; yes, wait...
rs232_wait_tx2_2:
    banksel common
	return						; Done.

	global	rs232_get_byte
rs232_get_byte:
 	bcf		PIR1,RCIF		; clear flag
	bcf		rs232_recieve_overflow		; clear flag
	clrf 	uart1_temp
	clrf 	uart2_temp
rs232_get_byte2:
	btfsc 	PIR1,RCIF		; data arrived?
    return
;	bra     rs232_get_byte3

	decfsz 	uart2_temp,F
	bra 	rs232_get_byte2
	decfsz 	uart1_temp,F
	bra		rs232_get_byte2
						; timeout occoured (about 20ms)
	bsf		rs232_recieve_overflow		; set flag
;rs232_get_byte3:
	bcf		RCSTA1,CREN		; Clear receiver status
	bsf		RCSTA1,CREN
	return				; and return anyway

	END