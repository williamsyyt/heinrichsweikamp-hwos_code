;=============================================================================
;
;   File ostc3.asm
;
;   Definition of the ostc3 dive computer platform.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-05-24 : [jDG] Cleanups from initial Matthias code.
;   2011-06-24 : [MH]  Added clock speeds.
#include "ostc3.inc"

;=============================================================================
;----------------------------- CONFIG ---------------------------------
    CONFIG	RETEN = OFF          ;Disabled - Controlled by SRETEN bit
    CONFIG	SOSCSEL = HIGH       ;High Power SOSC circuit selected
    CONFIG  XINST = OFF          ;Code won't excute in extended mode...
    CONFIG	FOSC = INTIO2        ;Internal RC oscillator, no clock-out
    CONFIG	PLLCFG = OFF
    CONFIG	IESO = OFF           ;Disabled
    CONFIG	PWRTEN = OFF         ;Disabled, because incompatible with ICD3 (Ri-400)
    CONFIG	BOREN = ON           ;Controlled with SBOREN bit
    CONFIG	BORV = 2             ;2.0V
    CONFIG	BORPWR = MEDIUM      ;BORMV set to medium power level
    CONFIG	WDTEN = ON           ;WDT controlled by SWDTEN bit setting
    CONFIG	WDTPS = 128          ;1:128
    CONFIG	RTCOSC = SOSCREF     ;RTCC uses SOSC
    CONFIG	MCLRE = ON           ;MCLR Enabled, RG5 Disabled
    CONFIG	CCP2MX = PORTBE      ;RE7-Microcontroller Mode/RB3-All other modes
;=============================================================================
boot    CODE
    global init_ostc3

init_ostc3:
	banksel common              ; Bank1
;init oscillator
	movlw	b'01110010'
	movwf	OSCCON				; 16MHz INTOSC
	movlw	b'00001000'
	movwf	OSCCON2				; Secondary Oscillator running
	movlw	b'00000000'
	movwf	OSCTUNE				; 4x PLL Disable (Bit6) - only works with 8 or 16MHz (=32 or 64MHz)
	bcf		RCON,SBOREN			; Bown-Out off
	bcf		RCON,IPEN			; Priority Interrupts off
    clrf    CM1CON              ; Disable
    banksel WDTCON
    movlw   b'10000000'
    movwf   WDTCON              ; Setup Watchdog 

; I/O Ports
	banksel 0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.

	clrf	REFOCON				; No reference oscillator active on REFO pin
	clrf	ODCON1				; Disable Open Drain capability
	clrf	ODCON2				; Disable Open Drain capability
	clrf	ODCON3				; Disable Open Drain capability
    clrf    CM2CON              ; Disable
    clrf    CM3CON              ; Disable

	movlw	b'11000000'			; ANSEL, AN7 and AN6 -> Analog inputs, PORTA is digital.
	movwf	ANCON0
	movlw	b'00000111'			; ANSEL, AN8, AN9, AN10 -> Analog in
	movwf	ANCON1
	movlw	b'00000010'			; ANSEL, AN17 -> Analog input
	movwf	ANCON2

	banksel common

	movlw	b'00000000'			; 1= Input -> Data TFT_high
	movwf	TRISA
	movlw	b'00000000'			; Init port
	movwf	PORTA

	movlw	b'00000011'			; 1= Input, (RB0, RB1) -> Switches, RB2 -> Power_MCP, RB3 -> s8_npower, RB4 -> LED_green, RB5 -> /TFT_POWER
	movwf	TRISB
	movlw	b'00101000'			; Init port
	movwf	PORTB

    movlw	b'10011010'			; 1= Input, (RC0, RC1) -> SOSC, RC2 -> TFT_LED_PWM, (RC3,RC4) -> I²C, RC5 -> MOSI_MS5541, (RC6, RC7) -> UART1
	movwf	TRISC
	movlw	b'00000000'			; Init port
	movwf	PORTC

	movlw	b'00100000'			; 1= Input, RD0 -> TFT_NCS, RD1 -> TFT_RS, RD2 -> TFT_NWR, RD3 -> TFT_RD, RD4 -> MOSI_Flash, RD5 -> MISO_Flash, RD6 -> CLK_Flash, RD7 -> TFT_NRESET
	movwf	TRISD
	movlw	b'00000000'			; Init port
	movwf	PORTD

	movlw	b'00000000'			; 1= Input, RE1 -> Power_IR, RE2 -> CS_MCP, RE3 -> LED_blue, RE4 -> power_sw1, RE5 -> Set to 1 for cR hardware
	movwf	TRISE
	movlw	b'00110001'			; Init port
	movwf	PORTE

	movlw	b'01111110'			; 1= Input, (RF1, RF2, RF3, RF4, RF5) -> Analog
	movwf	TRISF
	movlw	b'00000000'			; Init port
	movwf	PORTF

	movlw	b'00001110'			; 1= Input, <7:6> not implemented, RG0 -> TX3_PIEZO_CFG, RG2 -> RX2, RG3 -> AN17_RSSI, RG4 -> SOSC_OUT, RG5 -> /RESET
	movwf	TRISG
	movlw	b'00000001'			; Init port
	movwf	PORTG

	movlw	b'00000000'			; 1= Input -> Data TFT_low
	movwf	TRISH
	movlw	b'00000000'			; Init port
	movwf	PORTH

	movlw	b'10011011'			; 1= Input, RJ4 -> vusb_in, RJ5 -> power_sw2,  RJ6 -> CLK_MS5541, RJ7 -> MISO_MS5541
	movwf	TRISJ
	movlw	b'00100000'			; Init port
	movwf	PORTJ


; Timer 0
	movlw	b'00000001'				; Timer0 with 1:4 prescaler
;	movlw	b'00001000'				; Timer0 with 1:1 prescaler
	movwf	T0CON

; Timer 1 - Button hold-down timer
	movlw	b'10001100'             ; 32768Hz clock source, 1:1 Prescaler -> ; 30,51757813µs/bit in TMR1L:TMR1H
	movwf	T1CON

	banksel 0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.

; RTCC
	movlw 	0x55
	movwf 	EECON2
	movlw 	0xAA
	movwf 	EECON2
	bsf 	RTCCFG,RTCWREN		; Unlock sequence for RTCWREN
	bsf		RTCCFG,RTCPTR1
	bsf		RTCCFG,RTCPTR0
	bsf		RTCCFG,RTCEN		; Module enable
	bsf		RTCCFG,RTCOE		; Output enable
	movlw	b'00000100'			; 32768Hz SOCS on RTCC pin (PORTG,4) Bit7-5: Pullups for Port D, E and J
	movwf	PADCFG1
	movlw	b'11000100'
	movwf	ALRMCFG				; 1 second alarm
	movlw	d'1'
	movwf	ALRMRPT				; Alarm repeat counter
	movlw 	0x55
	movwf 	EECON2
	movlw 	0xAA
	movwf 	EECON2
	bcf 	RTCCFG,RTCWREN		; Lock sequence for RTCWREN

	banksel common
; A/D Converter
	movlw	b'00011000'			; power off ADC, select AN6
	movwf	ADCON0
	movlw	b'00100000'			; 2.048V Vref+
	movwf	ADCON1
	movlw	b'10001101'			; Right justified
	movwf	ADCON2


;init serial port1 (TRISC6/7)
	movlw	b'00001000'			; BRG16=1
	movwf	BAUDCON1
;	movlw 	b'00100100'			; BRGH=1, SYNC=0
;	movwf 	TXSTA1
	movlw 	.34					; SPBRGH:SPBRG = .34  : 114285 BAUD @ 16MHz (+0,79% Error to 115200 BAUD)
	movwf 	SPBRG1				; SPBRGH:SPBRG = .207 :  19230 BAUD @ 16MHz (-0,16% Error to 19200 BAUD)
	clrf	SPBRGH1				;
;	movlw 	b'10010000'
;	movwf 	RCSTA1

	clrf	RCSTA1
	clrf	TXSTA1					; UART disable
    bcf     PORTC,6                 ; TX hard to GND

;init serial port2 (TRISG2)
    banksel BAUDCON2
	movlw	b'00100000'			; BRG16=0           ; inverted for IR
	movwf	BAUDCON2
	movlw 	b'00100000'			; BRGH=0, SYNC=0
	movwf 	TXSTA2
	movlw 	.102                ; SPBRGH:SPBRG = .102  : 2403 BAUD @ 16MHz
	movwf 	SPBRG2
	clrf	SPBRGH2
	movlw 	b'10010000'
	movwf 	RCSTA2
    banksel common

; Timer3 for IR-RX Timeout
	clrf	T3GCON				; Reset Timer3 Gate Control register
;	movlw	b'10001101'			; 1:1 Prescaler -> 2seconds@32768Hz, not synced
	movlw	b'10001001'			; 1:1 Prescaler -> 2seconds@32768Hz, synced
; 30,51757813µs/bit in TMR3L:TMR3H
	movwf	T3CON

; SPI Module(s)
; SPI2: External Flash
	movlw	b'00110000'
	movwf	SSP2CON1
	movlw	b'00000000'
	movwf	SSP2STAT
; ->0,25MHz Bit clock  @1MHz mode (Eco)
; ->  4MHz  Bit clock @16MHz mode (Normal)
; -> 16MHz  Bit clock @64MHz mode (Fastest)

; MSSP1 Module: I2C Master
    movlw	b'00101000'		; I2C Master Mode
	movwf	SSP1CON1
	movlw	b'00000000'
	movwf	SSP1CON2
	movlw	0x27
	movwf	SSP1ADD			; 100kHz @ 16MHz Fosc

; PWM Module(s)
; PWM1 for LED dimming
	movlw	b'00001100'
	movwf	CCP1CON
	movlw	b'00000001'
	movwf	PSTR1CON			; Pulse steering disabled
	movlw	d'255'
	movwf	PR2					; Period
	; 255 is max brightness (300mW)
	clrf	CCPR1L				; Duty cycle
	clrf	CCPR1H				; Duty cycle
	movlw	T2CON_NORMAL
	movwf	T2CON

; Timer5 for ISR-independent wait routines
	clrf	T5GCON				; Reset Timer5 Gate Control register
;	movlw	b'10001101'			; 1:1 Prescaler -> 2seconds@32768Hz, not synced
	movlw	b'10001001'			; 1:1 Prescaler -> 2seconds@32768Hz, synced
; 30,51757813µs/bit in TMR5L:TMR5H
	movwf	T5CON

; Timer7 for 62,5ms Interrupt (Sensor states)
	banksel 0xF16				; Addresses, F16h through F5Fh, are also used by SFRs, but are not part of the Access RAM.
	clrf	T7GCON				; Reset Timer7 Gate Control register
;	movlw	b'10001101'			; 1:1 Prescaler -> 2seconds@32768Hz, not synced
	movlw	b'10001001'			; 1:1 Prescaler -> 2seconds@32768Hz, synced
	movwf	T7CON
	clrf	TMR7L
	movlw	.248
	movwf	TMR7H				; -> Rollover after 2048 cycles -> 62,5ms

	banksel common
; Interrupts
	movlw	b'11010000'
	movwf	INTCON
	movlw	b'00001000'			; BIT7=1: Pullup for PORTB disabled
	movwf	INTCON2
	movlw	b'00000000'
	movwf	INTCON3
	movlw	b'00000001'			; Bit0: TMR1
	movwf	PIE1
	movlw	b'00000010'			; Bit1: TMR3
	movwf	PIE2			
	movlw	b'00000000'			; Bit1: TMR5
	movwf	PIE5
	movlw	b'00100001'			; Bit0: RTCC, Bit5: UART2
	movwf	PIE3
	movlw	b'00001000'			; Bit3: TMR7
	movwf	PIE5

    bsf     power_sw1
    btfss   power_sw1
    bra     $-4
    bsf     power_sw2
    btfss   power_sw2
    bra     $-4

	movlw	d'2'
	movwf	speed_setting		; Normal


	return

;=============================================================================
	global	speed_eco
speed_eco:
	movlw	d'1'
	movff	WREG,speed_setting		; Bank-independent
	; Done in ISR
	return
;=============================================================================
	global	speed_normal
speed_normal:
	movlw	d'2'
	movff	WREG,speed_setting		; Bank-independent
	; Done in ISR
	return
;=============================================================================
	global	speed_fastest
speed_fastest:
	movlw	d'3'
	movff	WREG,speed_setting		; Bank-independent
	; Done in ISR
	return
;=============================================================================

    END