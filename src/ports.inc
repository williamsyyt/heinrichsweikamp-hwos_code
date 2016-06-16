;=============================================================================
;
;   File ports.inc
;
;   Portmap
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2012-08-13 : [mH] Creation

;----------------------------- PORTS ---------------------------------

; PORTA: TFT_HIGH
; TRIS=b'00000000'

; PORTB
#DEFINE	switch_left1	PORTB,1	; Switch
#DEFINE	switch_right2	PORTB,0	; Switch
#DEFINE mcp_power		PORTB,2	; RX Power supply
#DEFINE	s8_npower		PORTB,3	; Powers S8 Bulkhead (inverted)
#DEFINE	LEDg			PORTB,4	; LED green
#DEFINE	tft_power		PORTB,5 ; via P-MOSFET (Inverted)
#DEFINE	icsp_clk		PORTB,6	; ICSP
#DEFINE	icsp_dat		PORTB,7	; ICSP
; TRIS=b'00001011'

; PORTC
#DEFINE	SOSC_RC0		PORTC,0	; SOSC
#DEFINE	SOSC_RC1		PORTC,1	; SOSC
#DEFINE	TFT_PWM			PORTC,2	; TFT Backlight
#DEFINE	I2C_RC3			PORTC,3	; I²C
#DEFINE	I2C_RC4			PORTC,4	; I²C
#DEFINE	MS5541_mosi		PORTC,5	; MS5541
#DEFINE	uart1_RC6		PORTC,6	; UART1 (USB)
#DEFINE	uart1_RC7		PORTC,7	; UART1 (USB)
; TRIS=b'10011010'

; PORTD
#DEFINE	tft_cs			PORTD,0	; /CS
#DEFINE	tft_rs			PORTD,1	; RS
#DEFINE	tft_nwr			PORTD,2	; /WR
#DEFINE	tft_rd			PORTD,3 ; RD
#DEFINE	flash_mosi		PORTD,4	; MOSI
#DEFINE	flash_miso		PORTD,5	; MISO
#DEFINE	flash_clk		PORTD,6	; CLK
#DEFINE	tft_nreset		PORTD,7	; /RESET
; TRIS=b'00100000'

; PORTE
#DEFINE	RE0_unused		PORTE,0	; unused
#DEFINE	ir_power		PORTE,1	; Power supply for IR
#DEFINE	charge_disable		PORTE,2	; ex. mcp_ncs (RX /CS) (Available from hardware rev x.x only)
#DEFINE	LEDr			PORTE,3	; LED red
#DEFINE	power_sw2		PORTE,4	; Power supply for switch1 circuit
#DEFINE	RE5_unused		PORTE,5	; unused
#DEFINE	lightsen_power		PORTE,6	; Power supply for lightsensor
#DEFINE	flash_ncs		PORTE,7	; /CS
; TRIS=b'00000000'

; PORTF
; RF1 (AN6, Batt_analog) and RF2 (AN7, Lightsensor)
; TRIS=b'01111110'
#DEFINE NRTS            PORTF,6 ; I
#DEFINE NCTS            PORTF,7 ; 0

; PORTG  
#DEFINE TX3_PIEZO_CFG   PORTG,0
#DEFINE	RG1_blocked_by_RS232_2	PORTG,1	; unused
#DEFINE	tsop_rx			PORTG,2	; IR (RX2)
; RG3 (AN17, RSSI RX)
; RG4 (32768Hz Clock out)
#DEFINE	RG5_unused		PORTG,5	; /MCLR
#DEFINE	RG6_unused		PORTG,6	; unavailable
#DEFINE	RG7_unused		PORTG,7	; unavailable
; TRIS=b'10001111'

; PORTJ
#DEFINE	RJ0_unused		PORTJ,0	; unused
#DEFINE	RJ1_unused		PORTJ,1	; unused
#DEFINE	CHRG_OUT		PORTJ,2	; CHRG_OUT
#DEFINE	CHRG_IN 		PORTJ,3	; CHRG_IN
#DEFINE	vusb_in			PORTJ,4	; external power supply detect, USB enumerated
#DEFINE	power_sw1		PORTJ,5	; Power supply for switch2 circuit
#DEFINE	MS5541_clk		PORTJ,6	; MS5541 mH
#DEFINE	MS5541_miso		PORTJ,7	; MS5541 mH
; TRIS=b'10010000'

