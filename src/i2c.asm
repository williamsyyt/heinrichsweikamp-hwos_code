;=============================================================================
;
;   File i2c.asm
;
;   I2C Interface to HMC5883L and MMA8452Q
;
;   HMC5883L's read address (8-Bit):    0x3D
;   HMC5883L's write address (8-Bit):   0x3C
;
;   MMA8452Q's read address (8-Bit):    0x39
;   MMA8452Q's write address (8-Bit):   0x38
;
;   Copyright (c) 2012, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2012-08-22 : [mH] Creation


#include "ostc3.inc"                ; Mandatory header
#include "wait.inc"

i2c    CODE

WaitMSSP:
	decfsz		i2c_temp,F          ; check for timeout during I2C action
	bra			WaitMSSP2
	bra			I2CFail             ; timeout occured
WaitMSSP2:
	btfss		PIR1,SSPIF
	bra			WaitMSSP
	clrf		i2c_temp
	bcf			PIR1,SSPIF
	nop
	return

I2C_WaitforACK:
	btfss		SSPCON2,ACKSTAT     ; checks for ACK bit from slave
    return
I2CFail:
	rcall		I2CReset            ; I2C Reset
	bcf			PIR1,SSPIF
	clrf		i2c_temp
	return

I2CReset:                           ; Something went wrong (Slave holds SDA low?)
	clrf		SSP1CON1            ; wake-up slave and reset entire module
	clrf		SSP1CON2
	clrf		SSP1STAT
	bcf			TRISC,3             ; SCL OUTPUT
	bsf			TRISC,4             ; SDA Input
	bcf			PORTC,3
	movlw		d'9'
	movwf		i2c_temp            ; clock-out 9 clock cycles manually
I2CReset_1:
	bsf			PORTC,3             ; SCL=1
	nop
	nop
	nop
	nop
	btfsc		PORTC,4             ; SDA=1?
	bra			I2CReset_2          ; =1, SDA has been released from slave
	bcf			PORTC,3             ; SCL=0
	nop
	nop
	bcf			PORTC,3
	nop
	nop
	decfsz		i2c_temp,F
	bra         I2CReset_1          ; check for nine clock cycles
I2CReset_2:
	bsf			TRISC,3             ; SCL Input
	clrf		SSP1CON1            ; setup I²C Mode
	WAITMS		d'10'               ; Reset-Timeout for I2C devices
	movlw		b'00000000'         ; with slew rate control
	movwf		SSPSTAT
	movlw		b'00101000'
	movwf		SSP1CON1
	movlw		b'00000000'
	movwf		SSP1CON2
    movlw       0x27
	movwf		SSP1ADD
	return

I2C_TX:
	movwf		SSP1BUF
	rcall		WaitMSSP
	bra 		I2C_WaitforACK      ; Returns...

I2C_TwoBytesRX_div16:       ; Get two bytes and devide lo:hi/16 (signed)
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,hi          ; Data Byte
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,lo          ; Data Byte
I2C_TwoBytesRX_div16_2:     ; devide lo:hi/16 (signed) only
    bcf			STATUS,C
    btfsc       hi,7        ; Copy sign bit to carry
    bsf         STATUS,C
    rrcf		hi          ; /2
	rrcf		lo
    bcf			STATUS,C
    btfsc       hi,7        ; Copy sign bit to carry
    bsf         STATUS,C
    rrcf		hi          ; /4
	rrcf		lo
    bcf			STATUS,C
    btfsc       hi,7        ; Copy sign bit to carry
    bsf         STATUS,C
    rrcf		hi          ; /8
	rrcf		lo
    bcf			STATUS,C
    btfsc       hi,7        ; Copy sign bit to carry
    bsf         STATUS,C
    rrcf		hi          ; /16
	rrcf		lo
    return

    global  I2C_RX_accelerometer
I2C_RX_accelerometer:
   	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x38                ; address
    rcall       I2C_TX
	movlw		0x01
    rcall       I2C_TX
	bsf			SSP1CON2,RSEN		; Repeated start condition (!)
	rcall		WaitMSSP
	movlw		0x39                ; address
    rcall       I2C_TX

    ; Chip orientation on the PCB requires
    ; Original = Corrected
    ; x = -x
    ; y = -y
    ; z = -z


    rcall       I2C_TwoBytesRX_div16 ; Get two bytes and devide /16 (signed)
    comf        hi                    ; 16bit sign change.
    negf        lo
    btfsc       STATUS,C            ; Carry to propagate ?
    incf        hi,F                ; YES: do it.
    movff       lo,accel_DX+0
    movff       hi,accel_DX+1       ; Copy result

    rcall       I2C_TwoBytesRX_div16 ; Get two bytes and devide /16 (signed)
    comf        hi                    ; 16bit sign change.
    negf        lo
    btfsc       STATUS,C            ; Carry to propagate ?
    incf        hi,F                ; YES: do it.
    movff       lo,accel_DY+0
    movff       hi,accel_DY+1       ; Copy result

    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,hi          ; Data Byte
	bsf			SSP1CON2, RCEN      ; Enable recieve mode
    rcall		WaitMSSP
; According to datasheet there should be no Master Acknowlegde for the last Byte (accel_DZ+0)...
	movff		SSP1BUF,lo          ; Data Byte

    rcall       I2C_TwoBytesRX_div16_2; devide lo:hi/16 (signed) only
    comf        hi                    ; 16bit sign change.
    negf        lo
    btfsc       STATUS,C            ; Carry to propagate ?
    incf        hi,F                ; YES: do it.
    movff       lo,accel_DZ+0
    movff       hi,accel_DZ+1       ; Copy result

	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP
    return

I2C_OneByteRX:
	bsf			SSP1CON2, RCEN      ; Enable recieve mode
	rcall		WaitMSSP
	bsf			SSP1CON2,ACKEN		; Master acknowlegde
	rcall		WaitMSSP
    return

    global  I2C_RX_compass
I2C_RX_compass:
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x3C                ; address
    rcall       I2C_TX
	movlw		0x03
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP

	bcf			PIR1,SSPIF
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x3D                ; address
    rcall       I2C_TX

    ; Compass IC sends data in following order:
    ; x MSB
    ; x LSB
    ; z MSB
    ; z LSB
    ; y MSB
    ; y LSB

    ; Chip orientation on the PCB requires
    ; Original = Corrected
    ; x = -y
    ; z = z
    ; y = x

    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,compass_DY+1; Data Byte
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,compass_DY+0; Data Byte
    banksel compass_DY
    comf        compass_DY+1        ; 16bit sign change.
    negf        compass_DY+0
    btfsc       STATUS,C            ; Carry to propagate ?
    incf        compass_DY+1,F      ; YES: do it.
    banksel common
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,compass_DZ+1; Data Byte
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,compass_DZ+0; Data Byte
    rcall       I2C_OneByteRX       ; Get one byte
	movff		SSP1BUF,compass_DX+1; Data Byte
	bsf			SSP1CON2, RCEN      ; Enable recieve mode
	rcall		WaitMSSP
	movff		SSP1BUF,compass_DX+0; Data Byte
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP
    return

    global  I2C_init_compass
I2C_init_compass:
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x3C                ; address
    rcall       I2C_TX
	movlw		0x00
    rcall       I2C_TX
;	movlw		b'01101001'        ; ConfigA:  3Hz, 8 Samples averaged, Test Mode (Positive Bias)
	movlw		b'01101000'        ; ConfigA:  3Hz, 8 Samples averaged
    rcall       I2C_TX
    bra         I2C_init_compass_common

    global  I2C_init_compass_fast
I2C_init_compass_fast:
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x3C                ; address
    rcall       I2C_TX
	movlw		0x00
    rcall       I2C_TX
    movlw		b'00111000'        ; ConfigA: 75Hz, 2 Samples averaged
;    movlw		b'00111001'        ; ConfigA: 75Hz, 2 Samples averaged, Test Mode (Positive Bias)
    rcall       I2C_TX
I2C_init_compass_common:
    movff       opt_compass_gain,i2c_temp    ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
    swapf       i2c_temp,F
    comf        i2c_temp,F
    bcf         STATUS,C
    rlcf        i2c_temp
    movf        i2c_temp,W
    clrf        i2c_temp
    rcall       I2C_TX
	movlw		b'00000000'        ; Continous Mode
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP
    bsf         compass_enabled
    return

    global  I2C_sleep_compass
I2C_sleep_compass:
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x3C                ; address
    rcall       I2C_TX
	movlw		0x00
    rcall       I2C_TX
	movlw		b'01101000'        ; ConfigA
    rcall       I2C_TX
	movlw		b'00100000'        ; ConfigB
    rcall       I2C_TX
	movlw		b'00000010'        ; Idle Mode
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP
    bcf         compass_enabled
    return


   global  I2C_init_accelerometer
I2C_init_accelerometer:
    rcall       I2C_sleep_accelerometer ; Regs can only be changed in St.By mode

	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x38                ; address
    rcall       I2C_TX
	movlw		0x0E                ; XYZ_DATA_CFG
    rcall       I2C_TX
	movlw		b'00000000'         ; High pass Filter=0 , +/- 2g range
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP


	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x38                ; address
    rcall       I2C_TX
	movlw		0x2A                ; CTRL_REG1
    rcall       I2C_TX
;	movlw		b'00110000'         ; CTRL_REG1: 160ms data rate, St.By Mode
	movlw		b'00110100'         ; CTRL_REG1: 160ms data rate, St.By Mode, reduced noise mode
    rcall       I2C_TX
	movlw		b'00000010'         ; CTRL_REG2: High Res in Active mode
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP

	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x38                ; address
    rcall       I2C_TX
	movlw		0x2A                ; CTRL_REG1
    rcall       I2C_TX
;	movlw		b'00110001'         ; CTRL_REG1: 160ms data rate, Active Mode
	movlw		b'00110101'         ; CTRL_REG1: 160ms data rate, St.By Mode, reduced noise mode, Active Mode
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP

    return

    global  I2C_sleep_accelerometer
I2C_sleep_accelerometer:
	bsf			SSP1CON2,SEN		; Start condition
	rcall		WaitMSSP
	movlw		0x38                ; address
    rcall       I2C_TX
	movlw		0x2A                ; CTRL_REG1
    rcall       I2C_TX
	movlw		b'00000000'         ; St. By Mode
    rcall       I2C_TX
	bsf			SSP1CON2,PEN		; Stop condition
	rcall		WaitMSSP
    return


    END