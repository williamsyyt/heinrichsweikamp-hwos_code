;=============================================================================
;
;   File external_flash.asm
;
;   External flash
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-12 : [mH] creation

	#include "hwos.inc"
	#include "wait.inc"

basic    CODE
;=============================================================================

	global		incf_ext_flash_address_p1
incf_ext_flash_address_p1:					; Increase by one
	movlw		.1

	global	incf_ext_flash_address0
incf_ext_flash_address0:
    addwf		ext_flash_address+0,F      ; increase address
    movlw		d'0'
    addwfc		ext_flash_address+1,F
    addwfc		ext_flash_address+2,F

	movlw		0x40
	cpfseq		ext_flash_address+2			; at address 40FFFF?
	return									; No, return
;	clrf		ext_flash_address+0
;	clrf		ext_flash_address+1
	clrf		ext_flash_address+2			; Yes, rollover to 0x000000
	return

	global		incf_ext_flash_address0_p1_0x20
incf_ext_flash_address0_p1_0x20:			; Increase by one
	movlw		.1

	global		incf_ext_flash_address0_0x20
incf_ext_flash_address0_0x20:		 	; with roll-over at 0x200000 to 0x000000
    addwf		ext_flash_address+0,F      ; increase address
    movlw		d'0'
    addwfc		ext_flash_address+1,F
    addwfc		ext_flash_address+2,F

	movlw		0x20
	cpfseq		ext_flash_address+2			; at address 0x200000?
	return									; No, return
;	clrf		ext_flash_address+0
;	clrf		ext_flash_address+1
	clrf		ext_flash_address+2			; Yes, rollover to 0x000000
	return

	global	decf_ext_flash_address0
decf_ext_flash_address0:
    subwf		ext_flash_address+0,F      ; decrease address: do a 16-8bits substract.
    movlw		d'0'
    subwfb		ext_flash_address+1,F
    movlw		d'0'
    subwfb		ext_flash_address+2,F

	btfss		ext_flash_address+2,7		; Rollover to 0xFFFFFF?
	return									; No, return
	clrf		ext_flash_address+2			; Set to 0x00FFFFF
	setf		ext_flash_address+1
	setf		ext_flash_address+0
	return

	global	ext_flash_power_down
ext_flash_power_down:
	movlw		0x04				; Write disable
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	return

	global	ext_flash_byte_read_plus	; Return data read in WREG and SSP2BUF and 
ext_flash_byte_read_plus:				; increase address after read
	rcall	ext_flash_byte_read
	movwf	temp1						; store received data
	bra		incf_ext_flash_address_p1	; +1 and return

	global	ext_flash_byte_read_plus_0x20; Return data read in WREG and SSP2BUF and 
ext_flash_byte_read_plus_0x20:			; increase address after read with banking at 0x200000
	rcall	ext_flash_byte_read
	movwf	temp1						; store received data
	bra		incf_ext_flash_address0_p1_0x20 ;+1 and return


	global	ext_flash_byte_read		; Return data read in WREG
ext_flash_byte_read:
	movlw		0x03				; Read command
	rcall		write_spi
    rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	rcall		write_spi			; Dummy write to read data into WREG
	bsf			flash_ncs			; CS=1
	movwf		temp1
	return							; Return data read in WREG and temp1

ext_flash_write_address: ; Write 24bit address ext_flash_address:3 via SPI
	movf		ext_flash_address+2,W	; 24Bit Address
	rcall		write_spi
	movf		ext_flash_address+1,W
	rcall		write_spi
	movf		ext_flash_address+0,W
	bra 		write_spi               ; And return....

	global	ext_flash_read_block_start		; Return data read in WREG
ext_flash_read_block_start:
	movlw		0x03				; Read command
	rcall		write_spi
    rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	rcall		write_spi			; Dummy write to read data into WREG
	return							; Return data read in WREG

	global	ext_flash_read_block		; Return data read in WREG
ext_flash_read_block:
	rcall		incf_ext_flash_address_p1	; Increase address +1
	bra			write_spi1			; Dummy write to read data into WREG and return

	global	ext_flash_read_block_stop	; Return data read in WREG
ext_flash_read_block_stop:
	bsf			flash_ncs			; CS=1
	return							; NO data in WREG

	global	write_byte_ext_flash_plus_header
write_byte_ext_flash_plus_header:		; Write from WREG and increase address after write
	movwf		temp1					; store data
	; test if write is done at first byte of 4kB block
	; if yes -> delete 4kB block first
	tstfsz		ext_flash_address+0			; at 0x00?
	bra			write_byte_ext_flash_plus_h1	; No, normal Write

	movf		ext_flash_address+1,W
	andlw		0x0F						; Mask lower nibble
	tstfsz		WREG						; at 0x.0?
	bra			write_byte_ext_flash_plus_h1; No, normal Write
	
	; At beginning of 4kB block -> rease first!
	rcall		ext_flash_erase4kB		; Erases 4kB sector @ext_flash_address:3
write_byte_ext_flash_plus_h1:
	movf		temp1,W
	rcall		ext_flash_byte_write	; Write the byte
	bra			incf_ext_flash_address_p1	; +1 and return

    global  write_byte_ext_flash_plus_nocnt ; No increase of ext_flash_dive_counter:3
write_byte_ext_flash_plus_nocnt:
    movwf		temp1                       ; store data
    bra         write_byte_ext_flash_plus2

    global  write_byte_ext_flash_plus_nodel ; Does NOT delete 4kB Page when required
write_byte_ext_flash_plus_nodel:            ; Write from WREG and increase address after write with banking at 0x200000
    movwf		temp1                       ; store data
    bra         write_byte_ext_flash_plus1  ; Ignore possible begin of 4kB page, there have been written 0xFF already

	global	write_byte_ext_flash_plus	; Write from WREG and increase address after write with banking at 0x200000
write_byte_ext_flash_plus:
	movwf		temp1					; store data
    
    ; First, increase dive length counter
    incf        ext_flash_dive_counter+0,F
    movlw       .0
    addwfc      ext_flash_dive_counter+1,F
    addwfc      ext_flash_dive_counter+2,F  ; 24bit++

write_byte_ext_flash_plus2:
	; Now test if write is done at first byte of 4kB block
	; if yes -> delete 4kB block first
	tstfsz		ext_flash_address+0			; at 0x00?
	bra			write_byte_ext_flash_plus1	; No, normal Write

	movf		ext_flash_address+1,W
	andlw		0x0F						; Mask lower nibble
	tstfsz		WREG						; at 0x.0?
	bra			write_byte_ext_flash_plus1	; No, normal Write
	
	; At beginning of 4kB block -> erase first!
	rcall		ext_flash_erase4kB		; Erases 4kB sector @ext_flash_address:3
write_byte_ext_flash_plus1:
	movf		temp1,W
	rcall		ext_flash_byte_write	; Write the byte
	bra			incf_ext_flash_address0_p1_0x20		 ; +1 and roll over at 0x200000 to 0x000000	and return

	global	ext_flash_byte_write	; Write from WREG
ext_flash_byte_write:
	movwf		temp1				; store data byte
	bsf			flash_ncs			; CS=1
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x02				; Write command
	rcall		write_spi
    rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	movf		temp1,W				; load data byte
	rcall		write_spi			; write one byte of data!
	bsf			flash_ncs			; CS=1
	return
	
	global	ext_flash_disable_protection		; Disable write protection
ext_flash_disable_protection:
	bsf			flash_ncs			; CS=1
	movlw		0x50				; EWSR command
	rcall		write_spi
	bsf			flash_ncs			; CS=1

	movlw		0x01				; WRSR command
	rcall		write_spi
	movlw		b'00000000'			; New status
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	return

	global	ext_flash_enable_protection
ext_flash_enable_protection:
	bsf			flash_ncs			; CS=1
	movlw		0x50				; EWSR command
	rcall		write_spi
	bsf			flash_ncs			; CS=1

	movlw		0x01				; WRSR command
	rcall		write_spi
	movlw		b'00011100'			; New status (Write protect on)
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	return


	global	ext_flash_erase4kB		; Erases 4kB sector
ext_flash_erase4kB:
	bsf			flash_ncs			; CS=1
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x20				; Sector erase command
	rcall		write_spi
    rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	bsf			flash_ncs			; CS=1
;	bra			ext_flash_wait_write	; Wait for write... and return
ext_flash_wait_write:
	WAITMS		d'1'				; TBE=25ms...
	movlw		0x05				; RDSR command
	rcall		write_spi			; Read status
	rcall		write_spi			; Read status into WREG
	bsf			flash_ncs			; CS=1
	btfsc		SSP2BUF,0			; Write operation in process?
	bra			ext_flash_wait_write	; Yes, wait more..
	return

	global	ext_flash_erase_logbook	; erases logbook memory (000000h -> 2FFFFFh -> 3MByte)
ext_flash_erase_logbook:
	bsf			flash_ncs			; CS=1
	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	clrf		ext_flash_address+2

	movlw		d'48'
	movwf		temp1				; 48*64kB=917504 Bytes
ext_flash_erase_logbook_loop:
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0xD8				; 64kB erase command
	rcall		write_spi
    rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	bsf			flash_ncs			; CS=1
	rcall		ext_flash_wait_write	; Wait for write...

	incf		ext_flash_address+2,F	; 64kB ahead
	decfsz		temp1,F
	bra			ext_flash_erase_logbook_loop
	return


write_spi:		; With data in WREG...
	bcf			flash_ncs			; CS
	global	write_spi1
write_spi1:		; With data in WREG...
	bcf			SSP2STAT,WCOL		; Clear flag	
	movwf		SSP2BUF				; Write to buffer
	btfsc		SSP2STAT,WCOL		; Was buffer full?
	bra			write_spi1			; Yes, try again

write_spi2:	; Wait for write command
	btfss		SSP2STAT, BF		; Buffer full?
	bra			write_spi2			; No, wait.
	movf		SSP2BUF,W			
	return	; Returns RX data in WREG and SSP2BUF

	END