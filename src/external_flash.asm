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
	movlw		0x02				; Write (PP, Page-Program) command
	rcall		write_spi
	rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	movf		temp1,W				; load data byte
	rcall		write_spi			; write one byte of data!
	bra	ext_flash_wait_write	    ; And return...

	global	ext_flash_byte_write_comms  ; without wait, ~86us fixed delay due to 115200 Bauds
ext_flash_byte_write_comms:
	movwf		temp1				; store data byte
	bsf			flash_ncs			; CS=1
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x02				; Write (PP, Page-Program) command
	rcall		write_spi
	rcall       ext_flash_write_address ; Write 24bit address ext_flash_address:3 via SPI
	movf		temp1,W				; load data byte
	rcall		write_spi			; write one byte of data!
	bsf			flash_ncs			; CS=1
	return
	
	global	ext_flash_disable_protection		; Disable write protection
ext_flash_disable_protection:
    ; unlock old memory
	bsf			flash_ncs			; CS=1
	movlw		0x50				; EWSR command
	rcall		write_spi
	bsf			flash_ncs			; CS=1

	movlw		0x01				; WRSR command
	rcall		write_spi
	movlw		b'00000000'			; New status
	rcall		write_spi
	bsf			flash_ncs			; CS=1

    ; unlock new memory
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x98				; ULBPR command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x42				; WBPR command
	rcall		write_spi
	movlw       .18
	movwf       temp2
ext_flash_disable_protection2:
	movlw       0x00
	rcall		write_spi
        decfsz      temp2,F             ; 18 bytes with 0x00
	bra         ext_flash_disable_protection2
        bsf			flash_ncs			; CS=1
	return

	global	ext_flash_enable_protection
ext_flash_enable_protection:
    ; lock old memory
	bsf			flash_ncs			; CS=1
	movlw		0x50				; EWSR command
	rcall		write_spi
	bsf			flash_ncs			; CS=1

	movlw		0x01				; WRSR command
	rcall		write_spi
	movlw		b'00011100'			; New status (Write protect on)
	rcall		write_spi
	bsf			flash_ncs			; CS=1

    ; lock new memory
;    	movlw		0x06				; WREN command
;	rcall		write_spi
;	bsf			flash_ncs			; CS=1
;	movlw		0x8D				; LBPR command
;	rcall		write_spi
;	bsf			flash_ncs			; CS=1
	movlw		0x06				; WREN command
	rcall		write_spi
	bsf			flash_ncs			; CS=1
	movlw		0x42				; WBPR command
	rcall		write_spi
    movlw       .18
    movwf       temp2
ext_flash_enable_protection2:
    movlw       0xFF
    rcall		write_spi
    decfsz      temp2,F             ; 18 bytes with 0xFF
    bra         ext_flash_enable_protection2
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
;	bra			ext_flash_wait_write	; Wait for write... and return
ext_flash_wait_write:
	bsf			flash_ncs			; CS=1
;	WAITMS		d'1'				; TBE/TSE=25ms...
	movlw		0x05				; RDSR command
	rcall		write_spi			; Read status
	rcall		write_spi			; Read status into WREG
	bsf			flash_ncs			; CS=1
	btfsc		SSP2BUF,0			; Write operation in process?
	bra			ext_flash_wait_write	; Yes, wait more..
	return

	global	ext_flash_erase_logbook	; erases logbook memory (000000h -> 2FFFFFh -> 3MByte -> 3145728 Bytes)
ext_flash_erase_logbook:
	bsf			flash_ncs			; CS=1
	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	clrf		ext_flash_address+2

	setf		temp1				; 256*12kB=3145728 Bytes
ext_flash_erase_logbook_loop:
    rcall       ext_flash_erase4kB  ; 4kB
    rcall       ext_flash_add_4kB   ; Increase ext_flash_address:3 by 4kB
    rcall       ext_flash_erase4kB  ; 4kB
    rcall       ext_flash_add_4kB   ; Increase ext_flash_address:3 by 4kB
    rcall       ext_flash_erase4kB  ; 4kB
    rcall       ext_flash_add_4kB   ; Increase ext_flash_address:3 by 4kB
	decfsz		temp1,F
	bra			ext_flash_erase_logbook_loop
	return

ext_flash_add_4kB:
    movlw       0x10
    addwf       ext_flash_address+1,F
    movlw		d'0'
    addwfc		ext_flash_address+2,F
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

    global      fix_180_dives
fix_180_dives:           ; fix dives made with the 1.80
    clrf    divesecs    ; Here: # of dive (0-255) in TOC
fix_180_dives2:
    rcall   fix_load_dive_into_buffer   ; Load header #divesecs into buffer:256
    rcall   fix_check_buffer            ; Check the buffered dive
    tstfsz  WREG                ; Dive needs fix?
    rcall   fix_buffered_dive   ; Yes, fix and save it
    decfsz  divesecs,F          ; All done?
    bra     fix_180_dives2      ; No, continue
    return                      ; All done.

fix_buffered_dive:           ; Yes, fix and save it
    rcall	ext_flash_disable_protection	; Disable write protection for external flash
    banksel buffer
    ; Set to 1.81
    movlw   .81
    movwf   buffer+.49
    ; Fix wrong profile length
    movlw   .3
    subwf	buffer+.9,F
    movlw	d'0'
    subwfb	buffer+.10,F
    subwfb	buffer+.11,F
    banksel common
    ; save result into external flash again
    rcall   fix_set_to_toc_start
    clrf    lo
    lfsr    FSR0,buffer+0
fix_buffered_dive2:
    movf    POSTINC0,W
    rcall   write_byte_ext_flash_plus_header; Write from WREG and increase address after write
    decfsz  lo,F                            ; All done?
    bra     fix_buffered_dive2              ; No, continue
    return

fix_check_buffer:            ; Check the buffered dive
    movff   buffer+.48,temp1
    movlw   .1
    cpfseq  temp1           ; buffer+.48 = .1 ?
    retlw   .0              ; No, abort
    movff   buffer+.49,temp1
    movlw   .80
    cpfseq  temp1           ; buffer+.49 = .80 ?
    retlw   .0              ; No, abort
    retlw   .1              ; Yes, fix this dive

fix_load_dive_into_buffer:  ; Load header #divesecs into buffer:256
    rcall   fix_set_to_toc_start
    clrf    lo
    lfsr    FSR0,buffer+0
fix_load_dive_into_buffer2:
    rcall   ext_flash_byte_read_plus    	; increase address after read
    movff   temp1,POSTINC0                  ; copy into buffer
    decfsz  lo,F                            ; All done?
    bra     fix_load_dive_into_buffer2      ; No, continue

fix_set_to_toc_start:
    clrf    ext_flash_address+0
    clrf    ext_flash_address+1
    movlw   0x20
    movwf   ext_flash_address+2
    movlw   .16
    mulwf   divesecs; divesecs*16 = offset to 0x2000 (up:hi)
    movf    PRODL,W
    addwf   ext_flash_address+1,F
    movf    PRODH,W
    addwfc  ext_flash_address+2,F
    ; pointer at the first 0xFA of header
    return


	END