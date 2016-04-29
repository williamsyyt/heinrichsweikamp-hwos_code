;=============================================================================
;
;   File external_flash.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-12 : [mH] creation

; Misc
  extern	incf_ext_flash_address_p1	; +1 for the ext_flash_address:3
 extern ext_flash_disable_protection; Disables write protection
 extern	ext_flash_enable_protection	; Enables write protection
 extern fix_180_dives               ; fix dives made with the 1.80

; Writes
 extern	write_byte_ext_flash_plus	; Write from WREG and increase address after write with banking at 0x200000
 extern write_byte_ext_flash_plus_nocnt ; No increase of ext_flash_dive_counter:3
 extern write_byte_ext_flash_plus_nodel ; Does NOT delete 4kB Page when required
 extern write_byte_ext_flash_plus_header ; Write from WREG and increase address after write
 extern ext_flash_byte_write		; Writes one byte from WREG @ext_flash_address:3
 extern	write_spi1					; Just (dummy)write to read a byte

; Delelte
 extern ext_flash_erase_logbook		; erases logbook memory (000000h -> 2FFFFFh -> 3MByte)
 extern ext_flash_erase4kB			; Erases 4kB sector @ext_flash_address:3
 
; Reads
 extern ext_flash_read_block_start	; Block read start and reads one byte@ext_flash_address:3 into WREG
 extern ext_flash_read_block		; Read another byte into WREG
 extern ext_flash_read_block_stop	; Stop block read
 extern ext_flash_byte_read			; Reads one byte@ext_flash_address:3 into WREG and temp1 
 extern	ext_flash_byte_read_plus	; Return data read in WREG and temp1 and increase address after read with banking at 0x200000
 extern	ext_flash_byte_read_plus_0x20; Return data read in WREG and temp1

; Will decrease ext_flash_address:2 with the 8Bit value "ext_flash_temp1"
	extern	decf_ext_flash_address0
decf_ext_flash_address	macro	ext_flash_temp1
        movlw	ext_flash_temp1
        call 	decf_ext_flash_address0
    endm

	extern	incf_ext_flash_address0
; Will increase ext_flash_address:2 with the 8Bit value "ext_flash_temp1"
incf_ext_flash_address	macro	ext_flash_temp1	
    	movlw	ext_flash_temp1                
    	call 	incf_ext_flash_address0
	endm

; With banking at 0x200000
	extern	incf_ext_flash_address0_0x20
; Will increase ext_flash_address:2 with the 8Bit value "ext_flash_temp1"
incf_ext_flash_address_0x20	macro	ext_flash_temp1	
    	movlw	ext_flash_temp1                
    	call 	incf_ext_flash_address0_0x20
	endm


