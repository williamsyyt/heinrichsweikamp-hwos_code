;=============================================================================
;
;   File eeprom_rs232.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-08-03 : [mH] moving from OSTC code

write_int_eeprom	macro	eeprom_address
	movlw	eeprom_address
	call	write_int_eeprom_1
	endm

read_int_eeprom	macro	eeprom_address
	movlw	eeprom_address
	call	read_int_eeprom_1
	endm

	extern	write_int_eeprom_1
	extern	read_int_eeprom_1
	extern	read_eeprom
	extern	write_eeprom
	extern	enable_rs232
	extern	disable_rs232
	extern	rs232_get_byte
	extern	rs232_wait_tx
    extern  enable_ir
    extern  disable_ir
