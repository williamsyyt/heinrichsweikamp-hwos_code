;=============================================================================
;
;   File convert.inc
;
;   Converts register values to string
;
;   Copyright (c) 2011, Matthias Heinrichs, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2007-10-07 : [MH]  Creation for OSTC sources
;   2010-12-10 : [jDG] Optimize macro size
;

    extern  output16_3_call
output_16_3 macro			; displays only last three figures from a 16Bit value (0-999)
	call 	output16_3_call
	endm

    extern  output16
output_16dp macro temp4		; 16Bit with decimal point
	movlw	temp4			; Temp4 stores position for decimal point
	call 	output16
	endm					

    extern  output16_call
output_16 macro				; 16Bit Normal
	call 	output16_call
	endm

   extern  output8_call
output_8 macro				; 8 Bit Normal
 	call    output8_call
	endm

	extern  output99_call
output_99 macro				; displays only last two figures from a 8Bit value (0-99)
	call 	output99_call
	endm

	extern  output99x_call
output_99x macro			; displays only last two figures from a 8Bit value with leading zero (00-99) 
	call 	output99x_call
	endm
