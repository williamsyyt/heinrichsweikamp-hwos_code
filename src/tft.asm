;=============================================================================
;
;   File tft.asm
;
;   Managing the TFT screen
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-05-24 : [jDG] Cleanups from initial Matthias code.

#include "ostc3.inc"
#include "wait.inc"
#include "varargs.inc"
#include "external_flash.inc"
#include "tft_outputs.inc"
#include "eeprom_rs232.inc"

;=============================================================================
; TFT_frame needs to backup coordinates.
        CBLOCK  tmp
            save_top                
            save_height 
            save_left   
            save_width
        	ds_line                 ; Current line (0..239).
        	ds_column               ; Current columnx2 (0..159)
        	ds_pixel:2              ; Current pixel color.
        	ds_count                ; Repetition count.
        ENDC

;=============================================================================
; Basic bit-level macros

RD_H		macro
			bsf tft_rd,0
			endm

RD_L		macro
			bcf tft_rd,0
			endm

RS_H		macro
			bsf tft_rs,0
			endm

RS_L		macro
			bcf tft_rs,0
			endm

NCS_H		macro
			bsf tft_cs,0
			endm

NCS_L		macro
			bcf tft_cs,0
			endm

WR_H		macro
			bsf tft_nwr,0
			endm

WR_L		macro
			bcf tft_nwr,0
			endm

;=============================================================================
; Byte-leve macros
;
Index_out		macro low_b
				movlw low_b
				rcall TFT_CmdWrite
				endm

Parameter_out	macro high_b, low_b
				movlw high_b
				movwf PORTA		; Upper
				movlw low_b
				rcall TFT_DataWrite
				endm


basic   CODE

 
;=============================================================================
; TFT_write_flash_image
;
; Inputs:  FSR2 = EEPROM address / 256
;          win_left, win_top : imagte CENTER position
; Outputs: win_height, win_width.
;          image copyed on screen.
; Trashed: PROD, hi, lo
; 
	global	TFT_write_flash_image
TFT_write_flash_image:
    ; Get back the full 24bit EEPROM address
    clrf    ext_flash_address+0
    movff   FSR2L,ext_flash_address+1
    movf    FSR2H,W
    iorlw   0x30
    movwf   ext_flash_address+2
    
    ; Read header: width and height
    global  TFT_write_flash_image_addr
TFT_write_flash_image_addr:    
	call	ext_flash_read_block_start
	movff   SSP2BUF,win_width+0 
	movwf	SSP2BUF						; Write to buffer to initiate new read
	btfss	SSP2STAT, BF                ; Next byte ready ?
	bra		$-2                         ; NO: wait...
   	movff   SSP2BUF,win_width+1
	movwf	SSP2BUF						; Write to buffer to initiate new read
	btfss	SSP2STAT, BF                ; Next byte ready ?
	bra		$-2                         ; NO: wait...
	movff   SSP2BUF,win_height  
	movwf	SSP2BUF						; Write to buffer to initiate new read
	btfss	SSP2STAT, BF                ; Next byte ready ?
	bra		$-2                         ; NO: wait...
	movff   SSP2BUF,WREG                ; drop 4th byte.
	movwf	SSP2BUF						; Write to buffer to initiate new read
	btfss	SSP2STAT, BF                ; Next byte ready ?
	bra		$-2                         ; NO: wait...

    ; Sanity check on header to avoid badly uploaded images.
    iorwf   WREG                        ; Check height < 256
    bnz     TFT_write_flash_image_failed
    movf    win_width+1,W               ; Check width < 512
    andlw   0xFE
    bnz     TFT_write_flash_image_failed
    
    ; Center image on win_top, win_left values
    bcf     STATUS,C                    ; Clear carry
    rrcf    win_height,W                ; And get height/2
    subwf   win_top,F                   ; top -= height/2
    rrcf    win_width+1,W               ; Get 9th bit into carry
    rrcf    win_width+0,W               ; Get width/2 (in 0..320 range)
    bcf     STATUS,C
    rrcf    WREG,W                      ; Get width/2 in 0..160 range
    subwf   win_leftx2,F                ; left -= width/2

	rcall	TFT_box_write		    	; Inputs : win_top, win_leftx2, win_height, win_width(in 1..320 range)
        
    ; Compute number of pixels to move (result on 17 bits !)
    clrf    TBLPTRU
    movf    win_width+0,W
    mulwf   win_height                  ; Result in PRODL:H
    movf    win_width+1,W
    bz      TFT_write_flash_image_1     ; width > 8bits ?
    movf    win_height,W                ; YES: add extra
    addwf   PRODH,F
    rlcf    TBLPTRU                     ; And carry into upper register.
TFT_write_flash_image_1:
    incf    PRODH,F                     ; Pre-condition nested loops
    incf    TBLPTRU,F

    ; Write pixels
	Index_out 0x22						; Frame Memory Data Write start
	RS_H								; Data

TFT_write_flash_image_loop:
	btfss	SSP2STAT, BF				; Buffer full?
	bra		$-2                         ; NO: wait...
	movff	SSP2BUF,PORTH			    ; Read lo
	movwf	SSP2BUF						; Write to buffer to initiate new read

	btfss	SSP2STAT, BF				; Buffer full?
	bra		$-2                         ; NO: wait...
	movff	SSP2BUF,PORTA               ; And read hi
	movwf	SSP2BUF						; Write to buffer to initiate new read
	WR_L
	WR_H								; Write 1 Pixel

	decfsz	PRODL,F
	bra		TFT_write_flash_image_loop
	decfsz	PRODH,F
	bra		TFT_write_flash_image_loop
	decfsz	TBLPTRU,F
	bra		TFT_write_flash_image_loop

	btfss	SSP2STAT, BF				; Buffer full?
	bra		$-2	                        ; No, wait
	movf	SSP2BUF,W                   ; Read dummy byte

	bsf		flash_ncs					; CS=1
	movlw	0x00                        ; NOP, to stop window mode
	bra     TFT_CmdWrite				; This routine "returns"
	
	;---- Draw a 4x4 red square in place of missing images...
TFT_write_flash_image_failed:
    movlw   -1
    addwf   win_leftx2,F
    movlw   -2
    addwf   win_top,F
    movlw   2
    movwf   win_width+0
    clrf    win_width+1
    movlw   4
    movwf   win_height
    movlw   color_red
    rcall   TFT_set_color
    goto    TFT_box

;=============================================================================
;

    global  TFT_CmdWrite
TFT_CmdWrite:
	RS_L				; Command
	clrf	PORTA		; Upper
	movwf	PORTH		; Lower
	WR_L
	WR_H				; Tick
	return;

    global  TFT_DataWrite
TFT_DataWrite:
	RS_H				; Data
	movwf 	PORTH		; Lower
	WR_L
	WR_H				; Tick
	return

;=============================================================================
;
    global  TFT_ClearScreen
TFT_ClearScreen:
	Index_out 0x50				; Window Horizontal Start Address
	Parameter_out 0x00, 0x00	; 0-239
	Index_out 0x51				; Window Horizontal End Address
	Parameter_out 0x00, 0xEF	; 0-239
	Index_out 0x52				; Window Vertical Start Address
	Parameter_out 0x00, 0x00	; 0-319
	Index_out 0x53				; Window Vertical End Address
	Parameter_out 0x01, 0x3F	; 0-319
	Index_out 0x20				; Frame Memory Horizontal Address
	Parameter_out 0x00, 0x00	; 0-239
	Index_out 0x21				; Frame Memory Vertical Address
	Parameter_out 0x01, 0x3F	; 0-319

	Index_out 0x22				; Frame Memory Data Write start

	RD_H						; Not Read
	RS_H						; Data
	NCS_L						; Not CS
	clrf	PORTA				; Data Upper
	clrf	PORTH				; Data Lower

	movlw	d'10'
	movwf	tft_temp3
TFT_ClearScreen2:
	movlw	d'30'
	movwf	tft_temp2
TFT_ClearScreen3:
	clrf	tft_temp1				; 30*10*256=76800 Pixels -> Clear complete 240*320
TFT_ClearScreen4:
	WR_L
	WR_H							; Tick
	decfsz	tft_temp1,F
	bra		TFT_ClearScreen4
	decfsz	tft_temp2,F
	bra		TFT_ClearScreen3
	decfsz	tft_temp3,F
	bra		TFT_ClearScreen2
	return

;=============================================================================
; 
    global  TFT_DisplayOff
TFT_DisplayOff:
	clrf	CCPR1L				; PWM OFF
	clrf	PORTA
    clrf	PORTH
	RD_L	; LOW
	nop
	RS_L	; LOW
	bcf		tft_nwr
	nop
	bcf		tft_cs
	nop
	bcf		tft_nreset
	WAITMS	d'1'
	bsf		tft_power				; inverted...
	bcf		lightsen_power  		; power-down light sensor
	return

; -----------------------------
; TFT boot
; -----------------------------
    global  TFT_boot
TFT_boot:
	clrf	PORTA
	clrf	PORTH
	RD_L	; LOW
	bcf		tft_nwr
	nop
	bcf		tft_cs
	nop
	bcf		tft_nreset
	WAITMS	d'1'
	bcf		tft_power				; inverted...
	WAITMS	d'1'

	RD_H	; Keep high
	WR_H	;
	NCS_L	; Not CS

	WAITMS	d'2'
	bsf		tft_nreset
	WAITMS	d'150'
	bsf		lightsen_power  		; Supply power to light sensor

; Data Transfer Synchronization
	Parameter_out 0x00, 0x00
	Parameter_out 0x00, 0x00

; Init through config table...
    movlw   LOW     display0_config_table
    movwf   TBLPTRL
    movlw   HIGH    display0_config_table
    movwf   TBLPTRH
    movlw   UPPER   display0_config_table
    movwf   TBLPTRU
    rcall   display0_init_loop

	Index_out 0x22
;	WAITMS	d'81'					; 46
	call	TFT_ClearScreen
	Index_out 0x07
	Parameter_out 0x01, 0x00
	return


display0_config_table:
    ; Reg, Dat0, Dat1 or 0xFF,0x00,0x00 for end
    db  0xA4,0x00,0x01,0xFF,.002,0x00
    db  0x09,0x00,0x01,0x92,0x04,0x00
    db  0x93,0x04,0x02,0x94,0x00,0x02
    db  0x07,0x00,0x00,0x10,0x04,0x30
    db  0x11,0x02,0x37,0x12,0x11,0x8D
    db  0x13,0x11,0x00,0x01,0x01,0x00
    db  0x02,0x02,0x00,0x03,0x50,0x20
    db  0x0A,0x00,0x08,0x0D,0x00,0x00
    db  0x0E,0x00,0x30,0xFF,.151,0x00
    db  0x12,0x11,0xBD,0x20,0x00,0x00
    db  0x21,0x00,0x00,0x30,0x06,0x02
    db  0x31,0x56,0x0D,0x32,0x05,0x07
    db  0x33,0x06,0x09,0x34,0x00,0x00
    db  0x35,0x09,0x06,0x36,0x57,0x05
    db  0x37,0x0D,0x06,0x38,0x02,0x06
    db  0x39,0x00,0x00,0xFF,0x00,0x00

display0_init_loop:
    TBLRD*+
    movlw   0xFF
    cpfseq  TABLAT
    bra     display0_config_write    ; Write Config pair to Display
    ; Delay ms or quit (return)
    TBLRD*+
    tstfsz  TABLAT                  ; End of config?
    bra     $+4                     ; No
    return                          ; Done.
    movf    TABLAT,W
    call    WAITMSX                 ; Wait WREG milliseconds
    TBLRD*+                         ; Dummy read (Third byte of delay command)
    bra     display0_init_loop      ; Loop

display0_config_write:              ; With command in WREG
    movf    TABLAT,W
    rcall   TFT_CmdWrite            ; Write command
    TBLRD*+                         ; Get config0
    movff   TABLAT,PORTA
    TBLRD*+                         ; Get config1
    movf    TABLAT,W
    rcall   TFT_DataWrite           ; Write config
    bra     display0_init_loop      ; Loop


;=============================================================================
; Smooth lighting-up of the display:
;
; Trashes: WREG, PRODL
; Typical usage:
;    clrf    CCPR1L            ; Backlight off
;   [draw splash screen]
;   call     TFT_DisplayFadeIn
;
        global  TFT_Display_FadeIn
TFT_Display_FadeIn:
    	movlw	CCP1CON_VALUE			; See ostc3.inc
        movwf	CCP1CON
		bsf		tft_is_dimming	; TFT is dimming, ignore ambient sensor!
        clrf    CCPR1L          ; Backlight off - to be sure
		movff	max_CCPR1L,PRODL
TFT_Display_FadeIn_0:
        incf    CCPR1L,F        ; Duty cycle
        WAITMS  d'2'
        decfsz  PRODL,F
        bra     TFT_Display_FadeIn_0 
		bcf		tft_is_dimming	; dimming done.
        return

;=============================================================================
; Smooth lighting-off of the display:
; Trashes: WREG, PRODL
        global  TFT_Display_FadeOut
TFT_Display_FadeOut:
		movff	max_CCPR1L,PRODL
		bsf		tft_is_dimming	; TFT is dimming, ignore ambient sensor!
TFT_Display_FadeOut_0:
        movff   PRODL,CCPR1L    ; Duty cycle
        WAITMS  d'1'
        decfsz  PRODL,F
        bra     TFT_Display_FadeOut_0 
        clrf    CCPR1L
        return

;=============================================================================

        global  box_std_block, box_black_block, box_color_block

box_std_block:                          ; Use white color
        setf    WREG
        bra     box_common
box_black_block:                        ; Use black color
        clrf    WREG
box_common:
box_color_block:
        rcall   TFT_set_color
        VARARGS_BEGIN
            VARARGS_GET8    win_top
            VARARGS_GET8    win_height
            VARARGS_GET8    win_leftx2
            VARARGS_GET8    win_width
        VARARGS_END
        bra    TFT_box

;-----------------------------------------------------------------------------

        global  box_frame_std, box_frame_common, box_frame_color, box_frame_color16

box_frame_std:
        setf    WREG
        rcall   TFT_set_color

box_frame_common:
        VARARGS_BEGIN
            VARARGS_GET8    win_top
            VARARGS_GET8    win_height
            VARARGS_GET8    win_leftx2
            VARARGS_GET8    win_width
        VARARGS_END
        bra    TFT_frame

box_frame_color:
      	rcall	TFT_set_color
box_frame_color16:
		bra		box_frame_common

;=============================================================================
; Init for half_pixel_write
; Set column register on TFT device, and current color.
; Inputs: win_leftx2
; Outputs: win_color:2
; Trashed: WREG, PROD
        global init_pixel_write
init_pixel_write:
        movff   win_leftx2,WREG
        mullw   2
        rcall   pixel_write_col320      ; Start Address Vertical (.0 - .319)
        setf    WREG
        bra     TFT_set_color

;-----------------------------------------------------------------------------
; Writes two half-pixels at position (win_top,win_leftx2)
; Inputs: win_leftx2, win_top, win_color:2
; Trashed: WREG, PROD
    global  pixel_write
pixel_write:
        movff   win_leftx2,WREG
        mullw   2						; win_leftx2 x 2 -> PRODH:PRODL
        rcall   pixel_write_col320      ; Start Address Vertical (.0 - .319)
        rcall   half_pixel_write        ; Write this half-one.

        movff   win_leftx2,WREG         ; Address of next one
        mullw   2
        infsnz  PRODL                   ; +1
        incf    PRODH
    	rcall   pixel_write_col320
    	bra     half_pixel_write        ; Note: Cmd 0x20 is mandatory, because
    	                                ; of the autoincrement going vertical

	global	pixel_write_col320
pixel_write_col320:
		Index_out 0x21					; Frame Memory Vertical Address
		bra     TFT_DataWrite_PROD

;-----------------------------------------------------------------------------
; Writes one half-pixel at position (win_top,win_leftx2).
; Inputs: win_leftx2, win_top, win_color:2
; Trashed: WREG, PROD
        global  half_pixel_write
half_pixel_write:
    	movff  	win_top,WREG            ; d'0' ... d'239'
    ; Variant with Y position in WREG.
half_pixel_write_1:
	  	sublw   .239                    ; 239-Y --> Y

    	mullw   1                       ; Copy row to PRODH:L
		Index_out 0x20				; Frame Memory Horizontal Address
		rcall   TFT_DataWrite_PROD

		Index_out 0x22				; Frame Memory Data Write start
		RS_H				; Data
		movff	win_color1,PORTA		; Upper
		movff	win_color2,PORTH		; Lower
		WR_L
		WR_H				; Tick
    	return

;-----------------------------------------------------------------------------
; Writes a vertical line of half-pixel at position (win_top,win_leftx2,win_height).
; Inputs: win_leftx2, win_top, win_height, win_color:2
; Trashed: WREG, PROD, TABLAT, TBLPTRL
	global	half_vertical_line
half_vertical_line:
        clrf    TABLAT                  ; Loop index.

half_vertical_line_loop:
        movff   win_leftx2,WREG         ; Init X position.
        mullw   2
        movf    TABLAT,W                ; Get loop index
        andlw   1                       ; Just low bit
        xorwf   PRODL,F                 ; And use it to jitter current X position
        rcall   pixel_write_col320      ; Start Address Vertical (.0 - .319)

        movff   win_height,WREG         ; Index reached height (Bank0 read) ?
        xorwf   TABLAT,W
        btfsc   STATUS,Z                ; Equals ?
        return                          ; Yes: done.
        movff   win_top,WREG            ; Y = top + index (Bank0 read)
        addwf   TABLAT,W
        rcall   half_pixel_write_1
        incf    TABLAT,F                ; index++
        bra     half_vertical_line_loop

;-----------------------------------------------------------------------------
; Writes a horizontal line of half-pixel at position (win_top,win_leftx2,win_width).
; Inputs: win_leftx2, win_top, win_width, win_color:2
; Trashed: WREG, PROD, TABLAT, TBLPTRL
	global	half_horizontal_line
half_horizontal_line:
        clrf    TABLAT                  ; Loop index.

half_horizontal_line_loop:
        movff   win_leftx2,WREG         ; Init X position.
        mullw   2
        rcall   pixel_write_col320      ; Start Address Vertical (.0 - .319)
        movff   win_width,WREG          ; Index reached height (Bank0 read) ?
        xorwf   TABLAT,W
        btfsc   STATUS,Z                ; Equals ?
        return                          ; Yes: done.
        movff   win_top,WREG            ; Y = top + index (Bank0 read)
        addwf   TABLAT,W
        rcall   half_pixel_write_1
        incf    TABLAT,F                ; index++
        bra     half_horizontal_line_loop


;-----------------------------------------------------------------------------
; TFT Data Cmd via W
;
    global  TFT_DataWrite_PROD
TFT_DataWrite_PROD:
;	RD_H				; Keep high
	RS_H				; Data
	movff	PRODH,PORTA	; Move high byte to PORTA
    movff	PRODL,PORTH	; Move low byte to PORTH
	WR_L
	WR_H                ; Tick
	return

TFT_DataRead_PROD:
    Index_out 0x22                  ; Frame Memory Data Read start
    setf    TRISA                   ; PortA as input.
    setf    TRISH                   ; PortH as input.
	RS_H				; Data
	WR_H                ; Not write
	RD_L                ; Read!
    nop
    nop
    nop
	RD_H				; Tick
    nop
	RD_L                ; Read!
    nop
    nop
    nop
    movff   PORTA,PRODH
    movff   PORTH,PRODL
	RD_H				; Tick
    nop
    clrf    TRISA                   ; PortA as output
    clrf    TRISH                   ; PortH as output
	return



;=============================================================================
; Output TFT Window Address commands.
; Inputs : win_top, win_leftx2, win_height, win_width.
; Output : PortA/PortH commands.
; Trashed: PROD
;
        global  TFT_box_write
TFT_box_write:
		movff	win_leftx2,WREG         ; Compute left = 2*leftx2 --> PROD
		mullw	2

        global  TFT_box_write_16bit_win_left
TFT_box_write_16bit_win_left:           ; With column in PRODL:PRODH
        ;---- Normal horizontal window ---------------------------------------
        ; Output 0x35 left,
        ;        0x36 right ==  left + width - 1.

		Index_out 0x52				; Window Vertical Start Address
		rcall   TFT_DataWrite_PROD          ; Output left
		Index_out 0x21				; Frame Memory Vertical Address
		rcall   TFT_DataWrite_PROD			; Output left

		movff	win_width+0,WREG	    ; right = left + width - 1
		addwf	PRODL,F
		movff	win_width+1,WREG
		addwfc	PRODH,F
		decf	PRODL,F			    ; decrement result
		btfss   STATUS,C
		decf	PRODH,F

		Index_out 0x53				; Window Vertical End Address
		rcall   TFT_DataWrite_PROD

        ;---- Flipped vertical window ----------------------------------------
        ; Output 0x37 flipped(bottom) = 239-bottom = 240 - top - height
        ;             flipped(top)    = 239-top
TFT_box_flip_V:
		movff   win_top,PRODL
		movff   win_height,WREG
		addwf   PRODL,W
		sublw   .240                 ; 240 - top - height
		movwf   PRODH                ; First byte

		movf	PRODL,W
		sublw   .239                ; 249-top
		movwf   PRODL               ; --> second byte.

		Index_out 0x50				; Window Horizontal Start Address
		clrf	PORTA				; Upper
		movf	PRODH,W
		rcall	TFT_DataWrite		; Lower (and tick)

		Index_out 0x51				; Window Horizontal End Address
		clrf	PORTA				; Upper
		movf	PRODL,W
		rcall	TFT_DataWrite		; Lower (and tick)

		Index_out 0x20				; Frame Memory Horizontal Address
		clrf	PORTA				; Upper
		movf	PRODL,W
		rcall	TFT_DataWrite		; Lower (and tick)
		return


;=============================================================================
; TFT_frame : draw a frame around current box with current color.
; Inputs:  win_top, win_leftx2, win_height, win_width, win_color1, win_color2
; Outputs: (none)
; Trashed: WREG, PROD, aa_start:2, aa_end:2
    global  TFT_frame
TFT_frame:
    movff   win_top,save_top            ; Backup everything.
    movff   win_height,save_height
    movff   win_leftx2,save_left
    movff   win_width,save_width

    ;---- TOP line -----------------------------------------------------------
    movlw   1                           ; row ~ height=1
    movff   WREG,win_height
    rcall   TFT_box

    ;---- BOTTOM line --------------------------------------------------------
    movff   save_top,PRODL               ; Get back top,
    movff   save_height,WREG             ; and height
    addwf   PRODL,W                      ; top+height
    decf    WREG                         ; top+height-1
    movff   WREG,win_top                 ; top+height-1 --> top
    rcall   TFT_box                        

    ;---- LEFT column --------------------------------------------------------
    movff   save_top,win_top             ; Restore top/height.
    movff   save_height,win_height
    movlw   1                               ; column ~ width=1
    movff   WREG,win_width
    rcall   TFT_box

    ;---- RIGHT column -------------------------------------------------------
    movff   save_left,WREG
    movff   save_width,PRODL
    addwf   PRODL,W
    decf    WREG
    movff   WREG,win_leftx2
    rcall     TFT_box
    
    ;---- Restore everything -------------------------------------------------
    movff   save_left,win_leftx2
    movff   save_width,win_width
    return

;=============================================================================
; TFT_box : fills current box with current color.
; Inputs:  win_top, win_leftx2, win_height, win_width, win_color1, win_color2
; Outputs: (none)
; Trashed: WREG, PROD
    global  TFT_box

TFT_box:
    ;---- Define Window ------------------------------------------------------
	movf	win_width,W
	bcf     STATUS,C
	rlcf    WREG
	movwf   win_width+0
	movlw   0
	rlcf    WREG
	movwf   win_width+1
	rcall   TFT_box_write               ; Setup box

    global  TFT_box_16bit_win_left
TFT_box_16bit_win_left:
    rrcf    win_width+1,W               ; width /= 2
    rrcf    win_width+0,W
    movwf   win_width

    ;---- Fill Window --------------------------------------------------------
	Index_out 0x22						; Frame Memory Data Write start

	clrf	PRODH                       ; Column counter.
	RS_H				; Data

TFT_box2:                              ; Loop height times
	movff	win_height,PRODL

TFT_box3:                              ; loop width times
	movff	win_color1,PORTA			; Upper
	movff	win_color2,PORTH			; Lower
	WR_L
	WR_H				; Tick

	movff	win_color1,PORTA			; Upper
	movff	win_color2,PORTH			; Lower
	WR_L
	WR_H				; Tick

	decfsz	PRODL,F                     ; row loop finished ?
	bra		TFT_box3                   ; No: continue.

    incf    PRODH,F                     ; column count ++

    movff   win_bargraph,WREG           ; current column == bargraph ?
    cpfseq  PRODH
    bra     TFT_box4                   ; No: just loop.

    clrf    WREG                        ; Yes: switch to black
    movff   WREG,win_color1
    movff   WREG,win_color2
TFT_box4:

    movff   win_width+0,WREG            ; compare ?
    xorwf   PRODH,W
    bnz     TFT_box2                    ; Loop not finished.

	movlw	0x00                        ; NOP, to stop window mode
	rcall   TFT_CmdWrite

	setf    WREG                        ; Reset bargraph mode...
	movff   WREG,win_bargraph

	return

;=============================================================================
;Converts 8Bit RGB b'RRRGGGBB' into 16Bit RGB b'RRRRRGGGGGGBBBBB'
    global  TFT_set_color

TFT_set_color:
	movwf	tft_temp1				; Get 8Bit RGB b'RRRGGGBB'
	movwf   tft_temp2               ; Copy

	; Mask Bit 7,6,5,4,3,2
	movlw	b'00000011'
	andwf	tft_temp2,F

	movlw	b'00000000'
	dcfsnz	tft_temp2,F
	movlw	b'01010000'
	dcfsnz	tft_temp2,F
	movlw	b'10100000'
	dcfsnz	tft_temp2,F
	movlw	b'11111000'
	movwf	tft_temp3				; Blue done.

	movff	tft_temp1,	tft_temp2	; Copy
	; Mask Bit 7,6,5,1,0
	movlw	b'00011100'
	andwf	tft_temp2,F
	rrncf	tft_temp2,F
	rrncf	tft_temp2,F

	movlw	b'00000000'
	dcfsnz	tft_temp2,F
	movlw	b'00000100'
	dcfsnz	tft_temp2,F
	movlw	b'00001000'
	dcfsnz	tft_temp2,F
	movlw	b'00001100'
	dcfsnz	tft_temp2,F
	movlw	b'00010000'
	dcfsnz	tft_temp2,F
	movlw	b'00010100'
	dcfsnz	tft_temp2,F
	movlw	b'00100000'
	dcfsnz	tft_temp2,F
	movlw	b'00111111'
	movwf	tft_temp4			

	rrcf	tft_temp4,F
	rrcf	tft_temp3,F

	rrcf	tft_temp4,F
	rrcf	tft_temp3,F

	rrcf	tft_temp4,F
	rrcf	tft_temp3,F		; tft_temp3 (b'GGGBBBBB') done.

	movff	tft_temp1,	tft_temp2	; Copy
	clrf	tft_temp1

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F		; Green done.

	; Mask Bit 4,3,2,1,0
	movlw	b'11100000'
	andwf	tft_temp2,F

	rrncf	tft_temp2,F
	rrncf	tft_temp2,F
	rrncf	tft_temp2,F
	rrncf	tft_temp2,F
	rrncf	tft_temp2,F

	movlw	b'00000000'
	dcfsnz	tft_temp2,F
	movlw	b'00000100'
	dcfsnz	tft_temp2,F
	movlw	b'00001000'
	dcfsnz	tft_temp2,F
	movlw	b'00001100'
	dcfsnz	tft_temp2,F
	movlw	b'00010000'
	dcfsnz	tft_temp2,F
	movlw	b'00010100'
	dcfsnz	tft_temp2,F
	movlw	b'00100000'
	dcfsnz	tft_temp2,F
	movlw	b'00111111'
	movwf	tft_temp4			

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F	

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F

	rrcf	tft_temp4,F
	rrcf	tft_temp1,F		; Red done.

	movff	tft_temp1,win_color1
	movff	tft_temp3,win_color2	; Set Bank0 Color registers...
	return

;=============================================================================
; Dump screen contents to the UART

    global  TFT_dump_screen
TFT_dump_screen:
    bsf         no_sensor_int
	movlw	    'l'
	movwf	    TXREG                   ; Send command echo.
    call		rs232_wait_tx           ; wait for UART
    ;---- Send DISPLAY box command for the full screen window -------------------
	Index_out 0x50				; Window Horizontal Start Address
	Parameter_out 0x00, 0x00	; 0-239
	Index_out 0x51				; Window Horizontal End Address
	Parameter_out 0x00, 0xEF	; 0-239
	Index_out 0x52				; Window Vertical Start Address
	Parameter_out 0x00, 0x00	; 0-319
	Index_out 0x53				; Window Vertical End Address
	Parameter_out 0x01, 0x3F	; 0-319

    clrf        ds_column
    rcall       dump_screen_pixel_reset
dump_screen_1:
    btg         LEDr                 ; LED activity toggle
    ; Dump even column
	movlw	    .240                 ; 240 lines, once.
	movwf	    ds_line
dump_screen_2:
	Index_out   0x20				; Frame Memory Horizontal Address
    movff       ds_line,WREG        ; d'0' ... d'239'
   	mullw       1                   ; Copy row to PRODH:L
	rcall       TFT_DataWrite_PROD

    movff       ds_column,WREG     ; Init X position.
    mullw       2
    rcall       pixel_write_col320  ; Start Address Vertical (.0 - .319)

    rcall       TFT_DataRead_PROD      ; read pixel
    rcall       dump_screen_pixel

    decfsz	    ds_line,F
    bra		    dump_screen_2
    rcall       dump_screen_pixel_flush

    ; Dump odd column
	movlw	    .240                    ; 240 lines, twice.
	movwf	    ds_line
dump_screen_3:
	Index_out   0x20				; Frame Memory Horizontal Address
    movff   	ds_line,WREG        ; d'0' ... d'239'
   	mullw       1                   ; Copy row to PRODH:L
	rcall       TFT_DataWrite_PROD

    movff       ds_column,WREG     ; Init X position.
    mullw       2
    movlw       .1
    addwf       PRODL,F
    movlw       0
    addwfc      PRODH,F             ; +1
    rcall       pixel_write_col320  ; Start Address Vertical (.0 - .319)

    rcall       TFT_DataRead_PROD      ; read pixel
    rcall       dump_screen_pixel

    decfsz	    ds_line,F
    bra		    dump_screen_3
    rcall       dump_screen_pixel_flush

    incf        ds_column,F
    movlw       .160
    cpfseq      ds_column
    bra		    dump_screen_1

    bcf         no_sensor_int
    clrf        RCREG1              ; Clear receive buffer
	bcf         RCSTA1,CREN         ; Clear receiver status
	bsf         RCSTA1,CREN
    bsf         enable_screen_dumps ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    return


;=============================================================================
; Pixel compression
;
; Input: PRODH:L = pixel.
; Output: Compressed stream on output.
; Compressed format:
;       0ccccccc    : BLACK pixel, repeated ccccccc+1 times (1..128).
;       11cccccc    : WHITE pixel, repeated cccccc+1 times (1..64).
;       10cccccc HIGH LOW : color pixel (H:L) repeated ccccc+1 times (1..64).
;
dump_screen_pixel:
    movf        PRODH,W                 ; Compare pixel-high
    xorwf       ds_pixel+1,W
    bnz         dump_screen_pixel_1     ; Different -> dump.

    movf        PRODL,W                 ; Compare pixel-low
    xorwf       ds_pixel+0,W
    bnz         dump_screen_pixel_1     ; Different -> dump.

    incf        ds_count,F              ; Same color: just increment.
    return

dump_screen_pixel_1:                    ; Send (pixel,count) tuple
    movf        ds_count,W              ; Is count zero ?
    bz          dump_screen_pixel_2     ; Yes: skip sending.

    movf        ds_pixel+1,W            ; This is a BLACK pixel ?
    iorwf       ds_pixel+0,W    
    bz          dump_screen_pix_black   ; YES.

    movf        ds_pixel+1,W            ; This is a white pixel ?
    andwf       ds_pixel+0,W
    incf        WREG
    bz          dump_screen_pix_white   ; YES.

    ; No: write the pixel itself...
    movlw       .64                     ; Max color pixel on a single byte.
    cpfsgt      ds_count                ; Skip if count > 64
    movf        ds_count,W              ; W <- min(64,count)
    subwf       ds_count,F              ; ds_count <- ds_count-W
    decf        WREG                    ; Save as 0..63
    iorlw       b'10000000'             ; MARK as a color pixel.

    movwf       TXREG
    call		rs232_wait_tx           ; wait for UART
    movff       ds_pixel+1,TXREG
    call		rs232_wait_tx           ; wait for UART
    movff       ds_pixel+0,TXREG
    call		rs232_wait_tx           ; wait for UART
    bra         dump_screen_pixel_1

dump_screen_pixel_2:
    movff       PRODH,ds_pixel+1        ; Save new pixel color
    movff       PRODL,ds_pixel+0
    movlw       1
    movwf       ds_count                ; And set count=1.
    return

dump_screen_pix_black:
    movlw       .128                    ; Max black pixel on a single byte.
    cpfsgt      ds_count                ; Skip if count > 128
    movf        ds_count,W              ; W <- min(128,count)
    subwf       ds_count,F              ; ds_count <- ds_count-W
    decf        WREG                    ; Save as 0..127
dump_screen_pix_3:
    movwf       TXREG
    call        rs232_wait_tx
    bra         dump_screen_pixel_1     ; More to dump ?

dump_screen_pix_white:
    movlw       .64                     ; Max white pixel on a single byte.
    cpfsgt      ds_count                ; Skip if count > 64
    movf        ds_count,W              ; W <- min(64,count)
    subwf       ds_count,F              ; ds_count <- ds_count-W
    decf        WREG                    ; Save as 0..63
    iorlw       b'11000000'             ; MARK as a compressed white.
    bra         dump_screen_pix_3

dump_screen_pixel_flush:
    clrf        PRODH
    clrf        PRODL
    rcall       dump_screen_pixel_1     ; Send it
dump_screen_pixel_reset:
    clrf        ds_count                ; But clear count.
    return

    end