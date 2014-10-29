;=============================================================================
;
;   File tft.inc
;
;   Declaring interfaces to the TFT screen and its Oxxx controler
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-05-24 : [jDG] Cleanups from initial Matthias code.

;=============================================================================
; TFT public subroutines
;=============================================================================

; Writes two half-pixels at position (win_top,win_leftx2)
; Inputs: win_leftx2, win_top, win_color:2
; Trashed: WREG, PROD
    extern  pixel_write

;-----------------------------------------------------------------------------
; Writes one half-pixel at position (win_top,win_leftx2).
; Inputs: win_leftx2, win_top, win_color:2
; Trashed: WREG, PROD
    extern  half_pixel_write

;-----------------------------------------------------------------------------
; TFT_frame : draw a frame around current box with current color.
; Inputs:  win_top, win_leftx2, win_height, win_width, win_color1, win_color2
; Outputs: (none)
; Trashed: WREG, PROD, aa_start:2, aa_end:2, win_leftx2, win_width:1
    extern  TFT_frame

;-----------------------------------------------------------------------------
	extern	TFT_DisplayOff              ; Power-off everything (need a boot next)
    extern  TFT_boot                    ; Initialize screen hardware
    extern  TFT_Display_FadeIn          ; Smooth lighting
    extern  TFT_Display_FadeOut         ; Smooth darkening
    extern  TFT_ClearScreen
    extern  TFT_box_write
    extern  TFT_box_write_16bit_win_left; With column in PRODL:PRODH
    extern  TFT_box
    extern  TFT_box_16bit_win_left      ; With column in PRODL:PRODH
    extern  TFT_DataWrite_PROD
    extern  TFT_set_color
	extern	init_pixel_write
	extern	pixel_write
	extern	pixel_write_col320
	extern	half_vertical_line
	extern  half_horizontal_line
    extern  TFT_dump_screen

;=============================================================================
; Low level macros (for aa_wordprocessor and color_processor).
;
            extern  TFT_CmdWrite
Index_out	macro low_b
			movlw low_b
			call  TFT_CmdWrite
			endm

;=============================================================================
; A shortcut for TFT_box and TFT_frame call sequences.
;
        extern  box_frame_std, box_frame_common, box_frame_color, box_frame_color16
        extern  box_std_block, box_black_block, box_color_block

; Erase a given screen area.
WIN_BOX_BLACK macro   top, bottom, left, right
        call    box_black_block
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

; Fill a given screen area with standard color (White).
WIN_BOX_STD macro   top, bottom, left, right
        call    box_std_block
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

; Fill a given screen area with color from WREG (8bits rrrgggbb)
WIN_BOX_COLOR macro   top, bottom, left, right
        call    box_color_block
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

; Draw a frame in standard color (White).
WIN_FRAME_STD macro   top, bottom, left, right
        call    box_frame_std
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

; Draw a frame with color from WREG (8bits rrrgggbb)
WIN_FRAME_COLOR macro   top, bottom, left, right
        call    box_frame_color
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

; Draw a frame with color from win_color (16bits in TFT format)
WIN_FRAME_COLOR16 macro   top, bottom, left, right
        call    box_frame_color16
        db      top, (bottom)-(top)+1, left, (right)-(left)+1
        endm

WIN_FONT 	macro	win_font_input
			movlw	win_font_input
			movff	WREG,win_font
			endm
			
WIN_TOP 	macro 	win_top_input
			movlw	win_top_input
			movff	WREG,win_top
			endm

WIN_HEIGHT 	macro 	h
			movlw	h
			movff	WREG,win_height
			endm
			
WIN_LEFT 	macro 	win_left_input
			movlw	win_left_input
			movff	WREG,win_leftx2
			endm
			
WIN_WIDTH 	macro 	w
			movlw	w
			movff	WREG,win_width
			endm

WIN_COLOR	macro 	win_color_input
			movlw	win_color_input
			call	TFT_set_color
			endm

;=============================================================================
; TFT_write_flash_image
;
; Inputs:  cx, cy : Image center (in 0..160 x 0..240 range)
;          image  : flash image header's address
; Outputs: win_top, win_left, win_height, win_width
;          image copyed on screen.
; Trashed: PROD, hi, lo
; 
;    extern  TFT_write_flash_image
;    extern  TFT_write_flash_image_addr
TFT_WRITE_FLASH_IMAGE macro cx, cy, image
    If LOW(image) != 0
        Error "Image "image" mis-aligned"
    Endif
    If (UPPER(image) & 0xF0) != 0x30
        Error "Image "image" not in image flash memory"
    Endif
    If (cx < 0) || (cx > 160)
        Error "Image center's X "cx" not in 0..160 range".
    Endif
    If (cy < 0) || (cy > 240)
        Error "Image center's Y "cy" not in 0..240 range".
    Endif
            ; Once we know image is aligned, we can use the compact (4bytes)
            ; 12bit registers to pass the address:
            lfsr    FSR2, ((image)>>8) & 0xFFF
            WIN_LEFT    cx
            WIN_TOP     cy
            call    TFT_write_flash_image
            endm

;=============================================================================
; TFT_write_prom_image
;
TFT_WRITE_PROM_IMAGE    macro   icon
            extern  icon, color_image
            movlw   LOW(icon)
            movwf   TBLPTRL
            movlw   HIGH(icon)
            movwf   TBLPTRH
            movlw   UPPER(icon)
            movwf   TBLPTRU
            call    color_image
            endm

;=============================================================================
; Macro to provides our own interface code.
;
PIXEL_WRITE macro   colRegister, rowRegister
        movff   colRegister,win_leftx2
        movff   rowRegister,win_top
        call    pixel_write
        endm

INIT_PIXEL_WROTE macro colRegister
        movff   colRegister,win_leftx2
        call    init_pixel_write
        endm

HALF_PIXEL_WRITE macro rowRegister
        movff   rowRegister,win_top
        call    half_pixel_write
        endm

