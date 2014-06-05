;=============================================================================
;
;   File File color_processor.asm
;
;   Decompress and draw an image.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2010-12-13 : [jDG] Creation.
;  2010-12-30 : [jDG] Revised to put temp into ACCESSRAM0
;
; RATIONALS: The OSTC have a nice color screen, and a std geek attitude impose
;            to show off ... ;-)
;
; Inputs: TBLPTR points to the image description block.
;         win_top, win_leftx2 the Top/Leftx2 corner here to put the image.
; Ouputs: None.
; Trashed: TBLPTR, TABLAT, FSR2, PROD, win_width, win_height
;
; ImageBloc:
;       db  widthx2, height
;       db  nbColors, 0     ; Unused yet... Should be 0 to keep packing happy.
;       dw  color0, color1, color2, color3, ...
;       db  ...packed pixels...
;
; Limitations:
; * nbColors should be <= 15.
; * image width should be even.
; * image left border should be on even position too.
; Compressed format:
; - Upper nibble = color, lower nibble = count-1.
; - All bytes F* accumulates to make count larger than 16.
; Eg. 00          is 1 pixel  color 0
;     07          is 8 pixels color 0
;     70          is 1 pixel  color 7
;     bf          is 16 pixels of color .11
;     F1 F2 F3 04 is 0x1235 pixels of color 0.
;
;-----------------------------------------------------------------------------

#include "ostc3.inc"
#include "tft.inc"

;-----------------------------------------------------------------------------
; Temporary overlay (in bank 1).

        CBLOCK  tmp                     ; Data overlay in reserved tmp area.
            img_colors:1
            img_pixels:3
            img_count:2
            ; Reserved to tmp+0x07...
        ENDC

;-----------------------------------------------------------------------------
;
; Note: Some variables (win_width, win_height) are in BANK0 !
basic   CODE
        global  color_image
color_image:
        banksel win_width               ; Bank1, just to be sure...

        ;---- Get image parameters -------------------------------------------
        tblrd*+                     
        movff   TABLAT,win_width
        tblrd*+
        movff   TABLAT,win_height
        tblrd*+
        movff   TABLAT,img_colors
        tblrd*+                         ; Skip one byte (future flags ?)
        ;---- Copy color table -----------------------------------------------
        movf    img_colors,W
        lfsr    FSR2,buffer
get_colors_loop:                    
        tblrd*+
        movff   TABLAT,POSTINC2
        tblrd*+
        movff   TABLAT,POSTINC2
        decfsz  WREG
        bra     get_colors_loop

        ; Compute width * height * 2 : the number of pixels to write.
        clrf    img_pixels+2
        movf    win_width,W             ; Compute number of pixels to draw
        mulwf   win_height              ; 0 .. 160x240
        bcf     STATUS,C                ; BEWARE: mulwf does not reset carry flag !
        rlcf    PRODL                   ; x2 --> 0 .. 320x240, might be > 0xFFFF
        rlcf    PRODH
        movff   PRODL, img_pixels+0
        movff   PRODH, img_pixels+1
        rlcf    img_pixels+2            ; Get the upper bit in place.
        
        clrf    WREG                    ; Decrement count to ease end detection.
        decf    img_pixels+0,F
        subwfb  img_pixels+1,F
        subwfb  img_pixels+2,F

        ;---- Send window command --------------------------------------------
        clrf    win_width+1             ; x2 on width, for the true box size.
        rlcf    win_width+0
        rlcf    win_width+1
        call    TFT_box_write
        Index_out 0x22

        ;---- Decode pixels --------------------------------------------------
color_image_loop_xy:
        ; Get pixel count
        clrf    img_count+0
        clrf    img_count+1

        ;---- Decode repetition count
color_image_decode_1:
        tblrd*+                         ; Get one byte

        btfss   TABLAT,7                ; High bit cleared ?
        bra     color_image_decode_2    ; YES: this is a color byte.
        
        rlcf    TABLAT,F                ; Drop high bit.
        movlw   .7                      ; Move 7 bits
color_image_decode_3:
        rlcf    TABLAT,F                ; Get bit into carry
        rlcf    img_count+0,F           ; Push into pixel count
        rlcf    img_count+1,F
        decfsz  WREG
        bra     color_image_decode_3    ; and loop foreach 7 bits.
        
        bra     color_image_decode_1    ; Decode next byte.

color_image_decode_2:
        ;---- Get pixel color into PROD
        movf    TABLAT,W                ; Get color index.
        addwf   WREG                    ; *2
        lfsr    FSR2,buffer             ; Reinitialize color table.
        movff   WREG,FSR2L              ; LOW(buffer) == 0
        movff   POSTINC2,PRODL
        movff   POSTINC2,PRODH
        
        ; Substract count-1 from the number of pixel we should do.
        movf    img_count+0,W           ; Make a 24bit substraction.
        subwf   img_pixels+0,F
        movf    img_count+1,W
        subwfb  img_pixels+1,F
        movlw   0
        subwfb  img_pixels+2,F

color_image_not_over:
        infsnz  img_count+0             ; Increment count.
        incf    img_count+1

        ; Loop sending pixel color
        incf    img_count+1             ; Because we decrement first, should add one here !
        bsf     tft_rs,0    ; RS_H				; Data
    	movff	PRODH,PORTA	; Move high byte to PORTA
        movff	PRODL,PORTH	; Move low byte to PORTH
color_image_loop_pixel:
    	bcf     tft_nwr,0       ; WR_L
        bsf     tft_nwr,0       ; WR_H                ; Tick
        decfsz  img_count+0
        bra     color_image_loop_pixel
        decfsz  img_count+1
        bra     color_image_loop_pixel
        
        ; And count (on a 24bit counter)
        clrf    WREG                    ; Make a 24bit decrement.
        decf    img_pixels+0
        subwfb  img_pixels+1,F
        subwfb  img_pixels+2,F

        bnn     color_image_loop_xy     ; Not finished ? loop...

        ;---- Closeup --------------------------------------------------------        
        Index_out 0x00
        return

        end