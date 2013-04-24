#include "ostc3.inc"

; Make sure symbols from the .inc are available to the C code:
    ; Filtered data
    global compass_DX_f, compass_DY_f, compass_DZ_f
    global accel_DX_f,   accel_DY_f,   accel_DZ_f

    ; Calibration data
    global compass_CX_f
    global compass_CY_f
    global compass_CZ_f

    ; Tmp values to pass Q15 arithmetics around
    global compass_a
    global compass_b

    ; Result
    global compass_heading, compass_roll, compass_pitch

compass code
;-----------------------------------------------------------------------------
; Filter compass values
;
; Apply linear filtering to input parameters.
    global compass_filter

; Apply filtering formula:
;   reg_f += (reg - reg_f) / 4
FILTER16 MACRO   reg, reg_f
        movf    reg_f+0,W
        subwf   reg+0,W
        movwf   PRODL
        movf    reg_f+1,W
        subwfb  reg+1,W
        movwf   PRODH

        bcf     STATUS,C        ; Copy sign bit into carry
        btfsc   PRODH,7
        bsf     STATUS,C
        rrcf    PRODH,F         ; 16bit shift right
        rrcf    PRODL,F

        bcf     STATUS,C        ; Copy sign bit into carry
        btfsc   PRODH,7
        bsf     STATUS,C
        rrcf    PRODH,F         ; 16bit shift right
        rrcf    PRODL,W

        addwf   reg_f+0,F
        movf    PRODH,W
        addwfc  reg_f+1,F
        ENDM

compass_filter:
        banksel   compass_DX

        FILTER16  compass_DX, compass_DX_f
        FILTER16  compass_DY, compass_DY_f
        FILTER16  compass_DZ, compass_DZ_f
        FILTER16  accel_DX,   accel_DX_f
        FILTER16  accel_DY,   accel_DY_f
        FILTER16  accel_DZ,   accel_DZ_f
        banksel   common
        return

;-----------------------------------------------------------------------------

        global compass_filter_init
compass_filter_init:
        movff   compass_DX+0, compass_DX_f+0
        movff   compass_DX+1, compass_DX_f+1
        movff   compass_DY+0, compass_DY_f+0
        movff   compass_DY+1, compass_DY_f+1
        movff   compass_DZ+0, compass_DZ_f+0
        movff   compass_DZ+1, compass_DZ_f+1
        movff   accel_DX+0,   accel_DX_f+0
        movff   accel_DX+1,   accel_DX_f+1
        movff   accel_DY+0,   accel_DY_f+0
        movff   accel_DY+1,   accel_DY_f+1
        movff   accel_DZ+0,   accel_DZ_f+0
        movff   accel_DZ+1,   accel_DZ_f+1
        return

;-----------------------------------------------------------------------------
; Q15 fractional numbers: a * b / 2**16 (UNSIGNED)
;
; Uses 16x16->16 multiply, for positiv integers, keeping only the most
; revelant bits.
;
; Used to multiply two Q15 numbers, in the range 0..1,
; represented as 0..32767, that is a / 2**15.
;
; (a/2**15) * (b/2**15) = a*b / 2**30 = (a*b/2**16) / 2**14.
; So to get back a Q15 number, we need a shift-left...
    global compass_umul
compass_umul:
        rcall   compass_mul_16

; The 2x time, by left-shifting inserting the missing bit:
compass_mul_2:
        rlcf    compass_r+2,F           ; Missing bit into carry
        rlcf    compass_r+0,F
        rlcf    compass_r+1,F
        movff   compass_r+0,PRODL      ; return value into ProdH:L
        movff   compass_r+1,PRODH
        return

; The 16x16-> multiply:
compass_mul_16:
        banksel compass_a

        movf    compass_a+1,W           ; Block ah*bh
        mulwf   compass_b+1
        movff   PRODL,compass_r+0       ; and copy
        movff   PRODH,compass_r+1

        movf    compass_a+0,W           ; Block al*bl
        mulwf   compass_b+0
        movff   PRODH,compass_r+2       ; Into fraction byte

        movf    compass_a+1,W           ; Block ah*bl
        mulwf   compass_b+0
        movf    PRODL,W
        addwf   compass_r+2,F           ; Fraction part to carry.
        movf    PRODH,W                 ; and add16
        addwfc  compass_r+0,F
        movlw   0
        addwfc  compass_r+1,F

        movf    compass_a+0,W           ; Block al*bh
        mulwf   compass_b+1
        movf    PRODL,W
        addwf   compass_r+2,F           ; Fraction part to carry.
        movf    PRODH,W                 ; and add16
        addwfc  compass_r+0,F
        movlw   0
        addwfc  compass_r+1,F

        return

;-----------------------------------------------------------------------------
; Q15 fractional numbers: a * b / 2**16 (SIGNED)

        global compass_imul
compass_imul:
        rcall   compass_mul_16

        btfss   compass_b+1,7
        bra     compass_mul_3

        movf    compass_a+0,W
        subwf   compass_r+0,F
        movf    compass_a+1,W
        subwfb  compass_r+1,F

compass_mul_3:
        btfss   compass_a+1,7
        bra     compass_mul_4

        movf    compass_b+0,W
        subwf   compass_r+0,F
        movf    compass_b+1,W
        subwfb  compass_r+1,F

compass_mul_4:
        bcf     compass_r+1,6           ; Copy bit 7 to 6, so keep it after 2x
        btfsc   compass_r+1,7
        bsf     compass_r+1,6
        bra     compass_mul_2

        END
