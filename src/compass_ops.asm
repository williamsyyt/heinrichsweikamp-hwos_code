#include    "ostc3.inc"
#include    "i2c.inc"
#include    "tft_outputs.inc"
#include    "isr.inc"
#include	"tft.inc"
#include	"strings.inc"
#include 	"wait.inc"                  ; speed_*
#include    "surfmode.inc"


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

    extern  compass
    extern  compass_reset_calibration
    extern  compass_add_calibration
    extern  compass_solve_calibration

compass code
;-----------------------------------------------------------------------------
; Filter compass values
;
; Apply linear filtering to input parameters.

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

        global  compass_filter
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

    global  compass_calibration_loop
compass_calibration_loop:               ; Compass calibration
    bsf     no_sensor_int               ; No Sensor ISR
    call    I2C_sleep_accelerometer     ; Stop accelerometer
    call    I2C_sleep_compass           ; Stop compass
    call	TFT_ClearScreen
    ; Mask
    WIN_COLOR   color_greenish
	WIN_SMALL	.16,.0
	STRCPY_TEXT_PRINT	tCompassMenu
    btfss		switch_right2           ; wait until button is released
    bra         $-4

    call	TFT_standard_color
;	WIN_SMALL	.0,.215
;    STRCPY_TEXT_PRINT  tExit
    WAITMS  d'255'
    WAITMS  d'255'
    movlw   .7                          ; Gain init
    movff   WREG,opt_compass_gain
compass_calibration_gainset:            ; Reduce the gain, set bank here!
    banksel opt_compass_gain
    decf    opt_compass_gain,F          ; Reduce by one
    btfsc   STATUS,N                    ; <0?
    clrf    opt_compass_gain            ; Yes, keep at zero

    banksel common
    call    I2C_init_accelerometer
    call    I2C_init_compass_fast
    call    TFT_compass_show_gain       ; Show the current compass gain

    WAITMS  d'100'

 	clrf	timeout_counter2
	clrf 	timeout_counter3

    call	speed_fastest
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; read Accelerometer

    ; Test all axes for +4096 (Hi byte=16)
    banksel compass_DX+1
    movlw   .16
    cpfseq  compass_DX+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DY+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DZ+1
    bra     $+4
    bra     compass_calibration_gainset

    ; Test all axes for -4096 (Hi byte=240)
    movlw   .240
    cpfseq  compass_DX+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DY+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DZ+1
    bra     $+4
    bra     compass_calibration_gainset
    banksel common

    rcall   compass_filter_init         ; set DX_f values
    call	compass_reset_calibration   ; Reset CX_f values
    banksel common

compass_calibration_loop2:
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    rcall   compass_filter              ; Filter compass raw data
    banksel common

    ; Twice
    call    I2C_RX_compass              ; read compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    rcall   compass_filter              ; Filter compass raw data
    banksel common

    ; Test all axes for +4096 (Hi byte=16)
    banksel compass_DX+1
    movlw   .16
    cpfseq  compass_DX+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DY+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DZ+1
    bra     $+4
    bra     compass_calibration_gainset

    ; Test all axes for -4096 (Hi byte=240)
    movlw   .240
    cpfseq  compass_DX+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DY+1
    bra     $+4
    bra     compass_calibration_gainset
    cpfseq  compass_DZ+1
    bra     $+4
    bra     compass_calibration_gainset
    banksel common
;
;    ; Three
;    call    I2C_RX_compass              ; read compass
;    call    I2C_RX_accelerometer        ; Test Accelerometer
;    call    compass_filter              ; Filter compass raw data
;    banksel common
;
;    ; Four times to get cleaner values
;    call    I2C_RX_compass              ; read compass
;    call    I2C_RX_accelerometer        ; Test Accelerometer
;    call    compass_filter              ; Filter compass raw data

    ; And register only one value out of four:
    call    compass_add_calibration     ; check and store new max/min values
    banksel common

    call    TFT_compass_fast            ; show values

    btfsc	sleepmode					; Sleepmode active?
    bra     compass_calibration_exit    ; Yes, exit

;    btfsc   switch_left                 ; Button pressed?
;    bra     compass_calibration_exit    ; Yes, exit

    btfss	onesecupdate				; do every second tasks?
    bra     compass_calibration_loop2   ; no, loop here

	movlw	.60
    call	timeout_testmode			; check timeout
    movlw   .60
    call    TFT_show_timeout_testmode   ; Show the timeout

    bcf     onesecupdate                ; clear flag

    bra     compass_calibration_loop2   ; loop here

compass_calibration_exit:
    call    compass_solve_calibration
    banksel common
    extern  option_save_all
	call	option_save_all             ; save all settings into EEPROM
    bcf     sleepmode                   ; Clear the flag before exiting to surfacemode
    movlw   .6
    movwf   customview_surfmode         ; Set to compass view...
   	goto	surfloop       				; ...and exit


        END
