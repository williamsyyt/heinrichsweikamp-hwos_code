#include    "ostc3.inc"
#include    "i2c.inc"
#include    "tft_outputs.inc"
#include    "isr.inc"
#include	"tft.inc"
#include	"strings.inc"
#include 	"wait.inc"                  ; speed_*
#include    "surfmode.inc"
#include    "divemode.inc"
#include    "math.inc"
#include     "convert.inc"


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
    global compass_heading; , compass_roll, compass_pitch

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
        rcall   filter_16_common
        addwf   reg_f+0,F
        movf    PRODH,W
        addwfc  reg_f+1,F
        ENDM

filter_16_common:
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
        return

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
    rcall   TFT_compass_show_gain       ; Show the current compass gain

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
    rcall   TFT_show_timeout_testmode   ; Show the timeout

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

    global  TFT_compass_fast
TFT_compass_fast:
	WIN_TINY	.20,.50
	STRCPY  "X:"
    movff	compass_DX+0,lo
    movff	compass_DX+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT  " Y:"
    movff	compass_DY+0,lo
    movff	compass_DY+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT  " Z:"
    movff	compass_DZ+0,lo
    movff	compass_DZ+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    output_16
	STRCAT_PRINT "  "
    return

TFT_show_timeout_testmode:              ; With timeout in WREG...
    movwf   hi
    WIN_TINY	.20,.68
    STRCPY  "T:"
    movf    timeout_counter2,W          ; current timeout
    subwf   hi,W                        ; subtract from timeout value
    addlw   .1                          ; +1
    movwf   lo
    bsf     leftbind
    output_8                            ; Display timeout
    bcf     leftbind
    STRCAT_PRINT "s "
    return


TFT_compass_show_gain:       ; Show the current compass gain
    movff   opt_compass_gain,lo    ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
    tstfsz  lo
    return                         ; Do not show unless gain=0
	WIN_TINY	.20,.86
    STRCPY_TEXT  tCompassGain
    movff   opt_compass_gain,lo    ; 0-7 (230LSB/Gauss to 1370LSB/Gaus)
    bsf     leftbind
    output_8
    bcf     leftbind
    STRCAT_PRINT "!"
    return

    global  TFT_surface_compass_mask
TFT_surface_compass_mask:
    WIN_SMALL   surf_compass_mask_column,surf_compass_mask_row
	call	TFT_standard_color
    STRCPY_TEXT_PRINT   tHeading            ; Heading:
    return

    global  TFT_dive_compass_mask
TFT_dive_compass_mask:
    WIN_FRAME_STD   dive_compass_graph_row, dive_compass_graph_row+dive_compass_graph_height, .0, .159
    return

    global  TFT_surface_compass_heading
TFT_surface_compass_heading:
    rcall   compass_heading_common
    WIN_STD   surf_compass_head_column,surf_compass_head_row
	call	TFT_standard_color
TFT_surface_compass_heading_com:     ; Show "000° N"
    movff	compass_heading+0,lo
    movff	compass_heading+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required

    ; Shown and actual identical?
    movff   compass_heading_shown+0,WREG
    cpfseq  lo
    bra     TFT_surface_compass_heading_com1    ; Not equal
    movff   compass_heading_shown+1,WREG
    cpfseq  hi
    bra     TFT_surface_compass_heading_com1    ; Not equal
    bra     TFT_surface_compass_heading_com3    ; equal, skip smoothing

TFT_surface_compass_heading_com1:
    movff   lo,sub_a+0
    movff   hi,sub_a+1
    movff   compass_heading_shown+0,sub_b+0
    movff   compass_heading_shown+1,sub_b+1
    call    subU16
    btfsc   neg_flag
    bra     TFT_surface_compass_heading_com2        ; shown > actual
    ; shown < actual
    banksel compass_heading_shown
    infsnz  compass_heading_shown+0,F
    incf    compass_heading_shown+1,F               ; +1
    bra     TFT_surface_compass_heading_com3

TFT_surface_compass_heading_com2:
    banksel compass_heading_shown
    movlw	d'1'
	subwf	compass_heading_shown+0,F
	movlw	d'0'
	subwfb	compass_heading_shown+1,F               ; -1

TFT_surface_compass_heading_com3:
    banksel common
    movff   compass_heading_shown+0,lo
    movff   compass_heading_shown+1,hi
    bsf     leftbind
    output_16dp .2      ; Result is "0.000"
    bcf     leftbind
    ; rearrange figures to "000"
    movff   buffer+2,buffer+0
    movff   buffer+3,buffer+1
    movff   buffer+4,buffer+2
    lfsr	FSR2,buffer+3
    STRCAT  "° "
    rcall   tft_compass_cardinal        ; Add cardinal and ordinal to POSTINC2
    clrf    WREG
    movff   WREG,buffer+.7              ; limit to 7 chars
    STRCAT_PRINT ""
    return

    global  TFT_dive_compass_heading
TFT_dive_compass_heading:
    rcall   compass_heading_common
;    ; ToDo - these are for development only, hardcoding the bearing position
;    ; 244° : SW - W
;    movlw   low(d'244')
;    movff   WREG,compass_bearing+0
;    movlw   high(d'244')
;    movff   WREG,compass_bearing+1

    movff   compass_heading_shown+0,xA+0
    movff   compass_heading_shown+1,xA+1
    ; xRD and xRDlft
    ; 1.  160° viewing angle: +360 offset if xA<=292; for non-negative scale
    movlw   high(d'292')
    movff   WREG,sub_a+1
    movlw   low(d'292')
    movff   WREG,sub_a+0
    movff   xA+1,sub_b+1
    movff   xA+0,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; xA>292
    bra     TFT_dive_compass_heading_1  ;yes
    ; no, xA<=292
    movlw   high(d'360')
    addwf   xA+1,1
    movlw   low(d'360')
    addwf   xA+0,1
    btfsc   STATUS,C
    incf    xA+1
TFT_dive_compass_heading_1:
    ; 2. -80: left pixel offset from the center
    movlw   low( d'80' )
    subwf   xA+0,1
    btfss   STATUS,C
    decf    xA+1
    ; 3. save it to xRD
    movff   xA+0,xRD+0
    movff   xA+1,xRD+1
    ; 4. add 160 (display px width)
    movlw   high(d'160')
    addwf   xA+1,1
    movlw   low(d'160')
    addwf   xA+0,1
    btfsc   STATUS,C
    incf    xA+1
    ; 5. save it to xRDr
    movff   xA+0,xRDr+0
    movff   xA+1,xRDr+1

;    ; Bearing ?
;    ; We can skip this xRD180 calculation if no bearing is set
;    bcf     compass_bearing_set
;    movff   compass_bearing+0,sub_a+0
;    movff   compass_bearing+1,sub_a+1
;    movlw   d'0'
;    cpfseq  sub_a+1
;    bra     TFT_dive_compass_bearing  ; something set, calculate xRD180
;    movlw   d'0'
;    cpfseq  sub_a+0
;    bra     TFT_dive_compass_bearing  ; something set, calculate xRD180
;    bra     TFT_dive_compass_ruler      ; no value in the bearing, skip calc
;
;TFT_dive_compass_bearing:
;    bsf     compass_bearing_set

    btfss   compass_bearing_set
    bra     TFT_dive_compass_ruler      ; no value in the bearing, skip calc

    ; we have bearing set, we will need xRD180 calculated
    ; xRD180  is xRDr-180
    movff   xRDr+1,sub_a+1
    movff   xRDr+0,sub_a+0
    movlw   high(d'180')
    movff   WREG,sub_b+1
    movlw   low(d'180')
    movff   WREG,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    movff   sub_c+1,xRD180+1
    movff   sub_c+0,xRD180+0

TFT_dive_compass_bearing_1:
    ; calculate bearing position and visibility (ahead or behind)
    bcf     compass_bearing_vis     ; default is not-visibly
    bcf     compass_bearing_ahd     ; default is behind
    ; get the bearing virtual display offset, store it to divA
    movff   compass_bearing+0,xA+0
    movff   compass_bearing+1,xA+1
    movlw   high(d'360')
    addwf   xA+1,1
    movlw   low(d'360')
    addwf   xA+0,1
    btfsc   STATUS,C
    incf    xA+1
    ; save it to reuse for upper/lower turns and ahead/behind checks
    movff   xA+1,divA+1
    movff   xA+0,divA+0

    ; check if it's ahead
    ; load the bearing offset into sub_a
    movff   divA+1,sub_a+1
    movff   divA+0,sub_a+0
    ; load the display offset back to sub_b
    movff   xRD+0,sub_b+0
    movff   xRD+1,sub_b+1
    rcall    TFT_dive_compass_bearing_ap
    ;test if we found it
    btfsc   compass_bearing_vis
    bra     TFT_dive_compass_bearing_dir

    ; check if it's ahead with an upper turn
    ; load the bearing offset into sub_a
    movff   divA+1,sub_a+1
    movff   divA+0,sub_a+0
    ; load the display offset back to sub_b
    movff   xRD+0,sub_b+0
    movff   xRD+1,sub_b+1
    movlw   high(d'360')
    addwf   sub_b+1,1
    movlw   low(d'360')
    addwf   sub_b+0,1
    btfsc   STATUS,C
    incf    sub_b+1
    rcall    TFT_dive_compass_bearing_ap
    ;test if we found it
    btfsc   compass_bearing_vis
    bra     TFT_dive_compass_bearing_dir

    ; check if it's ahead with a lower turn
    ; load the bearing offset into sub_a
    movff   divA+1,sub_a+1
    movff   divA+0,sub_a+0
    movlw   high(d'360')
    addwf   sub_a+1,1
    movlw   low(d'360')
    addwf   sub_a+0,1
    btfsc   STATUS,C
    incf    sub_a+1
    ; load the display offset back to sub_b
    movff   xRD+0,sub_b+0
    movff   xRD+1,sub_b+1
    rcall    TFT_dive_compass_bearing_ap
    ;test if we found it
    btfsc   compass_bearing_vis
    bra     TFT_dive_compass_bearing_dir

    ; marker is not ahead of us, check if it's behind us
    ; use the (160 - (xRD180 - xCM)) formula to see if it's on the display
    ; load the display offset back to sub_a
    movff   xRD180+0,sub_a+0
    movff   xRD180+1,sub_a+1
    ; load the marker's offset into sub_b
    movff   divA+0,sub_b+0
    movff   divA+1,sub_b+1
    rcall    TFT_dive_compass_bearing_bp
    ;test if we found it
    btfsc   compass_bearing_vis
    bra     TFT_dive_compass_bearing_dir

    ;check if it's behind with the lower turn
    movff   xRD180+0,sub_a+0
    movff   xRD180+1,sub_a+1
    movlw   high(d'360')
    addwf   sub_a+1,1
    movlw   low(d'360')
    addwf   sub_a+0,1
    btfsc   STATUS,C
    incf    sub_a+1
    ; load the marker's offset into sub_b
    movff   divA+0,sub_b+0
    movff   divA+1,sub_b+1
    rcall    TFT_dive_compass_bearing_bp
    ;test if we found it
    btfsc   compass_bearing_vis
    bra     TFT_dive_compass_bearing_dir

    ; check if it's behind with the upper turn
    movff   divA+1,sub_b+1
    movff   divA+0,sub_b+0
    movlw   high(d'360')
    addwf   sub_b+1,1
    movlw   low(d'360')
    addwf   sub_b+0,1
    btfsc   STATUS,C
    incf    sub_b+1
    rcall    TFT_dive_compass_bearing_bp
    bra     TFT_dive_compass_bearing_dir

TFT_dive_compass_bearing_ap:
    ; xCM received in sub_a
    ; xRD received in sub_b
    ; 1/a. check if it's viewable from the left side
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; xRD>divA
    return  ;no,
    ; yes, store the RO=RP-RD for drawing
    movff   sub_c+0,xC+0
    movff   sub_c+1,xC+1
    ; 1/b. check if it's viewable from the right side?
    movlw   d'2'        ;   avoid thin mess on the side of the display
    addwf   sub_a+0,1
    btfsc   STATUS, C
    incf    sub_a+1
    ; load the display offset right side into sub_b
    movlw   high(d'160')
    addwf   sub_b+1,1
    movlw   low(d'160')
    addwf   sub_b+0,1
    btfsc   STATUS,C
    incf    sub_b+1
    call    subU16      ;  sub_c = sub_a - sub_b
    btfss   neg_flag    ; xRDr>xA(+2)
    return ; no,
    ; print the bearing lines on the screen
    movff   xC+0,xCM
    bsf     compass_bearing_vis     ; set visible
    bsf     compass_bearing_ahd     ; set ahead
    return    ; done,

TFT_dive_compass_bearing_bp:
    ; use the (160 - (xRD180 - xCM)) formula to see if it's on the display
    ; the marker's offset received in sub_b
    ; the xRD180 display offset received in sub_a
    ; xRD180 - xCM
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; CM>xRD180
    return  ; no, not on screen
    ; 160 - (X)
    movlw   high(d'160')
    movff   WREG,sub_a+1
    movlw   low(d'160')
    movff   WREG,sub_a+0
    movff   sub_c+1,sub_b+1
    movff   sub_c+0,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; X>160
    return  ; no, not on screen
    ; check if not overflow - this sounds a double check...
    movlw   d'1'
    cpfslt  sub_c+1
    return  ; high set, >160
    movlw   d'160'
    cpfslt  sub_c+0
    return  ; low >160
    ; print the bearing lines on the screen
    movff   sub_c+0,xCM
    bsf     compass_bearing_vis
    return    ; done

TFT_dive_compass_bearing_dir:
    ; check if bearing to heading, and calculate the direction
    bcf     compass_bearing_eq
    btfss   compass_bearing_vis
    bra     TFT_dive_compass_bearing_lr
    btfss   compass_bearing_ahd
    bra     TFT_dive_compass_bearing_lr
    movff   xCM,xA+0
    movlw   d'80'
    cpfseq  xA+0
    bra     TFT_dive_compass_bearing_lr
    bsf     compass_bearing_eq
    bra     TFT_dive_compass_ruler  ; bearing points to heading, no signs are required, go to the ruler

TFT_dive_compass_bearing_lr:
    ; get the bearing virtual display offset
    movff   compass_bearing+0,xA+0
    movff   compass_bearing+1,xA+1
    ; divA =IF (U10>292;U10;U10+360)
    movlw   high(d'292')
    movff   WREG,sub_a+1
    movlw   low(d'292')
    movff   WREG,sub_a+0
    movff   xA+1,sub_b+1
    movff   xA+0,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; xA>292
    bra     TFT_dive_compass_bearing_lr_1  ;yes
    ; no, xA<=292
    movlw   high(d'360')
    addwf   xA+1,1
    movlw   low(d'360')
    addwf   xA+0,1
    btfsc   STATUS,C
    incf    xA+1
TFT_dive_compass_bearing_lr_1:
    ; 1. calculate whether bearing is to left or to right
    bsf     compass_bearing_lft   ; to the left by default
    ; xC: save center value to compare the direction to front value
    movff   xA+1,xC+1
    movff   xA+0,xC+0
    ; xB: we need the left side for comparism... left = -180
    movff   xA+1,sub_a+1
    movff   xA+0,sub_a+0
    movlw   high(d'180')
    movff   WREG,sub_b+1
    movlw   low(d'180')
    movff   WREG,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    movff   sub_c+1,xB+1    ; xB has the left side of the 180° distance center
    movff   sub_c+0,xB+0
    ; xA = IF(xRD>(xC+100);xRD-280;xRD+80)
    movff   xC+1,sub_a+1
    movff   xC+0,sub_a+0
    movlw   d'100'
    addwf   sub_a+0,1
    btfsc   STATUS,C
    incf    sub_a+1
    movff   xRD+1,sub_b+1
    movff   xRD+0,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; xRD>xC+100
    bra     TFT_dive_compass_bearing_lr_2   ; yes, xA=xRD-280
    ; no, xA = xRD+80
    movff   xRD+1,xA+1
    movff   xRD+0,xA+0
    movlw   d'80'
    addwf   xA+0,1
    btfsc   STATUS,C
    incf    xA+1
    bra     TFT_dive_compass_bearing_lr_c

TFT_dive_compass_bearing_lr_2:
    ; xA=xRD-280
    movff   xRD+1,sub_a+1
    movff   xRD+0,sub_a+0
    movlw   high(d'280')
    movff   WREG,sub_b+1
    movlw   low(d'280')
    movff   WREG,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    movff   sub_c+1,xA+1
    movff   sub_c+0,xA+0
    ;bra     TFT_dive_compass_bearing_lr_c

TFT_dive_compass_bearing_lr_c:
   ; xB < xA < xC => right, otherwise left (default)
    movff   xA+1,sub_b+1
    movff   xA+0,sub_b+0
    movff   xB+1,sub_a+1
    movff   xB+0,sub_a+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfss   neg_flag    ; xA>xB ?
    bra     TFT_dive_compass_ruler     ; No, xB >= xA, keep default left
    movff   xA+1,sub_a+1
    movff   xA+0,sub_a+0
    movff   xC+1,sub_b+1
    movff   xC+0,sub_b+0
    call    subU16      ;  sub_c = sub_a - sub_b
    btfss   neg_flag    ; xC>xA ?
    bra     TFT_dive_compass_ruler     ; No, xA >= xC, keep default left
    bcf     compass_bearing_lft

TFT_dive_compass_ruler:
    ; calculate mod15 for the ticks
    movff   xRD+0,xA+0
    movff   xRD+1,xA+1
	movlw	d'15'
	movwf	xB+0
	clrf	xB+1
	call	div16x16  				;xA/xB=xC with xA+0 as remainder
    ; check xA+0, it has the remainder
    movlw   d'0'
    cpfsgt  xA+0                        ; mod15 > 0
    bra     TFT_dive_compass_ruler_1  ; no, RM = 0
    ; yes RM = 15 - RDmod15
    movlw   d'15'
    subfwb  xA+0,1
TFT_dive_compass_ruler_1:
    ; xA+0 holds the RM, store it to 'lo'
    movff    xA+0,lo
    ; init DD to zero, store it to 'hi'
    movlw   d'0'
    movff   WREG,hi

TFT_dive_compass_ruler_loop:
    ; 1. check if we run of from the display
    movlw   d'160'    ; Looks like 160 works because TFT_box limits the dispay
    cpfslt  lo,1
    bra     TFT_dive_compass_ruler_lend    ; xRM >= W
    ; 2. Clear the tick area from DD to RM - in segments to avoid blinking
    ;    don't do a clear if we are at 0 (zero) otherwise it will blink
    ;    because of the width underflow
    movlw   d'0'
    cpfsgt  lo,1
    bra     TFT_dive_compass_ruler_loop_zz
    rcall    TFT_dive_compass_clr_ruler
TFT_dive_compass_ruler_loop_zz:
    ; 3. Draw the markers @ RM
    rcall    TFT_dive_compass_ruler_print
    ; 4. If D<82 and RM>79: means we put something over the center line
    ;    redraw the center line
    movlw   d'82'
    cpfslt  hi,1
    bra     TFT_dive_compass_ruler_loop_zz2
    movlw   d'79'
    cpfsgt  lo,1
    bra     TFT_dive_compass_ruler_loop_zz2
    rcall    TFT_dive_compass_c_mk
TFT_dive_compass_ruler_loop_zz2:
    ; 5. set D = RM + 2 : position after the 2px tick
    movff   lo,hi
    movlw   d'2'
    addwf   hi,1
    ; 6. set RM = RM + 15 : position to the next tick
    movlw   d'15'
    addwf   lo,1
    ; 7. loop
    bra     TFT_dive_compass_ruler_loop

TFT_dive_compass_ruler_lend:    ; loop end
    ; 8. clear the rest of the tick area if D<160
    movlw   d'160'
    cpfslt  hi,1
    bra     TFT_dive_compass_ruler_lend2    ; D >= W
    ; 9. position left to end of display to clear the remaining area
    movlw   d'160'
    movwf   lo
    ; 10. clear it
    rcall TFT_dive_compass_clr_ruler

TFT_dive_compass_ruler_lend2:
    rcall TFT_dive_compass_c_mk
    ; done with the compass ruler, put the labels on the screen
    ; get the RD abck to sub_b
    movff   xRD+0,sub_b+0
    movff   xRD+1,sub_b+1
    ; hi stores the display position
    movlw   d'0'
    movwf   hi
    ; lo stores the last item's display position
    movlw   d'0'
    movwf   lo
    bcf     print_compass_label

    movlw   d'14'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'219' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'219' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_1
    STRCPY_TEXT_PRINT     tSW                 ; yes - print it
dcr_1:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'7'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'267' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'267' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_2
    STRCPY_TEXT_PRINT     tW                 ; yes - print it
dcr_2:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'14'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'309' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'309' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_3
    STRCPY_TEXT_PRINT     tNW                 ; yes - print it
dcr_3:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'6'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'358' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'358' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_4
    STRCPY_TEXT_PRINT     tN                 ; yes - print it
dcr_4:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'13'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'399' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'399' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_5
    STRCPY_TEXT_PRINT     tNE                 ; yes - print it
dcr_5:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'6'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'448' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'448' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_6
    STRCPY_TEXT_PRINT     tE                 ; yes - print it
dcr_6:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'13'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'489' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'489' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_7
    STRCPY_TEXT_PRINT     tSE                 ; yes - print it
dcr_7:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'6'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'538' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'538' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_8
    STRCPY_TEXT_PRINT     tS                 ; yes - print it
dcr_8:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'14'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'579' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'579' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_9
    STRCPY_TEXT_PRINT     tSW                 ; yes - print it
dcr_9:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'7'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'627' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'627' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_10
    STRCPY_TEXT_PRINT     tW                 ; yes - print it
dcr_10:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'14'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'669' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'669' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_11
    STRCPY_TEXT_PRINT     tNW                 ; yes - print it
dcr_11:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

    movlw   d'6'
    movwf   up                  ; up stores the width of hte label
    movlw   low( d'718' )       ; position of the label
    movwf   sub_a+0
    movlw   high( d'718' )
    movwf   sub_a+1
    rcall    TFT_dive_compass_label_proc     ; check if the label should be on screen
    btfss   print_compass_label             ; Yes?
    bra     dcr_12
    STRCPY_TEXT_PRINT     tN                 ; yes - print it
dcr_12:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker

TFT_dive_compass_label_end:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker
    ; restore lo and hi for the final cleanup
    movff   xLO,lo
    movff   xHI,hi
    ; clear the rest of the SQ area if there are more space
    movlw   d'160'
    cpfslt  hi
    bra     TFT_dive_compass_label_end2    ; D >= 160, no more space
    ; position left to end of display to clear the remaining area
    movlw   d'160'
    movff   WREG,lo
    ; clear it
    rcall    TFT_dive_compass_clr_label
TFT_dive_compass_label_end2:
    rcall    TFT_dive_compass_c_mk           ; check if label is on the center line or the marker
    ; do we have bearing set?
    btfsc   compass_bearing_set
    bra     TFT_dive_compass_dir_text   ; bearing_set=1 - go and print the dir (<< or >>)
    rcall    TFT_dive_compass_dir_lclr     ; no, clear the area (e.g. we had but removed)
    rcall    TFT_dive_compass_dir_rclr
    bra     TFT_dive_compass_text

TFT_dive_compass_dir_text:
    ; bearing set, but does it point to heading?
    btfss   compass_bearing_eq
    bra     TFT_dive_compass_dir_text_2   ; bearing != heading - go and print the dir
    rcall    TFT_dive_compass_dir_lclr     ; bearing = heading, no need for direction markers
    rcall    TFT_dive_compass_dir_rclr
    bra     TFT_dive_compass_text

TFT_dive_compass_dir_text_2:
    movlw   color_green
    call    TFT_set_color
    btfsc   compass_bearing_lft
    bra     TFT_dive_compass_dir_ldir      ; bearing_lft=1, print the left marker
;TFT_dive_compass_text_rdir:
    WIN_SMALL   dive_compass_rdir_column,dive_compass_head_row
    STRCPY_PRINT    ">>"
    ; do not forget to clear the left
    rcall    TFT_dive_compass_dir_lclr
    bra     TFT_dive_compass_text

TFT_dive_compass_dir_ldir:
    WIN_SMALL   dive_compass_ldir_column,dive_compass_head_row
    STRCPY_PRINT    "<<"
    ; do not forget to clear the right
    rcall    TFT_dive_compass_dir_rclr
    ;bra     TFT_dive_compass_text

TFT_dive_compass_text:
    ; Text output
    call    TFT_standard_color
    WIN_SMALL   dive_compass_head_column,dive_compass_head_row
    rcall   TFT_surface_compass_heading_com  ; Show "000° N"
    return

TFT_dive_compass_dir_lclr:
    WIN_SMALL   dive_compass_ldir_column,dive_compass_head_row
    STRCPY_PRINT    "  "
    return

TFT_dive_compass_dir_rclr:
    WIN_SMALL   dive_compass_rdir_column,dive_compass_head_row
    STRCPY_PRINT    "  "
    return

TFT_dive_compass_label_proc:
    ; Input:
    ;   xHI:   DD  - display'a current position
    ;   xRD:   RD  - ruler display offset
    ;   sub_a: RP  - item's ruler display offset
    ; get the RD abck to sub_b
    movff   xHI,hi
    bcf     print_compass_label
    ; 1/a. check if it's viewable ? sub_a(RP) >= sub_b(RD) ?
    ;    set the carry flag if sub_b(xRD) is equal to or greater than sub_a(xRP):
    movff   xRD+0,sub_b+0
    movff   xRD+1,sub_b+1
    call    subU16      ;  sub_c = sub_a - sub_b
    btfsc   neg_flag    ; >=0?
    return              ; No
    ; store the RO=RP-RD for drawing
    movff   sub_c+0,xC+0
    movff   sub_c+1,xC+1

    ; 1/b. check if it's viewable ? sub_a(RP)+up(width) < sub_b(RD)+160
    ;      if already above, no need to process the rest of the labels
    ;movff   up,WREG    ; don't worry about the width, low level call prevents overload
    movlw   d'2'        ;   .. but still avoid thin mess on the side of the display
    addwf   sub_a+0,1
    btfsc   STATUS, C
    incf    sub_a+1

    movff   xRDr+0,sub_b+0
    movff   xRDr+1,sub_b+1
    call    subU16      ;  sub_c = sub_a - sub_b
    btfss   neg_flag    ; ? <0
    bra     TFT_dive_compass_label_end      ; No

    ; 2. restore RO=RP-RD from 1/a.
    movff   xC+0,lo

    ; 3. Clear the segment from DD(hi) to lo
    ; don't do a clear if we are at 0 (zero) otherwise it will blink
    ;   ?because of the width underflow?
    movlw   d'0'
    cpfsgt  lo
    bra     TFT_dive_compass_label_proc_p
    rcall   TFT_dive_compass_clr_label
TFT_dive_compass_label_proc_p:
    ; 4. print the SQ on the screen
    call    TFT_standard_color
    bsf     print_compass_label
;TFT_dive_compass_label_print:
    movlw   dive_compass_label_row
    movff   WREG,win_top
    movff   lo,win_leftx2
    movlw   FT_SMALL
    movff   WREG,win_font
    ; 6. retain the new display positions
    movff   hi,divB     ; old-hi will be used by the c_mk : clear+marker printing
    movff   lo,hi
    movff   up,WREG
    addwf   hi,1
    movff   lo,xLO
    movff   hi,xHI
    return

TFT_dive_compass_c_mk:
    ; Common task to draw center line and marker
    ;    until a proper implementation make it simple:
    rcall    TFT_dive_compass_mk
    rcall    TFT_dive_compass_cline
    return

TFT_dive_compass_mk:
    ; draw the bearing on the screen if visible and if we just put something over it
    btfss   compass_bearing_set
    return  ; bearing_set=0 nothing to display

    btfss   compass_bearing_vis
    return  ; bearing set but not visible

    ; save lo/hi from trashing
    movff   lo,xA+0
    movff   hi,xA+1

    ; did we just update the marker's position?
    ;                       DD.......DD
    ;          CM+2>=DD(old)    or     CM-2<=DD
    ; ToDo

    btfss   compass_bearing_ahd
    bra     TFT_dive_compass_mk_rear
;TFT_dive_compass_mk_front:
    clrf    lo
    movff   xCM,lo
    bsf     print_compass_label ; set=green marker
    rcall   TFT_dive_compass_mk_print
    bcf     print_compass_label
    bra     TFT_dive_compass_mk_end

TFT_dive_compass_mk_rear:
    clrf    lo
    movff   xCM,lo
    bcf     print_compass_label ; set=red marker
    rcall   TFT_dive_compass_mk_print

TFT_dive_compass_mk_end:
    movff   xA+0,lo
    movff   xA+1,hi
    return

TFT_dive_compass_mk_print:
    movlw   d'1'
    cpfsgt  lo
    bra     TFT_dive_compass_mk_print_2 ; lo<1, skip the first line
    movlw   d'2'
    subwf   lo,0
;    movff   WREG,win_leftx2
    rcall   TFT_dive_compass_mk_print_3
TFT_dive_compass_mk_print_2:
    movlw   d'2'
    addwf   lo,0
;    rcall   TFT_dive_compass_mk_print_3
;    return
TFT_dive_compass_mk_print_3:
    movff   WREG,win_leftx2
    movlw   dive_compass_label_row
    movff   WREG,win_top
    movlw   dive_compass_label_height-.2
    movff   WREG,win_height
    movlw   d'2'
    movff   WREG,win_width
    movlw   d'2'
    movff   WREG,win_bargraph
    movlw   color_green
    btfss   print_compass_label
    movlw   color_red
    call    TFT_set_color
    call    TFT_box
    return

TFT_dive_compass_clr_label:
    movlw   dive_compass_label_row-.2     ; set top & height
    movff   WREG,win_top
    movlw   dive_compass_label_height+.2
    movff   WREG,win_height
    rcall   TFT_dive_compass_clear
    return

TFT_dive_compass_clr_ruler:
    ; top tick
    movlw   dive_compass_tick_top_top     ; set top & height
    movff   WREG,win_top
    movlw   dive_compass_tick_height
    movff   WREG,win_height
    rcall    TFT_dive_compass_clear
    ;bottom tick
    movlw   dive_compass_tick_bot_top     ; set top & height
    movff   WREG,win_top
    movlw   dive_compass_tick_height
    movff   WREG,win_height
;    rcall   TFT_dive_compass_clear
;    return
TFT_dive_compass_clear:
    ; we receive RM in lo and DD in hi
    ; calculate width = RM-D
    movff   hi,WREG
    subwf   lo,0
    movff   WREG,win_width         ; RM-DD
    movff   WREG,win_bargraph
    incf    hi,W                   ; +1 pixel to avopid clipping of chars
    movff   WREG,win_leftx2
    movlw   color_black
    call    TFT_set_color
    call    TFT_box
    return

TFT_dive_compass_ruler_print:
    ; we receive RM in lo and DD in hi
    movlw   dive_compass_tick_top_top
    movff   WREG,win_top
    movlw   dive_compass_tick_height
    movff   WREG,win_height
    movlw   d'2'
    movff   WREG,win_width
    movlw   d'2'
    movff   WREG,win_bargraph
    movff   lo,win_leftx2          ; 0..159
    call    TFT_standard_color
    call    TFT_box
    movlw   dive_compass_tick_bot_top
    movff   WREG,win_top
    movlw   dive_compass_tick_height
    movff   WREG,win_height
    call    TFT_box
    return

TFT_dive_compass_cline:
   	movlw   color_yellow
    WIN_BOX_COLOR     dive_compass_tick_top_top,dive_compass_tick_bot_bot,.80,.81
    return

tft_compass_cardinal:
    btfsc  hi,0          ; Heading >255°?
    bra     tft_compass_cardinal2   ; Yes must be W, NW or N
    ; No, Must be W, SW, S, SE, E, NE or N
    movlw   .23
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_N
    movlw   .68
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_NE
    movlw   .113
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_E
    movlw   .158
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_SE
    movlw   .203
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_S
    movlw   .248
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_SW
    bra     tft_compass_cardinal_W

tft_compass_cardinal2:
    movlw   .37
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_W
    movlw   .82
    subwf   lo,W
    btfss   STATUS,C
    bra     tft_compass_cardinal_NW
;    bra     tft_compass_cardinal_N
tft_compass_cardinal_N:
    STRCAT_TEXT     tN
    return
tft_compass_cardinal_NE:
    STRCAT_TEXT     tNE
    return
tft_compass_cardinal_E:
    STRCAT_TEXT     tE
    return
tft_compass_cardinal_SE:
    STRCAT_TEXT     tSE
    return
tft_compass_cardinal_S:
    STRCAT_TEXT     tS
    return
tft_compass_cardinal_SW:
    STRCAT_TEXT     tSW
    return
tft_compass_cardinal_W:
    STRCAT_TEXT     tW
    return
tft_compass_cardinal_NW:
    STRCAT_TEXT     tNW
    return

compass_heading_common:
	call	speed_normal
    movlw   compass_averaging           ; numbers of extra averaging
    movwf   up
compass_heading_common2:
    rcall   TFT_get_compass
    decfsz  up,F
    bra     compass_heading_common2
    extern  compass
    call    compass                     ; Do compass corrections.
    banksel common

    ; More then compass_fast_treshold?
    movff   compass_heading_old+0,sub_a+0
    movff   compass_heading_old+1,sub_a+1
    movff   compass_heading+0,sub_b+0
    movff   compass_heading+1,sub_b+1
    call    sub16
    btfss   neg_flag                        ; <0?
    bra     compass_heading_common3         ; No, test for threshold
    ; Yes, subtract the other way round
    movff   compass_heading+0,sub_a+0
    movff   compass_heading+1,sub_a+1
    movff   compass_heading_old+0,sub_b+0
    movff   compass_heading_old+1,sub_b+1
    call    sub16
compass_heading_common3:
    movff   compass_heading+0,compass_heading_old+0 ; copy new "old"
    movff   compass_heading+1,compass_heading_old+1

    bcf     compass_fast_mode
    movlw   compass_fast_treshold
    cpfslt  sub_c+0                             ; > compass_fast_treshold?
    bsf     compass_fast_mode                   ; Yes!

    btfss   compass_fast_mode               ; In fast mode?
    return                                  ; No.
    ; Yes.
    movff	compass_heading+0,lo
    movff	compass_heading+1,hi
    call    TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
    movff   lo,compass_heading_shown+0
    movff   hi,compass_heading_shown+1
    return

TFT_get_compass:
    call    I2C_RX_compass              ; Test Compass
    call    I2C_RX_accelerometer        ; Test Accelerometer
    call    compass_filter              ; Filter Raw compass + accel readings.
    banksel common
    return


        END
