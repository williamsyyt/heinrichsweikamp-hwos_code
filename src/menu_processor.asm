;=============================================================================
;
;   File menu_processor.asm
;
;   Routines to handle all OSTC3 graphic/text menus.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2012-11-02 : [jDG] Cleanup for OSTC3: removed icons. Added scrolling.
;                      But need a font with lower/upper alpha chars...

#include "convert.inc"
#include "ostc3.inc"
#include "strings.inc"
#include "tft.inc"
#include "varargs.inc"
#include "wait.inc"
#include "start.inc"
#include "surfmode.inc"
#include "divemode.inc"
#include "tft_outputs.inc"
#include "eeprom_rs232.inc"
#include "adc_lightsensor.inc"
#include "calibrate.inc"

;NOTE: should be idenric in .inc and .asm !
#define MENU_LINES_MAX  .7              ; Number of lines per screen?
#define MENU_TITLE_FONT WIN_STD         ; Font should contains lower/UPPER alpha
#define MENU_LINE_FONT  WIN_SMALL       ; Font should contains lower/UPPER alpha
#define MENU_LEFT       .20             ; Position of first menu item
#define MENU_HEIGHT     .27             ; Spacing between menu lines.
#define MENU_VCENTER    .125            ; Position on screen.
#define MENU_LINE_MAX_LENGTH    .20     ; Length in characters

; Other needed references
    extern  aa_wordprocessor,option_inc,option_draw,comm_mode


;=============================================================================
; Temporary data.

        CBLOCK  tmp+0x20                ; Reserved space for options.asm
            menu_flags                  ; Various flags for menu:
                                        ;   bit 0 :dynamic menu
            menu_item                   ; Index of the current item.
            start_item                  ; Index of the first item (scrolling)
            item_max                    ; Number of items in menu.
            selected_item               ; Index of the current item.
            value_type                  ; Type for vertical menu.
            dynamic_item:3              ; Callback addr
            menu_block:3                ; Address of the menu block (ie. item 0)
            menu_title:3                ; text or proc for dynamic menu.
            menu_center                 ; centering for line menu.
            proc_item:3                 ; Address of the current proc.
            text_item:2                 ; Address of the current text.
        ; Reserved to tmp+0x35
        ENDC

#define option_item proc_item

basic       CODE
;=============================================================================
; menu handler.
;
; Input:    TBLPTR = addr of menu block.
        global  menu_processor
menu_processor:
        banksel common                  ; Bank1
        btfss   divemode                ; Not in divemode
        call    speed_fastest           ; Make it quick !
		
		;---- Read menu block ------------------------------------------------
        VARARGS_BEGIN                   ; Read inline PROM data
		clrf	STKPTR                  ; Never return, anyway...
        VARARGS_GET8    item_max        ; Get number of items
        VARARGS_GET8    menu_flags      ; Get flags
        VARARGS_GET24   menu_title      ; Get pointer to menu title
        VARARGS_GET8    menu_center     ; Vertical position
        movff   TBLPTRL, menu_block+0   ; Save base address for menu_read_item
        movff   TBLPTRH, menu_block+1
        movff   TBLPTRU, menu_block+2

    extern  TFT_clear_divemode_menu
        btfss   divemode                ; In divemode?
        bra     menu_processor0         ; No

        movlw   .1
        cpfsgt  menupos                 ; only if menupos=1...
        call    TFT_clear_divemode_menu ; ... Clear the menu!

        ; Draw one frame around the divemode menu
        extern  TFT_divemask_color
        call    TFT_divemask_color
    	WIN_FRAME_COLOR16 divemode_menu_row, divemode_menu_lower, divemode_menu_left ,divemode_menu_right	; top, bottom, left, right
        call    TFT_standard_color

        bra     menu_processor1         ; Yes, skip some lines here

menu_processor0:
        ;---- draw menu title ------------------------------------------------
		clrf	CCP1CON					; stop PWM
		bcf		PORTC,2					; Pull PWM out to GND
		call    TFT_ClearScreen
        rcall   menu_processor_title
        rcall   menu_processor_bottom_line

menu_processor1:
		movlw	FT_SMALL
        movff	WREG, win_font    
        
        ;---- Select menu type -----------------------------------------------
        bra     menu_vertical

    global menu_processor_bottom_line
menu_processor_bottom_line:
        ;---- Draw bottomline ------------------------------------------------
        TEXT_TINY 	.5,       .240-.16, tNext
        TEXT_TINY	.160-6*5, .240-.16, tEnter

        WIN_COLOR   color_greenish
        ; Serial and Firmware Version
        WIN_TINY	.57,.240-.16
        STRCPY  "#"
        call    TFT_cat_serial
        STRCAT  " v"
        call    TFT_cat_firmware
        STRCAT_PRINT ""
        call    TFT_standard_color
        return

;=============================================================================
; (re-)draw menu title.
;
menu_processor_title:        
        WIN_BOX_BLACK  .2,.23,.0,.160	; Clear Menu title
        MENU_TITLE_FONT	.0, .2        	; Menu title positionning

        btfss   menu_flags,0            ; Static or dynmic title ?
        bra     menu_processor_static_title

        rcall   menu_processor_call_title ; add gas, detail and color.
        bra     menu_processor_title_1

menu_processor_static_title:                
        movff   menu_title+0,FSR1L      ; Just copy string.
        movff   menu_title+1,FSR1H
        call    strcpy_text

menu_processor_title_1:
        WIN_COLOR   color_greenish
        movf    FSR2L,W                 ; Get title length
        mullw   .9                      ; Convert to half pixels
		bcf		STATUS,C				; Clear carry
		rrcf	PRODL					; /2
        movf    PRODL,W                 ; Back to WREG
        sublw   .80                    	; 80 - width
        movwf   win_leftx2              ; Aligned to center.
        
        call    aa_wordprocessor
        call    TFT_standard_color
        return

;=============================================================================
; Call dynamic proc for menu title:

menu_processor_call_title:
        movff   menu_title+2,PCLATU     ; Just execute computed goto.
        movff   menu_title+1,PCLATH
        movf    menu_title+0,W
        movwf   PCL
        
;=============================================================================
; Restart with first icon/line selected.
        global  menu_processor_reset
menu_processor_reset:
        banksel common
        lfsr    FSR2,menustack
        clrf    POSTINC2
        clrf    POSTINC2
        clrf    POSTINC2
        clrf    POSTINC2
        clrf    POSTINC2
        clrf    selected_item
        return

        global  menu_processor_pop
menu_processor_pop:
        movff   menustack+0,selected_item
        movff   menustack+1,menustack+0
        movff   menustack+2,menustack+1
        movff   menustack+3,menustack+2
        movff   menustack+4,menustack+3
        return

menu_processor_push:
        movff   menustack+3,menustack+4
        movff   menustack+2,menustack+3
        movff   menustack+1,menustack+2
        movff   menustack+0,menustack+1
        movff   selected_item,menustack+0
        clrf    selected_item
        return

;---- Execute menu selection -------------------------------------------------
do_menu_item:
        bcf     switch_right            ; Avoid loops.
        call    speed_normal            ; Back to normal speed.
        
        movf    selected_item,W         ; Reread proc address from table.
        rcall   menu_read_item          ; (destroy PROD)
    
        movff   selected_item,PRODL     ; Pass along selected line
    
        rcall   menu_processor_push     ; Remember where we get from. (clears selected_item)
    
        movff   proc_item+2,PCLATU      ; Then execute computed goto.
        movff   proc_item+1,PCLATH
        movf    proc_item+0,W
        movwf   PCL

;=============================================================================
; Get current item from table.
;
; Input : Item number in WREG, menu_block.
;
; Output: icon_large, text_item, proc_item 16bits pointers.
;
; Trashed: PROD, WREG
menu_read_item:
        mullw   .10                     ; 10 bytes per item.
        movf    PRODL,W                 ; Then do a 24bits add
        addwf   menu_block+0,W          ; with menu_block, and
        movwf   TBLPTRL                 ; setup TBLPTR
        movf    PRODH,W
        addwfc  menu_block+1,W
        movwf   TBLPTRH
        movlw   0
        addwfc  menu_block+2,W
        movwf   TBLPTRU

        VARARGS_GET8    value_type      ; Read 10 bytes of item data
        VARARGS_GET24   dynamic_item
        VARARGS_GET24   proc_item
        VARARGS_GET8    WREG            ; Skip dummy byte
        VARARGS_GET16   text_item

        return

;=============================================================================
; Vertical menu : set of line/value to choose from.
; Entry point to update lines already shown.
;
        global   menu_vertical
menu_vertical:
        btfss   divemode                ; Not in divemode
		clrf	timeout_counter2		; Reset timeout

menu_vertical_2:
        rcall   menu_draw_lines			; Always re-draw whole menu

		movlw	CCP1CON_VALUE			; See ostc3.inc
        btfss   divemode                ; Not in divemode
		movwf	CCP1CON                 ; Power-on backlight

menu_vertical_1:
        movf    selected_item,W         ; Get current item data
        rcall   menu_read_item
        movf    proc_item+0,W           ; Check if pro address is NULL ?
        iorwf   proc_item+1,W
        bz      next_line_menu          ; YES: not selectable !

        btfss   divemode                ; Not in divemode
        rcall   menu_draw_selected_line

        btfsc   in_color_menu           ; =1: In the color scheme menu
        call    TFT_show_color_schemes  ; Yes, update the color schemes

		extern	rtc_set_rtc
		btfss	settime_setdate			; In the Set Time or Set Date menu?
		bra		menu_line_loop_pre2		; no, skip all following
		
		movff	month,lo		; new month
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.28
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.30
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.30
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.30
		dcfsnz	lo,F
		movlw	.31
		dcfsnz	lo,F
		movlw	.30
		dcfsnz	lo,F
		movlw	.31
		cpfsgt	day						; day ok?
		bra		menu_line_loop_pre1		; OK!
		movlw	.1						; not OK, set to 1st
		movwf	day

menu_line_loop_pre1:
		btfsc	switch_right			; Enter pressed?
		call	rtc_set_rtc				; Yes, update mins,sec,hours,day,month and year to rtc module
		call	TFT_show_time_date_menu	; Update clock

menu_line_loop_pre2:
        bcf     switch_right
        bcf     switch_left
        btfss   divemode                ; Not in Divemode
        call    speed_normal

menu_line_loop_pre3:
    extern  divemode_option0_return
        btfsc   divemode                ; In divemode?
        goto    divemode_option0_return ; Yes, return to it

menu_line_loop:
        btfsc   switch_right
        bra     do_line_menu            ; Type dependent
        btfsc   switch_left
        bra     next_line_menu

        btfss	quarter_second_update   ; 1/4 second?
        bra     menu_line_loop1         ; Not yet...

        call    compute_ppo2			; compute mv_sensorX and ppo2_sensorX arrays
        btfsc   menu_show_sensors2      ; In the "Calibrate" menu?
        call    TFT_menu_hud2           ; Yes, update mV data
        bcf     quarter_second_update   ; Clear flag

menu_line_loop1:
		btfss	onesecupdate			; New second
		bra		menu_line_loop2			; not yet...

		call	timeout_surfmode		; timeout
		call	set_dive_modes			; check, if divemode must be entered
        call	get_battery_voltage		; gets battery voltage
		
		btfsc	settime_setdate			; In the Set Time or Set Date menu?
		call	TFT_show_time_date_menu	; Yes, update clock

        btfsc   menu_show_sensors       ; In the "Sensors" menu?
        call    TFT_menu_hud            ; Yes, update HUD data

		bcf		onesecupdate			; one second updates done

menu_line_loop2:
		btfsc	sleepmode				; Timeout?
		goto	restart					; Yes, back to surfacemode
		btfsc	divemode
		goto	restart					; Enter Divemode if required

        btfsc   enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
        bra     menu_line_loop3
        btfsc   vusb_in                     ; USB plugged in?
        call    comm_mode                   ; Start COMM mode
        bra     menu_line_loop4
menu_line_loop3:
        btfss   vusb_in                     ; USB (still) plugged in?
        bcf     enable_screen_dumps         ; No, clear flag
        call    rs232_get_byte
        btfsc   rs232_recieve_overflow
        bra     menu_line_loop4
        movlw   "l"
        cpfseq	RCREG1
        bra     menu_line_loop4
        call    TFT_dump_screen             ; Dump the screen contents
menu_line_loop4:

        bra     menu_line_loop

;---- Move to menu's next line
next_line_menu:
        btfss   divemode                ; not in divemode
        call    speed_fastest
        bcf     switch_left             ; Avoid looping.

        incf    selected_item,F         ; Select next item.
        movf    selected_item,W         ; Index == max ?
        cpfseq  item_max
        bra     menu_vertical_1         ; NO: redraw cursor.

        clrf    selected_item           ; YES: restart for item 0.
        bra     menu_vertical_1         ; Then redraw cursor.

        global  do_line_menu
do_line_menu:
        btfss   divemode                ; not in divemode
        call    speed_fastest
;        bcf     switch_right            ; Avoid looping.

        decf    menupos,W               ; menu_processor needs 0-5...
        btfsc   divemode                ; only in divemode
        movwf   selected_item

        movf    selected_item,W         ; Read selected descriptor
        rcall   menu_read_item

        movf    value_type,W            ; Switch on data type
        bz      menu_do_line_call       ; CALL
        dcfsnz  WREG
        bra     menu_do_line_call       ; STRING: do as call
        dcfsnz  WREG
        bra     menu_do_line_option     ; OPTION
        dcfsnz  WREG
        bra     menu_do_line_call       ; DYNAMIC: do as call
        bra     menu_line_loop_pre3     ; else do nothing...

;---- CALL
menu_do_line_call:
        rcall   do_menu_item            ; Same as icon menu: calculated goto.
        rcall   menu_processor_pop      ; Back to same line,
        bra     menu_vertical           ; Then continue into menu...

;---- Call option specific increment subroutine
menu_do_line_option:
        movff   option_item+0,FSR0L     ; Get option handle
        movff   option_item+1,FSR0H
        call    option_inc              ; increment
        
        movff   selected_item,PRODL     ; Pass selection to callback.
        rcall   menu_text_call        
        bra     menu_vertical_2         ; redraw all lines...

;-----------------------------------------------------------------------------

menu_draw_lines_divemode:
        movlw   divemode_menu_item1_row
        movff   WREG,win_top
        movlw   divemode_menu_item1_column
        movff   WREG,win_leftx2
        clrf    start_item
        movff   item_max,menupos4           ; Copy item_max for divemode cursor routine
        bra     menu_draw_lines_2

menu_draw_lines:
        btfsc   divemode                    ; in divemode?
        bra     menu_draw_lines_divemode    ; Yes

        btfsc   menu_flags,0            ; Dynamic title ?
        rcall   menu_processor_title    ; YES: redraw it then.

        MENU_LINE_FONT  MENU_LEFT, 0    ; Init start position/font
        movff   menu_center,win_top     ; computed in menu block.

        ; Does the menu have more than 6 lines ?
        movf    item_max,W
        addlw   -(MENU_LINES_MAX+1)     ; (max - 7)
        bnn     menu_draw_long_menu     ; bra if (max >= 7)

        clrf    start_item
        bra     menu_draw_lines_2

menu_draw_long_menu:
        movf    selected_item,W         ; Start at selected-6
        addlw   -(MENU_LINES_MAX-1)
        btfsc   STATUS,N                ; This is <0 ?
        clrf    WREG                    ; YES: start from top instead.
        movwf   start_item

menu_draw_lines_2:
        movff   start_item, menu_item

menu_draw_lines_1:
        call    TFT_standard_color     ; Restore color after disabled lines.
        
        movf    menu_item,W
        rcall   menu_read_item
        
        movf    value_type,W            ; Switch on data type
        bz      menu_draw_line_call
        dcfsnz  WREG
        bra     menu_draw_line_string
        dcfsnz  WREG
        bra     menu_draw_line_option
        dcfsnz  WREG
        bra     menu_draw_line_dynamic
        bra     menu_draw_line_none

menu_draw_line_string:
        movff   text_item+0,TBLPTRL     ; Read not-translated string from PROM.
        movff   text_item+1,TBLPTRH
        call    strcpy_prom             ; Copy in buffer
        bra     menu_draw_line_none
        
menu_draw_line_call:
        movff   text_item+0,FSR1L       ; Read string from PROM.
        movff   text_item+1,FSR1H
        call    strcpy_text             ; Copy in buffer

        bra     menu_draw_line_none
        
menu_draw_line_option:
        movff   text_item+0,FSR1L       ; Read string from PROM.
        movff   text_item+1,FSR1H
        call    strcpy_text             ; Copy in buffer

        movff   option_item+0,FSR0L     ; Retrieve option handle.
        movff   option_item+1,FSR0H
        btfss   settime_setdate         ; Not in Time/Date menu...
        call    option_draw
        bra     menu_draw_line_none

menu_draw_line_dynamic:
        lfsr    FSR2,buffer
        movff   menu_item,PRODL         ; Pass item to callback.
        rcall   menu_text_call          ; Push return address.
        bra     menu_draw_line_none

; Computed goto to pointer inside dynamic_item:
menu_text_call:
        movf    dynamic_item+0,W        ; Check if callback is NULL
        iorwf   dynamic_item+1,W
        iorwf   dynamic_item+2,W
        btfsc   STATUS,Z
        return                          ; YES: don't call it.

        movff   dynamic_item+2,PCLATU   ; Prepare...
        movff   dynamic_item+1,PCLATH
        movf    dynamic_item+0,W
        movwf   PCL                     ; And jump !

menu_draw_line_none:
        extern  TFT_fillup_with_spaces

        btfsc   divemode                        ; In divemode?
        bra     menu_draw_line_none_divemode    ; Yes
        
        movlw   MENU_LINE_MAX_LENGTH
        call    TFT_fillup_with_spaces  ; Fillup FSR2 with spaces (Total string length in #WREG)
        clrf    WREG
        movff   WREG,buffer+MENU_LINE_MAX_LENGTH;No   ; Make sure won't be longer than MENU_LINE_MAX_LENGTH ch
        call    aa_wordprocessor
        movlw   MENU_HEIGHT             ; No, Move to next line
        addwf   win_top,F
        incf    menu_item,F             ; inc loop counter

        movf    start_item,W            ; First line (scrolled)
        subwf   menu_item,W             ; current - first
        xorlw   MENU_LINES_MAX          ; Already done 6 lines ?
        btfsc   STATUS,Z
        return                          ; YES
menu_draw_line_none2:
        movf    menu_item,W             ; Done item_max lines ?
        xorwf   item_max,W
        btfss   STATUS,Z
        bra     menu_draw_lines_1       ; No: loop...
        return

menu_draw_line_none_divemode:
        movlw   .10
        call    TFT_fillup_with_spaces  ; Fillup FSR2 with spaces (Total string length in #WREG)
        clrf    WREG
        movff   WREG,buffer+.10

        call    aa_wordprocessor        ; Draw the line!

        movlw   .0
        movff   WREG,win_invert         ; Reset invert flag

        movlw   .24                     ; Divemode menu spacing
        addwf   win_top,F
        incf    menu_item,F             ; inc loop counter

        movlw   .3
        cpfseq  menu_item               ; At pos 4?
        bra     menu_draw_line_none2    ; No

        movlw   divemode_menu_item4_row
        movff   WREG,win_top            ; Reset row
        movlw   divemode_menu_item4_column
        movff   WREG,win_leftx2         ; New column
        bra     menu_draw_line_none2    ; Done.

;-----------------------------------------------------------------------------
; Put a mark in front of the current line
menu_draw_selected_line:
		clrf	timeout_counter2		; Reset timeout
        WIN_BOX_BLACK .34,.221,MENU_LEFT-8,MENU_LEFT-2  ; Clear left column
        
        call    TFT_standard_color
        WIN_SMALL  MENU_LEFT-8, 0       ; Arrow symbol only in small font
        movf    start_item,W            ; First line (scrolled)
        subwf   selected_item,W         ; selected - first
        mullw   MENU_HEIGHT             ; 30 pixel by line
        movf    PRODL,W                 ; result
        addwf   menu_center,W           ; added to first line
        movwf   win_top                 ; and stored to pos.
        STRCPY_PRINT    "\xb7"          ; print cursor

        return

        END
