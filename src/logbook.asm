;=============================================================================
;
;   File logbook.asm
;
;   Logbook
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2011-11-12 : [mH] moving from OSTC code

;=============================================================================
; Temp data, local to this module, moved to ACCES0 area.
;

#include    "hwos.inc"                  ; Mandatory header
#include	"tft.inc"
#include	"external_flash.inc"
#include	"math.inc"
#include	"strings.inc"
#include	"convert.inc"
#include	"tft_outputs.inc"
#include	"eeprom_rs232.inc"
#include 	"menu_processor.inc"
#include	"wait.inc"
#include	"start.inc"
#include	"surfmode.inc"
#include	"divemode.inc"
#include	"ghostwriter.inc"

   	extern   do_main_menu2,comm_mode

    CBLOCK tmp+0x36		        ; Keep space for menu processor.
        count_temperature       ; Current sample count for temperature divisor
        count_deco              ; Current sample count for deco (ceiling) divisor
        logbook_cur_depth:2     ; Current depth, for drawing profile.
        logbook_cur_tp:2        ; Current temperature, for drawing profile.
        logbook_last_tp         ; Y of the last item in Tp� curve.
        logbook_min_tp:2        ; Min temperature, for drawing profile.
        logbook_max_tp:2        ; Maximum temperature, for drawing profile.
        logbook_ceiling         ; Current ceiling, for drawing profile.
	logbook_flags		; Flags only used in logbook.asm
	logbook_page_number	; Page# in logbook
	logbook_divenumber	; # of dive in list during search
	logbook_divenumber_temp	; # of dive in list during search
	logbook_menupos_temp	; Last position of cursor
	profile_temp:2		; Temp for profile display
	profile_temp2:2		; Temp for profile display
	logbook_max_dive_counter	; Counts dive# to zero 
	logbook_max_dive_counter_temp; Counts dive# to zero 
	logbook_sample_counter:2; Amount of read samples
	divemins_backup		; Backup of divemins+0
	y_scale:2		; y-scale (The horizontal lines)
	x_scale:2		; x-scale (The vertical lines)
	logbook_pixel_x_pos	; x2 position of current pixel in X-direction
        logbook_min_temp_pos    ; lowest row in the temp graph
        logbook_max_temp_pos    ; lowest row in the temp graph
		;+.33, reserved to tmp+0x57
    ENDC

	#DEFINE	return_from_profileview	logbook_flags,0
	#DEFINE	all_dives_shown			logbook_flags,1
	#DEFINE	logbook_page_not_empty	logbook_flags,2
	#DEFINE	end_of_profile			logbook_flags,3
    #DEFINE keep_cursor_new_page    logbook_flags,4
    #DEFINE log_marker_found        logbook_flags,5

	
; Logbook Coordinates
	#DEFINE	logbook_list_left	.10			; Column of dive# in list
	#DEFINE	logbook_row_offset	.28			; Distance between rows of list
	#DEFINE	logbook_row_number	.6			; Amount of rows in the list

; Profile display
	#DEFINE	profile_height_pixels	.157	; Amount of pixels height for profile display
	#DEFINE	profile_width_pixels	.156	; Amount of pixels width for profile display
	#DEFINE	profile_left			.1		; Left border
	#DEFINE	profile_top				.65		; Top border

; "Bailout"
    #DEFINE logbook_bailout_column      .124
    #DEFINE logbook_bailout_row         .207

; Dive number
	#DEFINE	logbook_divenumer_column	.1
	#DEFINE	logbook_divenumer_row		.1
; Date and Time
	#DEFINE	logbook_date_column			.100
	#DEFINE logbook_date_row			.7
	#DEFINE	logbook_time_column			.120
	#DEFINE logbook_time_row			.38
; Max. Depth
	#DEFINE	log_max_value_row		.38
	#DEFINE	log_max_value_column	.1
; Divetime 
	#DEFINE	log_divetime_value_row		.38
	#DEFINE	log_divetime_value_column	.65
; Gaslist below profile
	#DEFINE	log_gas_row			.225
	#DEFINE	log_gas_column1		.0
	#DEFINE	log_gas_column2		log_gas_column1+(.1*.32)
	#DEFINE	log_gas_column3		log_gas_column1+(.2*.32)
	#DEFINE	log_gas_column4		log_gas_column1+(.3*.32)
	#DEFINE	log_gas_column5		log_gas_column1+(.4*.32)

; Logbook Page2
    ; Gaslist
    #DEFINE     log2_title_row1     .20
    #DEFINE     log2_title_column   .90
    #DEFINE     log2_gas_column     log2_title_column
    #DEFINE     log2_gas_row1       .36
    #DEFINE     log2_gas_row2       1*.16+log2_gas_row1
    #DEFINE     log2_gas_row3       2*.16+log2_gas_row1
    #DEFINE     log2_gas_row4       3*.16+log2_gas_row1
    #DEFINE     log2_gas_row5       4*.16+log2_gas_row1

    ; Setpoint List
    #DEFINE     log2_title_sp_row   .130
    #DEFINE     log2_sp_row1        .146
    #DEFINE     log2_sp_row2        1*.16+log2_sp_row1
    #DEFINE     log2_sp_row3        2*.16+log2_sp_row1
    #DEFINE     log2_sp_row4        3*.16+log2_sp_row1
    #DEFINE     log2_sp_row5        4*.16+log2_sp_row1

    ; Details list
    #DEFINE     log2_salinity_row       .55
    #DEFINE     log2_salinity_column    .2
    #DEFINE     log2_cns_row            .1*.16+log2_salinity_row
    #DEFINE     log2_cns_column         log2_salinity_column
    #DEFINE     log2_avr_row            .2*.16+log2_salinity_row
    #DEFINE     log2_avr_column         log2_salinity_column
    #DEFINE     log2_decomodel2_row     .3*.16+log2_salinity_row
    #DEFINE     log2_decomodel2_column  log2_salinity_column
    #DEFINE     log2_decomodel3_row     .4*.16+log2_salinity_row
    #DEFINE     log2_decomodel3_column  log2_salinity_column
    #DEFINE     log2_decomodel_row      .5*.16+log2_salinity_row
    #DEFINE     log2_decomodel_column   log2_salinity_column
    #DEFINE     log2_firmware_row       .6*.16+log2_salinity_row
    #DEFINE     log2_firmware_column    log2_salinity_column
    #DEFINE     log2_battery_row        .7*.16+log2_salinity_row
    #DEFINE     log2_battery_column     log2_salinity_column
    #DEFINE     log2_divemode_row       .8*.16+log2_salinity_row
    #DEFINE     log2_divemode_column    log2_salinity_column
    #DEFINE     log2_lastdeco_row       .9*.16+log2_salinity_row
    #DEFINE     log2_lastdeco_column    log2_salinity_column
; Air pressure
	#DEFINE     MBAR_row				.10*.16+log2_salinity_row
	#DEFINE     MBAR_column				log2_salinity_column


; Header coordinates
    #DEFINE log_profile_version  .8
    #DEFINE log_date            .12
    #DEFINE log_time            .15
    #DEFINE log_max_depth       .17
    #DEFINE log_divetime        .19
    #DEFINE log_min_temp        .22
    #DEFINE log_surface_press   .24
    #DEFINE log_desattime       .26
    #DEFINE log_gas1            .28
    #DEFINE log_gas2            .32
    #DEFINE log_gas3            .36
    #DEFINE log_gas4            .40
    #DEFINE log_gas5            .44
    #DEFINE log_firmware        .48
    #DEFINE log_battery         .50
    #DEFINE log_samplingrate    .52
    #DEFINE log_cns_start       .53
    #DEFINE log_gf_start        .55
    #DEFINE log_gf_end          .56
    #DEFINE log_batt_info	.59
    #DEFINE log_sp1             .60
    #DEFINE log_sp2             .62
    #DEFINE log_sp3             .64
    #DEFINE log_sp4             .66
    #DEFINE log_sp5             .68
    #DEFINE log_salinity        .70
    #DEFINE log_cns_end         .71
    #DEFINE log_avr_depth       .73
    #DEFINE log_total_seconds   .75
    #DEFINE log_gf_lo           .77
    #DEFINE log_sat_mult        .77
    #DEFINE log_gf_hi           .78
    #DEFINE log_desat_mult      .78
    #DEFINE log_decomodel       .79
    #DEFINE log_total_dives     .80
    #DEFINE log_divemode        .82
    #DEFINE log_last_stop       .243

LOG_POINT_TO    macro   address
    movlw   address
    movwf   ext_flash_address+0
    endm

;=============================================================================

logbook code

TFT_logbook_cursor:
;	call		speed_fastest
    WIN_BOX_BLACK   .0, .240-.16, logbook_list_left-.8, logbook_list_left-.1		;top, bottom, left, right

	WIN_LEFT	logbook_list_left-.8
	WIN_FONT 	FT_SMALL
;	bcf     win_invert              ; Reset invert flag
	call	TFT_standard_color

	movff	menupos,temp1
	dcfsnz	temp1,F
	movlw	d'0'
	dcfsnz	temp1,F
	movlw	logbook_row_offset
	dcfsnz	temp1,F
	movlw	.2*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.3*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.4*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.5*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.6*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.7*logbook_row_offset
	dcfsnz	temp1,F
	movlw	.8*logbook_row_offset

	movwf	win_top
	STRCPY_PRINT "\xB7"
	return

	global	logbook
logbook:
	clrf		logbook_flags
    clrf        CCP1CON                     ; stop PWM
    bcf         PORTC,2                     ; Pull PWM out to GND
    call		TFT_ClearScreen				; Clear screen
;	call		TFT_standard_color
	clrf		menupos3					; Here: used rows on current logbook-page	
	clrf		logbook_page_number			; Here: # of current displayed page
	clrf		logbook_divenumber			; # of dive in list during search
	clrf		divemins+0					; Here: used as temp variables
	clrf		divemins+1
	clrf		timeout_counter2			; For timeout
	movlw		logbook_row_number
	movwf		menupos						; Here: stores current position on display (logbook_row_number-x)
	read_int_eeprom .2						; Get low-byte of total dives
	movff		EEDATA,logbook_max_dive_counter

;-----------------------------------------------------------------------------	
; display dive headers backwards from read_int_eeprom .2 = lo-1
; 1st: 200000h-200FFFh -> lo=0
; 2nd: 201000h-201FFFh -> lo=1
; 3rd: 202000h-202FFFh -> lo=2
; 256: 2FF000h-2FFFFFh -> lo=255 (And hi>0...)
; Stop when
; a) no dive is stored (no valid header found)
; b) current dive has no valid header (Number of stored dives < 256)
; c) when 255 dives are reached divemins+0 = 255

logbook2:
;	call		speed_fastest
	incf		divemins+0,F		; increase dive counter
    incf        divemins+0,W        ; = 0x..FF ?
    bz          logbook_reset      ; Yes: FF --> loop.

	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> logbook_max_dive_counter=0
	; 2nd: 201000h-201FFFh -> logbook_max_dive_counter=1
	; 3rd: 202000h-202FFFh -> logbook_max_dive_counter=2
	; 256: 2FF000h-2FFFFFh -> logbook_max_dive_counter=255 (And hi>0...)

	decf		logbook_max_dive_counter,F	; -1

	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	movlw		0x20
	movwf		ext_flash_address+2
	movlw		.16
	mulwf		logbook_max_dive_counter; logbook_max_dive_counter*16 = offset to 0x2000 (up:hi)
	movf		PRODL,W
	addwf		ext_flash_address+1,F
	movf		PRODH,W
	addwfc		ext_flash_address+2,F
	; pointer at the first 0xFA of header

	call		ext_flash_byte_read		; Reads one byte@ext_flash_address:3 into WREG and temp1
	movwf		temp1
	movlw		0xFA
	cpfseq		temp1					; 0xFA found?
	bra			logbook3b				; No, abort

	incf		logbook_divenumber,F    ; new header found, increase logbook_divenumber
	bra			logbook4           		; Done with searching, display the header!

logbook3b:
	btfss		logbook_page_not_empty	; Was there at least one dive?
	bra         exit_logbook            ; Not a single header was found, leave logbook.
	bra			logbook_display_loop2

logbook_reset:
	tstfsz		logbook_divenumber		; Was there at least one dive?
	bra			logbook_reset2
	bra			logbook3b				; No, Nothing to do

logbook_reset2:
	bsf			all_dives_shown			; Yes
	bra			logbook_display_loop2	; Continue

logbook4:
	btfsc		all_dives_shown			; All dives displayed?
	bra			logbook_display_loop2	; Yes, but display first page again.

	call 		display_listdive		; display short header for list on current list position

	movlw		logbook_row_number
	cpfseq		menupos					; first dive on list (top place)?
	bra			logbook_display_loop1	; no, so skip saving of address

; store all registered required to rebuilt the current logbookpage after the detail/profile view
	movff		logbook_divenumber,logbook_divenumber_temp		; # of dive in list of the current page
	movff		divemins+0,divemins_backup						; amount of dives drawn until now
	movff		logbook_max_dive_counter,logbook_max_dive_counter_temp	; backup Counter

logbook_display_loop1:
	decfsz		menupos,F					; List full?
	bra			logbook2					; no, search another dive for our current logbook page

logbook_display_loop2:
	btfss		logbook_page_not_empty		; Was there one dive at all?
	bra			logbook						; Yes, so reload the first page

	; TFT_mask...

	WIN_LEFT	logbook_list_left
	WIN_TOP		logbook_row_offset*logbook_row_number
	STRCPY_TEXT_PRINT tNextLog				; "Next Page"
	WIN_LEFT	logbook_list_left
	WIN_TOP		logbook_row_offset*(logbook_row_number+.1)
	STRCPY_TEXT_PRINT tExit                 ; "Exit"

	movlw		d'1'						; Set cursor to position 1...
	btfsc		return_from_profileview		; .. unless we are returning from a detail/profile view
	movf		logbook_menupos_temp,W		; load last cursor position again
	movwf		menupos						; and set menupos byte
	bcf			return_from_profileview		; Do this only once while the page is loaded again!

	bcf			logbook_page_not_empty			; Obviously the current page is NOT empty

    movlw		d'7'						; Set cursor to position 7...
    btfsc       keep_cursor_new_page        ; ... if we came from the "new Page" line
    movwf       menupos						; and set menupos byte
    bcf         keep_cursor_new_page

	call		TFT_logbook_cursor          ; Show the cursor

    call        logbook_preloop_tasks       ; Clear some flags and set to Speed_eco
    call        menu_processor_bottom_line  ; Show bottom line
logbook_loop:
    btfsc		switch_left					; SET/MENU?
	goto		next_logbook3				; adjust cursor or create new page
    btfsc		switch_right				; ENTER?
	bra			display_profile_or_exit		; view details/profile or exit logbook

    rcall       log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
	btfsc		sleepmode					; Timeout?
	bra         exit_logbook                ; Yes

	bra         logbook_loop                ; Wait for something to do

display_profile_or_exit:
	movlw		logbook_row_number+.2		; exit?
	cpfseq		menupos
	bra			display_profile_or_exit2	; No, check for "Next Page"

exit_logbook:
;    call        TFT_DisplayOff
;    call        TFT_boot
	goto		do_main_menu2

display_profile_or_exit2:
	movlw		logbook_row_number+.1		; Next page?
	cpfseq		menupos
	bra			display_profile				; No, show details/profile
	goto		next_logbook2				; Next page!

display_profile:
    bcf         is_bailout
    bcf         gas6_changed                ; Clear event flags
;	call    	speed_fastest
	movff		menupos,logbook_menupos_temp; store current cursor position
	bsf			return_from_profileview		; tweak search routine to exit after found

	movf		logbook_page_number,W		; Number of page
	mullw		logbook_row_number
	movf		PRODL,W						
	addwf		menupos,W					; page*logbook_row_number+menupos=
	movwf		divesecs					; # of dive to show

display_profile2:
;	call		speed_fastest
    clrf        CCP1CON                     ; stop PWM
    bcf         PORTC,2                     ; Pull PWM out to GND
    call		TFT_ClearScreen				; Clear screen
; Set ext_flash pointer to "#divesecs-oldest" dive
; compute read_int_eeprom .2 - divesecs
; Read required header data for profile display
; look in header for pointer to begin of diveprofile (Byte 2-4)
; Set pointer (ext_flash_log_pointer:3) to this address, start drawing
	
	decf	divesecs,F		;-1
	read_int_eeprom .2
	movf	EEDATA,W
	bcf		STATUS,C
	subfwb	divesecs,W		; max. dives (low value) - divesecs
	movwf	lo				; result
	incf	divesecs,F		;+1
	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> lo=0
	; 2nd: 201000h-201FFFh -> lo=1
	; 3rd: 202000h-202FFFh -> lo=2
	; 256: 2FF000h-2FFFFFh -> lo=255 (And hi>0...)
	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	movlw		0x20
	movwf		ext_flash_address+2
	movlw		.16
	mulwf		lo					; lo*16 = offset to 0x2000 (up:hi)
	movf		PRODL,W
	addwf		ext_flash_address+1,F
	movf		PRODH,W
	addwfc		ext_flash_address+2,F
	; pointer at the first 0xFA of header

	; Now, show profile
    LOG_POINT_TO    log_samplingrate
	call	ext_flash_byte_read			; Read sampling rate
	movff	temp1,samplesecs_value		; needed later...

    LOG_POINT_TO    .2
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+0
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+1
	call		ext_flash_byte_read_plus		; Read start address of profile
	movff		temp1,ext_flash_log_pointer+2
	

	clrf		logbook_sample_counter+0
	clrf		logbook_sample_counter+1				; holds amount of read samples

	call		TFT_standard_color
    call        logbook_show_divenumber             ; Show the dive number in medium font

	WIN_SMALL	logbook_date_column, logbook_date_row
    LOG_POINT_TO    log_date
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+2		; Year
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+0		; Month
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+1		; Day
	call		TFT_convert_date				; converts into "DD/MM/YY" or "MM/DD/YY" or "YY/MM/DD" in postinc2
	STRCAT_PRINT	""

	WIN_SMALL	logbook_time_column, logbook_time_row
	call		ext_flash_byte_read_plus		; hour
	movff		temp1,lo
	call		ext_flash_byte_read_plus		; Minutes
	movff		temp1,hi
	output_99x							; hour
	PUTC		':'
	movff		hi,lo			
	output_99x							; minute
	STRCAT_PRINT	""					; Display 1st row of details

    LOG_POINT_TO    log_profile_version
    call		ext_flash_byte_read_plus	; Profile version
    movlw       0x24
    cpfslt      temp1                       ; <0x24?
    bra         log_skip_extra_icon         ; Yes, skip
    
    WIN_SMALL	logbook_time_column-.8, logbook_time_row
    STRCPY_PRINT	0x94                    ; "End of dive" icon

log_skip_extra_icon:
    LOG_POINT_TO    log_max_depth
	call		ext_flash_byte_read_plus	; read max depth
	movff		temp1,lo				
	call		ext_flash_byte_read_plus	; read max depth
	movff		temp1,hi
	movff		lo,xA+0						; calculate y-scale for profile display
	movff		hi,xA+1
	movlw		profile_height_pixels		; pixel height available for profile
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; does xA/xB=xC
	movff		xC+0,y_scale+0		; holds LOW byte of y-scale   (mbar/pixel!)
	movff		xC+1,y_scale+1		; holds HIGH byte of y-scale  (mbar/pixel!)
	infsnz		y_scale+0,F         ; increase one, because there may be a remainder
	incf		y_scale+1,F
	
	movlw		LOW	((profile_height_pixels+1)*.1000)
	movwf		xC+0
	movlw		HIGH	(((profile_height_pixels+1)*.1000) & h'FFFF')
	movwf		xC+1
	movlw		UPPER	((profile_height_pixels+1)*.1000)
	movwf		xC+2
	clrf		xC+3

	movff		lo,xB+0					; Max. Depth in mbar
	movff		hi,xB+1					; Max. Depth in mbar
	call		div32x16				; xC:4 / xB:2 = xC+3:xC+2 with xC+1:xC+0 as remainder

	movff		xC+0,x_scale+0	; 
	movff		xC+1,x_scale+1	; = Pixels/10m (For scale, draw any xx rows a scale-line)

	movf		x_scale+0,W
	iorwf		x_scale+1,W		; x_scale:2 = Null?
	bnz			display_profile_offset4		; No, continue
	incf		x_scale+1,F		; Yes, make x_scale+1>1 to make "display_profile2e" working

display_profile_offset4:
	WIN_SMALL	log_max_value_column,log_max_value_row

	TSTOSS		opt_units			; 0=Meters, 1=Feets
	bra			display_profile_offset4_metric
;display_profile_offset4_imperial:
	call		convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
	PUTC	' '
	bcf			leftbind
    output_16_3                             ; limit to 999 and display only (0-999)
	STRCAT_TEXT_PRINT    tFeets
	bra			display_profile_offset4_common

display_profile_offset4_metric:
	bsf			leftbind
	output_16dp	d'3'					; max. depth
	STRCAT_TEXT_PRINT   tMeters

display_profile_offset4_common:	
	call		ext_flash_byte_read_plus				; divetime in minutes	
	movff		temp1,lo
	call		ext_flash_byte_read_plus	
	movff		temp1,hi				; divetime in minutes

	movff		lo,xA+0					; calculate x-scale for profile display
	movff		hi,xA+1					; calculate total diveseconds first
	movlw		d'60'					; 60seconds are one minute...
	movwf		xB+0
	clrf		xB+1
	call		mult16x16				; result is in xC:2 !

	WIN_SMALL	log_divetime_value_column,log_divetime_value_row
	bsf			leftbind
	output_16							; divetime minutes
	movlw		LOW		d'600'
	movwf		xA+0
	movlw		HIGH	d'600'
	movwf		xA+1					; A vertical line every 600 seconds
	movff		samplesecs_value,xB+0		; Copy sampling rate
	clrf		xB+1
	call		div16x16				; xA/xB=xC with xA as remainder
	movff		xC+0,average_depth_hold_total+0
	movff		xC+1,average_depth_hold_total+1
	;average_depth_hold_total:2 holds interval of samples for vertical 10min line

; Restore divetime in minutes:
; get real sample time
    LOG_POINT_TO    log_total_seconds
	call		ext_flash_byte_read_plus				; Total sample time in seconds
	movff		temp1,xC+0
	call		ext_flash_byte_read_plus				; Total sample time in seconds
	movff		temp1,xC+1

	PUTC		':'
    LOG_POINT_TO    log_divetime+.2
	call		ext_flash_byte_read_plus				; read divetime seconds
	movff		temp1,lo
	movff		xC+0,xA+0				; now calculate x-scale value
	movff		xC+1,xA+1
	movlw		profile_width_pixels					; pix width available
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; xA/xB=xC
	movff		xC+0,xA+0	
	movff		xC+1,xA+1
	movf		samplesecs_value,W		; devide through sample interval!
	movwf		xB+0
	clrf		xB+1
	call		div16x16				; xA/xB=xC
	movff		xC+0,profile_temp+0		; store value (use any #xC sample, skip xC-1) into temp registers
	movff		xC+1,profile_temp+1		; store value (use any #xC sample, skip xC-1) into temp registers
	infsnz		profile_temp+0,F		; Increase by one, there might be a remainder
	incf		profile_temp+1,F

	bsf			leftbind
	output_99x							; divetime seconds
	call	TFT_standard_color
	STRCAT_PRINT    ""

	call		ext_flash_byte_read_plus                ; Read min. Temperature
	movff		temp1,logbook_min_tp+0
	call		ext_flash_byte_read_plus                ; Read min. Temperature
	movff		temp1,logbook_min_tp+1

    ; Set pointer to Gas 1 Type.
    LOG_POINT_TO    log_gas1+.3
    call		ext_flash_byte_read_plus				; read Gas Type
    decfsz      temp1,W                                 ; =1 (="First")?
    bra         logbook_find_first_gas2                 ; No.
    movlw       .1
    movwf       temp1
    bra         logbook_find_first_gas_done
logbook_find_first_gas2:
    ; Set pointer to Gas 2 Type.
    LOG_POINT_TO    log_gas2+.3
    call		ext_flash_byte_read_plus				; read Gas Type
    decfsz      temp1,W                                 ; =1 (="First")?
    bra         logbook_find_first_gas3                 ; No.
    movlw       .2
    movwf       temp1
    bra         logbook_find_first_gas_done
logbook_find_first_gas3:
    ; Set pointer to Gas 3 Type.
    LOG_POINT_TO    log_gas3+.3
    call		ext_flash_byte_read_plus				; read Gas Type
    decfsz      temp1,W                                 ; =1 (="First")?
    bra         logbook_find_first_gas4                 ; No.
    movlw       .3
    movwf       temp1
    bra         logbook_find_first_gas_done
logbook_find_first_gas4:
    ; Set pointer to Gas 4 Type.
    LOG_POINT_TO    log_gas4+.3
    call		ext_flash_byte_read_plus				; read Gas Type
    decfsz      temp1,W                                 ; =1 (="First")?
    bra         logbook_find_first_gas5                 ; No.
    movlw       .4
    movwf       temp1
    bra         logbook_find_first_gas_done
logbook_find_first_gas5:
    movlw       .5                                      ; Must be Gas5
    movwf       temp1
logbook_find_first_gas_done:
	movff		temp1,average_depth_hold_total+3; keep copy to restore color
    rcall       profile_display_color       	; Back to normal profile color.
    ; Pointer is now trashed!

; Point to profile portion of this dive
	movff		ext_flash_log_pointer+0,ext_flash_address+0
	movff		ext_flash_log_pointer+1,ext_flash_address+1
	movff		ext_flash_log_pointer+2,ext_flash_address+2

	incf_ext_flash_address_0x20		d'2'	; Skip 0xFA 0xFA
    call		ext_flash_byte_read_plus_0x20	; Read low byte of total dives into temp1 (at the time the dive was made)

	; Load total number of dives (low byte only)
	read_int_eeprom .2
	incf		EEDATA,W					; +1
	bsf			STATUS,C					; Set borrow
	subfwb		divesecs,W	; total dives - dive# to show - 1 = low byte of total dives (at the time the dive was made)
	cpfseq		temp1						; # of dive in logbook (Must be equal with low byte in short header)
	bra			display_profile_no_profile	; Not equal, no profile for this dive available!

	; Skip rest of short header: 3 Bytes
	; Skip length of profile data: 3 Bytes
    ; Skip sampling rate in profile section: 1Byte
    ; Skip number of divisors: 1Byte
	incf_ext_flash_address_0x20		d'8'

; Divisor temp
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movf		temp1,W
	movwf       divisor_temperature	        ; Store divisor
	movwf       count_temperature           ; Store to tp� counter too.
; Divisor Deco
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movf		temp1,W
	movwf       divisor_deco      			; Store divisor
	movwf		count_deco                  ; Store as temp, too
; Divisor GF
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movff		temp1,divisor_gf			; Store divisor
; Divisor ppO2 Sensors
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movff		temp1,divisor_ppo2_sensors	; Store divisor
; Divisor decoplan
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movff		temp1,divisor_decoplan		; Store divisor
; Divisor CNS
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movff		temp1,divisor_cns			; Store divisor
; Divisor Tank data
    incf_ext_flash_address_0x20		d'2'
;    call		ext_flash_byte_read_plus_0x20	; Read information type
;    call		ext_flash_byte_read_plus_0x20	; Read information Length
    call		ext_flash_byte_read_plus_0x20	; Read information Divisor
	movff		temp1,divisor_tank			; Store divisor

	; Start Profile display
    movlw       color_deepblue
	call		TFT_set_color				; Make this configurable?
    ; Draw a frame around profile area
    WIN_FRAME_COLOR16 profile_top-1,profile_top+profile_height_pixels+1,profile_left-1,profile_left+profile_width_pixels+1

	movlw		profile_top
	movwf		win_top
	movlw		profile_left
	movwf		win_leftx2				; Left border (0-159)
	movlw		d'1'
	movwf		win_height
	movlw		profile_width_pixels+.1
	movwf		win_width+0				; Right border (0-159)
	clrf		win_width+1
	bra		display_profile2f			; No 0m line
display_profile2e:
	call		TFT_box						; Inputs:  win_top, win_leftx2, win_height, win_width, win_color1, win_color2
display_profile2f:
	movf		win_top,W				; Get row
	addwf		x_scale+0,W					; Add line interval distance to win_top
	tstfsz		x_scale+1					; >255?
	movlw		d'255'						; Yes, make win_top>239 -> Abort here
	btfsc		STATUS,C					; A Cary from the addwf above?
	movlw		d'255'						; Yes, make win_top>239 -> Abort here
	movwf		win_top						; Result in win_top again
	movlw		profile_top+profile_height_pixels+.1 ; Limit
	cpfsgt		win_top						; >239?
	bra			display_profile2e			; No, draw another line

	clrf		timeout_counter2			; here: used as counter for depth readings
	movlw		profile_width_pixels+profile_left-.1
	movwf		ignore_digits				; here: used as counter for x-pixels
	bcf			end_of_profile				; clear flag

	movlw		profile_left+.1
	movwf		logbook_pixel_x_pos			; here: used as colum x2 (Start at Colum 5)

	movlw		profile_top+.1					; Zero-m row
	movwf		apnoe_mins					; here: used for fill between rows
    movwf       logbook_last_tp             ; Initialise for Tp� curve too.

    movlw       LOW(-.100)                  ; Initialize max tp� to -10.0 �C.
    movwf       logbook_max_tp+0
    movlw       HIGH 0xFFFF & (-.100)
    movwf       logbook_max_tp+1
    
    setf        logbook_cur_tp+0            ; Initialize Tp�, before the first recorded point.
    setf        logbook_cur_tp+1
    clrf        logbook_last_tp             ; Also reset previous Y for Tp�
    clrf        logbook_ceiling             ; Ceiling = 0, correct value for no ceiling.
    movlw       profile_top+.1
    movwf       logbook_min_temp_pos        ; Initialize for displaying the lowest temperature
    movlw       profile_top+profile_height_pixels
    movwf       logbook_max_temp_pos        ; Initialize for displaying the highest temperature

    movlw       profile_left
    movwf       win_leftx2
    movlw       profile_top
    movwf       win_top
    movlw       profile_height_pixels
    movwf       win_height
    movlw       LOW (profile_width_pixels*.2)
    movwf       win_width+0
    movlw       HIGH (profile_width_pixels*.2)
    movwf       win_width+1
    call        TFT_box_write           ; open box for d1

;    INIT_PIXEL_WRITE logbook_pixel_x_pos       ; pixel x2			(Also sets standard Color!)

profile_display_loop:
	; Init pixel write
        movf    logbook_pixel_x_pos,W
	mullw   2
        call    pixel_write_col320
    
	movff		profile_temp+0,profile_temp2+0
	movff		profile_temp+1,profile_temp2+1		; 16Bit x-scaler
	incf		profile_temp2+1,F					
	tstfsz		profile_temp2+0						; Must not be Zero
	bra			profile_display_loop2				; Not Zero!
	incf		profile_temp2+0,F					; Zero, Increase!

profile_display_loop2:
	rcall		profile_view_get_depth		; reads depth, temp and profile data

	btfsc		end_of_profile					; end-of profile reached?
	bra			profile_display_loop_done	; Yes, skip all remaining pixels


    ;---- Draw Ceiling curve, if any ---------------------------------------------
    movf        divisor_deco,W
    bz          profile_display_skip_deco

    movf        logbook_ceiling,W           ; Any deco ceiling ?
    bz          profile_display_skip_deco

	mullw       .100                        ; Yes: convert to mbar
	movff       PRODL,sub_a+0
	movff       PRODH,sub_a+1
	movff       logbook_cur_depth+0,sub_b+0    ; Compare with UNSIGNED current depth (16bits)
	movff       logbook_cur_depth+1,sub_b+1
	call        subU16                      ; set (or not) neg_flag

    movlw       color_dark_green            ; Dark green if Ok,
    btfss       neg_flag
    movlw       color_dark_red              ; Or dark red if ceiling overflown.
    call        TFT_set_color
    
	movff       PRODL,xA+0
	movff       PRODH,xA+1
	movff		y_scale+0,xB+0			; devide pressure in mbar/quant for row offsett
	movff		y_scale+1,xB+1
	call		div16x16					; xA/xB=xC

	movlw		profile_top+.1                  ; Starts right after the top line.
	movwf		win_top
	movff		logbook_pixel_x_pos,win_leftx2 ; Left border (0-159)
	movff		xC+0,win_height				
	call		half_vertical_line			; Inputs:  win_top, win_leftx2, win_height, win_color1, win_color2

profile_display_skip_deco:
    ;---- Draw Tp� curve, if any ---------------------------------------------
    movf        divisor_temperature,W
    bz          profile_display_skip_temp

	movf        logbook_cur_tp+0,W          ; Did we had already a valid Tp�C record ?
	andwf       logbook_cur_tp+1,W
	incf        WREG
	bz          profile_display_skip_temp   ; No: just skip drawing.

    movlw       LOW(((profile_height_pixels-.10)*.256)/.370)         ; fixed tp� scale: (-2 .. +35�C * scale256 )/153pix
 	movwf		xB+0
    movlw       HIGH(((profile_height_pixels-.10)*.256)/.370)
 	movwf		xB+1

	movf        logbook_cur_tp+0,W          ; Current Tp� - (-2.0�C) == Tp� + 20.
	addlw       LOW(.20)                    ; Low byte.
	movwf       xA+0
    movf		logbook_cur_tp+1,W
    btfsc       STATUS,C                    ; Propagate carry, if any
    incf        WREG
    movwf       xA+1
    call		mult16x16					; xA*xB=xC

    ; scale: divide by 256, ie. take just high byte.
    movf        xC+1,W
    sublw       profile_top+profile_height_pixels-.10		; Upside-down: Y = .75 + (.153 - result)
    movwf       xC+0

	; Check limits
	movlw		profile_top+.1
	movwf		xC+1
	cpfsgt		xC+0
	movff		xC+1,xC+0

    movlw       color_orange
    call        TFT_set_color

    movf        logbook_last_tp,W           ; do we have a valid previous value ?
    bz          profile_display_temp_1      ; No: skip the vertical line.
    movwf       xC+1
	call		profile_display_fill		; In this column between this row (xC+0) and the last row (xC+1)
profile_display_temp_1:
    movf        xC+0,W                      ; current row
    cpfsgt      logbook_min_temp_pos        ; check limit
    movwf       logbook_min_temp_pos        ; lowest row in the temp graph
    cpfslt      logbook_max_temp_pos        ; check limit
    movwf       logbook_max_temp_pos        ; lowest row in the temp graph

    movff       xC+0,logbook_last_tp
    PIXEL_WRITE logbook_pixel_x_pos,xC+0       ; Set col(0..159) x row (0..239), put a current color pixel.

profile_display_skip_temp:
    ;---- Draw depth curve ---------------------------------------------------
	movff		y_scale+0,xB+0			; devide pressure in mbar/quant for row offsett
	movff		y_scale+1,xB+1
	movff		logbook_cur_depth+0,xA+0
	movff		logbook_cur_depth+1,xA+1
	call		div16x16					; xA/xB=xC
	movlw		profile_top+.1
	addwf		xC+0,F						; add 75 pixel offset to result
	
	btfsc		STATUS,C                    ; Ignore potential profile errors
	movff		apnoe_mins,xC+0

    rcall       profile_display_color       ; Back to normal profile color.

    movff       apnoe_mins,xC+1
	call		profile_display_fill		; In this column between this row (xC+0) and the last row (xC+1)
	movff		xC+0,apnoe_mins				; Store last row for fill routine

    PIXEL_WRITE logbook_pixel_x_pos,xC+0    ; Set col(0..159) x row (0..239), put a std color pixel.
    incf		logbook_pixel_x_pos,F		; Next column

    ;---- Draw Marker square , if any ----------------------------------------
    btfss       log_marker_found            ; Any marker to draw?
    bra         profile_display_skip_marker ; No

    ; tiny "m"
    incf        apnoe_mins,W	; increase row (Y)
    movwf       win_top
    ; limit win_top to 220
    movlw	.220
    cpfslt	win_top
    movwf	win_top
    decf        logbook_pixel_x_pos,W	; decrease column (X)
    movwf       win_leftx2
    ; limit win_leftx2 to 151
    movlw	.151
    cpfslt	win_leftx2
    movwf	win_leftx2

    movlw       color_orange
    call        TFT_set_color
    WIN_FONT	FT_TINY
    lfsr	FSR2,buffer
    STRCPY_PRINT    "m"
    bcf         log_marker_found            ; Clear flag
    
    movlw       profile_left
    movwf       win_leftx2
    movlw       profile_top
    movwf       win_top
    movlw       profile_height_pixels
    movwf       win_height
    movlw       LOW (profile_width_pixels*.2)
    movwf       win_width+0
    movlw       HIGH (profile_width_pixels*.2)
    movwf       win_width+1
    call        TFT_box_write           ; re-open box for d1

profile_display_skip_marker:
    ;---- Draw CNS curve, if any ---------------------------------------------
    movf        divisor_cns,W
    bz          profile_display_skip_cns
    ;
    ; TODO HERE 
    ;
profile_display_skip_cns:

    ;---- Draw GF curve, if any ----------------------------------------------
    movf        divisor_gf,W
    bz          profile_display_skip_gf
    ;
    ; TODO HERE 
    ;
profile_display_skip_gf:

    ;---- All curves done.
    
profile_display_skip_loop1:					; skips readings!
	dcfsnz		profile_temp2+0,F
	bra			profile_display_loop3		; check 16bit....

	rcall		profile_view_get_depth		; reads depth, temp and profile data

	btfsc		end_of_profile					; end-of profile reached?
	bra			profile_display_loop_done	; Yes, skip all remaining pixels

	bra			profile_display_skip_loop1

profile_display_loop3:
	decfsz		profile_temp2+1,F			; 16 bit x-scaler test
	bra			profile_display_skip_loop1	; skips readings!

	decfsz		ignore_digits,F				; counts drawn x-pixels to zero
	bra			profile_display_loop		; Not ready yet
; Done.

display_profile_no_profile:					; No profile available for this dive!

profile_display_loop_done:
    btfss   is_bailout                          ; Bailout during the dive?
    bra     profile_display_loop_done_nobail    ; No
    ; Yes, show "Bailout"
   	movlw   color_pink
	call    TFT_set_color
    WIN_TINY	logbook_bailout_column,logbook_bailout_row
    STRCPY_TEXT_PRINT   tDiveBailout        ; Bailout
profile_display_loop_done_nobail:
    btfss   gas6_changed                         ; Gas6
    bra     profile_display_loop_done_nogas6      ; No
    ; Yes, show "Gas 6!"
   	movlw   color_pink
	call    TFT_set_color
    WIN_TINY	logbook_bailout_column,logbook_bailout_row-.15
    STRCPY_TEXT  tGas                       ; Gas
    STRCAT_PRINT  " 6!"

profile_display_loop_done_nogas6:
	decf	divesecs,F		;-1
	read_int_eeprom .2
	movf	EEDATA,W
	bcf		STATUS,C
	subfwb	divesecs,W		; max. dives (low value) - divesecs
	movwf	lo				; result
	incf	divesecs,F		;+1
	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> lo=0
	; 2nd: 201000h-201FFFh -> lo=1
	; 3rd: 202000h-202FFFh -> lo=2
	; 256: 2FF000h-2FFFFFh -> lo=255 (And hi>0...)
	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	movlw		0x20
	movwf		ext_flash_address+2
	movlw		.16
	mulwf		lo					; lo*16 = offset to 0x2000 (up:hi)
	movf		PRODL,W
	addwf		ext_flash_address+1,F
	movf		PRODH,W
	addwfc		ext_flash_address+2,F
	; pointer at the first 0xFA of header

    movff   logbook_min_temp_pos,win_top     ; Y position at lowest temperature
    movff   logbook_pixel_x_pos,win_leftx2
    movlw   .130
    cpfslt  win_leftx2                              ; limit left border to 130
    movwf   win_leftx2
    WIN_FONT   FT_TINY
	movlw   color_orange            ; Use same color as tp� curve
	call    TFT_set_color

    movff   logbook_min_tp+0,lo
	movff   logbook_min_tp+1,hi
    lfsr	FSR2,buffer

	TSTOSS	opt_units			; 0=�C, 1=�F
	bra		logbook_show_temp_metric
;logbook_show_temp_imperial:
	call	TFT_convert_signed_16bit    	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	call	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit
	lfsr	FSR2,buffer						; Overwrite "-"
	bsf		ignore_digit5		; Full degrees only
	output_16
	STRCAT_TEXT_PRINT  tLogTunitF
    ; Now, the max. temperature
    movlw   .15
    subwf   logbook_max_temp_pos,W
    movff   WREG,win_top        ; Y position at max temperature
    movff   logbook_max_tp+0,lo
	movff   logbook_max_tp+1,hi
    lfsr    FSR2,buffer
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	call	convert_celsius_to_fahrenheit	; convert value in lo:hi from celsius to fahrenheit
	output_16
	bcf		ignore_digit5
	STRCAT_TEXT_PRINT    tLogTunitF

	bra		logbook_show_temp_common

logbook_show_temp_metric:
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	movlw	d'3'
	movwf	ignore_digits
	bsf		leftbind
	output_16dp	d'2'					; temperature
	STRCAT_TEXT_PRINT    tLogTunitC
    ; Now, the max. temperature
    movlw   .15
    subwf   logbook_max_temp_pos,W
    movwf   win_top        ; Y position at max temperature
    movff   logbook_max_tp+0,lo
	movff   logbook_max_tp+1,hi
    lfsr    FSR2,buffer
	call	TFT_convert_signed_16bit	; converts lo:hi into signed-short and adds '-' to POSTINC2 if required
	movlw	d'3'
	movwf	ignore_digits
	bsf		leftbind
	output_16dp	d'2'					; temperature
	STRCAT_TEXT_PRINT    tLogTunitC

logbook_show_temp_common:

	bcf		leftbind
	call    TFT_standard_color

	; Get pointer to Gaslist
    LOG_POINT_TO    log_gas1

	movlw		color_white					; Color for Gas 1
	call		TFT_set_color				; Set Color...
	bsf			leftbind
	WIN_TINY	log_gas_column1, log_gas_row
    rcall       log_show_gas_common

	movlw		color_green					; Color for Gas 2
	call		TFT_set_color				; Set Color...
	WIN_TINY	log_gas_column2, log_gas_row
    rcall       log_show_gas_common

	movlw		color_red					; Color for Gas 3
	call		TFT_set_color				; Set Color...
	WIN_TINY	log_gas_column3, log_gas_row
    rcall       log_show_gas_common

	movlw		color_yellow				; Color for Gas 4
	call		TFT_set_color				; Set Color...
	WIN_TINY	log_gas_column4, log_gas_row
    rcall       log_show_gas_common

	movlw		color_cyan                  ; Color for Gas 5
	call		TFT_set_color				; Set Color...
	WIN_TINY	log_gas_column5, log_gas_row
    rcall       log_show_gas_common

    rcall       logbook_preloop_tasks       ; Clear some flags and set to Speed_eco
display_profile_loop:
	btfsc		switch_left					; SET/MENU?
	bra			logbook_page2   			; Show more information
	btfsc		switch_right				; ENTER?
	bra			exit_profileview			; back to list

    rcall       log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
	btfsc		sleepmode					; Timeout?
	bra			exit_profileview			; back to list
	bra			display_profile_loop		; wait for something to do

    global  log_screendump_and_onesecond
log_screendump_and_onesecond:    ; Check if we need to make a screenshot and check for new second
	btfsc		onesecupdate
	call		timeout_surfmode			; Timeout
	btfsc		onesecupdate
	call		set_dive_modes				; Check, if divemode must be entered
	bcf			onesecupdate				; one second update
	btfsc		divemode
	goto		restart						; Enter Divemode if required

    btfsc       enable_screen_dumps         ; =1: Ignore vin_usb, wait for "l" command (Screen dump)
    bra         log_screendump_and_onesecond2
    btfsc       vusb_in                     ; USB plugged in?
    call        comm_mode                   ; Start COMM mode
    return
log_screendump_and_onesecond2:
    btfss       vusb_in                     ; USB (still) plugged in?
    bcf         enable_screen_dumps         ; No, clear flag
    call        rs232_get_byte
    btfsc       rs232_recieve_overflow
    return
    movlw       "l"
    cpfseq      RCREG1
    return
    goto        TFT_dump_screen             ; Dump the screen contents and return

log_show_gas_common:
    extern  customview_show_mix
	lfsr		FSR2,buffer
	call		ext_flash_byte_read_plus					; Gas2 current O2
	movff		temp1,lo
	call		ext_flash_byte_read_plus					; Gas2 current He
	movff		temp1,hi
    call		customview_show_mix				; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
	STRCAT_PRINT	""
	call		ext_flash_byte_read_plus					; Gas2 change depth
    goto		ext_flash_byte_read_plus					; Gas2 Type and return

;=============================================================================
profile_display_color:
    movff       average_depth_hold_total+3,active_gas ; Restore gas color.
	movlw		color_white					; Default color
	dcfsnz		active_gas,F
	movlw		color_white					; Color for Gas 1
	dcfsnz		active_gas,F
	movlw		color_green					; Color for Gas 2
	dcfsnz		active_gas,F
	movlw		color_red					; Color for Gas 3
	dcfsnz		active_gas,F
	movlw		color_yellow				; Color for Gas 4
	dcfsnz		active_gas,F
	movlw		color_cyan      			; Color for Gas 5
	dcfsnz		active_gas,F
	movlw		color_pink					; Color for Gas 6
	goto		TFT_set_color				; Set Color...

;=============================================================================
; Draw a vertical line between xC+1 and xC+0, at current X position.
;
; Note: should keep xC+0
; Note: ascending or descending !
;
profile_display_fill:
    ; First, check if xC+0>apnoe_mins or xC+0<aponoe_mins
	movf	xC+0,W
	cpfseq	xC+1				    ; xC+0 = apone_mins?
	bra		profile_display_fill2	; No!
	return

profile_display_fill2:	
    ; Make sure to init X position.
    movf    logbook_pixel_x_pos,W
    mullw   2
    decf    PRODL,F
    movlw   0
    subwfb  PRODH,F
    call    pixel_write_col320

	movf	xC+0,W
	cpfsgt	xC+1				    ; apnoe_mins>xC+0?
	bra		profile_display_fill_up	; Yes!

profile_display_fill_down2:			; Loop	
	decf		xC+1,F

    HALF_PIXEL_WRITE    xC+1        ; Updates just row (0..239)

	movf		xC+0,W
	cpfseq		xC+1				; Loop until xC+1=xC+0
	bra			profile_display_fill_down2
	return							; apnoe_mins and xC+0 are untouched

profile_display_fill_up:			; Fill upwards from xC+0 to apone_mins!
	incf		xC+1,F

    HALF_PIXEL_WRITE    xC+1        ; Updates just row (0..239)

	movf		xC+0,W
	cpfseq		xC+1				; Loop until xC+1=apnoe_mins
	bra			profile_display_fill_up
	return							; apnoe_mins and xC+0 are untouched

;=============================================================================


profile_view_get_depth:
	infsnz		logbook_sample_counter+0,F
    incf        logbook_sample_counter+1,F		; Count read pixels

	movf		logbook_sample_counter+0,W
	cpfseq		average_depth_hold_total+0
	bra			profile_view_get_depth_no_line		; no need to draw a 10min line, continue
	movf		logbook_sample_counter+1,W
	cpfseq		average_depth_hold_total+1
	bra			profile_view_get_depth_no_line		; no need to draw a 10min line, continue
; draw a new 10min line here...
	clrf		logbook_sample_counter+0
	clrf		logbook_sample_counter+1					; clear counting registers for next line

	; Vertical lines...
	movlw       color_deepblue
	call		TFT_set_color						; Make this configurable?
	movlw		profile_top+.1
	movwf		win_top
	incf		logbook_pixel_x_pos,W	; draw one line to right to make sure it's the background of the profile
	movwf		win_leftx2		; Left border (0-159)
	movlw		profile_height_pixels
	movwf		win_height				
	movlw		profile_height_pixels
	movwf		win_width				; "Window" height
	call		half_horizontal_line        ; Inputs:  win_top, win_leftx2, win_width, win_color1, win_color2

profile_view_get_depth_no_line:
	call		ext_flash_byte_read_plus_0x20	; read depth first
	movff		temp1,logbook_cur_depth+0   	; low value
	call		ext_flash_byte_read_plus_0x20	; read depth first
	movff		temp1,logbook_cur_depth+1   	; high value
	call		ext_flash_byte_read_plus_0x20	; read Profile Flag Byte
	movff		temp1,timeout_counter2			; store Profile Flag Byte

	bcf			event_occured				; clear flag
	btfsc		timeout_counter2,7
	bsf			event_occured				; We also have an Event byte!
	bcf			timeout_counter2,7			; Clear Event Byte Flag (If any)
	; timeout_counter2 now holds the number of additional bytes to ignore (0-127)
	movlw		0xFD						; end of profile bytes?
	cpfseq		logbook_cur_depth+0
	bra			profile_view_get_depth_new1	; no 1st. 0xFD
	cpfseq		logbook_cur_depth+1
	bra			profile_view_get_depth_new1	; no 2nd. 0xFD
	bsf			end_of_profile				; End found! Set Flag! Skip remaining pixels!
	return

profile_view_get_depth_new1:
	btfsc		event_occured				; Was there an event attached to this sample?
	rcall		profile_view_get_depth_events	; Yes, get information about this event(s)
    
    ;---- Read Tp�, if any AND divisor reached AND bytes available -----------
    movf        divisor_temperature,W       ; Is Tp� divisor null ?
    bz          profile_view_get_depth_no_tp; Yes: no Tp� curve.
    decf        count_temperature,F         ; Decrement tp� counter
    bnz         profile_view_get_depth_no_tp; No temperature this time
    
    call		ext_flash_byte_read_plus_0x20				; Tp� low
	decf        timeout_counter2,F
	movff		temp1,logbook_cur_tp+0
    call		ext_flash_byte_read_plus_0x20				; Tp� high
	decf        timeout_counter2,F
	movff		temp1,logbook_cur_tp+1
	movff       divisor_temperature,count_temperature   ; Restart counter.
    
    ; Compute Tp� max on the fly...
    movff       logbook_cur_tp+0,sub_a+0    ; Compare cur_tp > max_tp ?
    movff       logbook_cur_tp+1,sub_a+1
    movff       logbook_max_tp+0,sub_b+0
    movff       logbook_max_tp+1,sub_b+1
    call        sub16                       ; SIGNED sub_a - sub_b
    btfsc       neg_flag
    bra         profile_view_get_depth_no_tp

    ; store max. temp only below start_dive_threshold (1,0m)
    tstfsz      logbook_cur_depth+1             ; > 2,56m?
    bra         profile_view_compute_max_temp   ; Yes, include in max. temp measurement
    movlw       start_dive_threshold            ; 1,0m
    cpfsgt      logbook_cur_depth+0             ; low value
    bra         profile_view_get_depth_no_tp    ; above 1,0m, ignore temp

profile_view_compute_max_temp:
    movff       logbook_cur_tp+0,logbook_max_tp+0
    movff       logbook_cur_tp+1,logbook_max_tp+1
    
    ;---- Read deco, if any AND divisor=0 AND bytes available ----------------
profile_view_get_depth_no_tp:
    movf        divisor_deco,W
    bz          profile_view_get_depth_no_deco
    decf        count_deco,F
    bnz         profile_view_get_depth_no_deco
    
    call		ext_flash_byte_read_plus_0x20
	decf        timeout_counter2,F
	movff		temp1,logbook_ceiling
	movff       divisor_deco,count_deco     ; Restart counter.
    call		ext_flash_byte_read_plus_0x20   ; Skip stop length
	decf        timeout_counter2,F

    ;---- Read GF, if any AND divisor=0 AND bytes available ------------------
profile_view_get_depth_no_deco:
    ; Then skip remaining bytes...
	movf		timeout_counter2,W			; number of additional bytes to ignore (0-127)
    tstfsz      timeout_counter2            ; Anything to skip?
	call		incf_ext_flash_address0_0x20; Yes, increases bytes in ext_flash_address:3 with 0x200000 bank switching
	return

profile_view_get_depth_events:
    clrf        EventByte2                  ; Clear EventByte2
	call		ext_flash_byte_read_plus_0x20 ; Read Event byte
	movff		temp1,EventByte				; store EventByte
	decf		timeout_counter2,F			; reduce counter

    btfss       EventByte,7                 ; Another Event byte?
    bra         profile_no_second_eventbyte ; No
    call		ext_flash_byte_read_plus_0x20; Read Event byte2
    movff		temp1,EventByte2			; store EventByte2
	decf		timeout_counter2,F			; reduce counter
    bcf         EventByte,7                 ; Clear flag

profile_no_second_eventbyte:
; Check event flags in the EventBytes
	btfsc		EventByte,4					; Manual Gas Changed?
	rcall       logbook_event1				; Yes!
	btfsc		EventByte,5					; Stored Gas Changed?
	rcall       logbook_event4				; Yes!
	btfsc		EventByte,6                 ; Setpoint Change?
	rcall       logbook_event3				; Yes!
	btfsc		EventByte2,0				; Bailout?
	rcall       logbook_event2				; Yes!
    ; Any Alarm?
    bcf         EventByte,4                 ; Clear bits already tested
    bcf         EventByte,5
    bcf         EventByte,6
    movlw       .6                          ; manual marker?
    cpfseq      EventByte
    return	   ; No, return
    bsf         log_marker_found            ; Manual marker! Draw small yellow rectancle here
    return

logbook_event4: ; Stored Gas changed!
	call		ext_flash_byte_read_plus_0x20 ; Read Gas#
	decf		timeout_counter2,F			  ; reduce counter
	movff		temp1,average_depth_hold_total+3
    rcall       profile_display_color         ; Change profile color according to gas number
	return

logbook_event1: ; Gas6 changed
    bsf         gas6_changed
    movlw       6                           ; Just color backup to 6
    movwf       average_depth_hold_total+3
    rcall       profile_display_color       ; Back to normal profile color.
    incf_ext_flash_address_0x20  .2         ; Skip two bytes
    decf		timeout_counter2,F			; reduce counter
    decf		timeout_counter2,F			; reduce counter
	return

logbook_event2: ; Bailout
    bsf         is_bailout                  ; Set flag
    movff       average_depth_hold_total+3,total_divetime_seconds+0 ; Backup last gas color in case we return to CCR
    movlw       6                           ; Use Gas6 color
    movwf       average_depth_hold_total+3
    rcall       profile_display_color       ; Back to normal profile color.
    incf_ext_flash_address_0x20  .2         ; Skip two bytes
    decf		timeout_counter2,F			; reduce counter
    decf		timeout_counter2,F			; reduce counter
	return

logbook_event3: ; Setpoint change
    incf_ext_flash_address_0x20  .1         ; Skip one byte
    decf		timeout_counter2,F			; reduce counter
    btfss       is_bailout                  ; Are we in bailout?
    return      ; No, return
    ; We were in bailout before, restore profile color
    movff       total_divetime_seconds+0,average_depth_hold_total+3 ; Restore color
    rcall       profile_display_color       ; Back to normal profile color.
    return

exit_profileview:
;	call		speed_fastest
	bcf			sleepmode
	clrf		timeout_counter2				; restore all registers to build same page again
	movff		divemins_backup,divemins+0
	movff		logbook_divenumber_temp, logbook_divenumber
	movff		logbook_max_dive_counter_temp,logbook_max_dive_counter
	incf		logbook_max_dive_counter,F
	decf		logbook_divenumber,F
	bcf			all_dives_shown
	clrf		menupos3					; here: used row on current page
	movlw		logbook_row_number
	movwf		menupos						; here: active row on current page
;    call        TFT_DisplayOff
;    call        TFT_boot
    clrf        CCP1CON                     ; stop PWM
    bcf         PORTC,2                     ; Pull PWM out to GND
	call		TFT_ClearScreen				; clear details/profile
	goto		logbook2					; start search

next_logbook2:
	btfsc		all_dives_shown				; all shown
	goto		logbook 					; all reset
	clrf		menupos3	
	movlw		logbook_row_number
	movwf		menupos
	incf		logbook_page_number,F		; start new screen
    bsf         keep_cursor_new_page        ; Keep cursor on "next page"
    clrf        CCP1CON                     ; stop PWM
    bcf         PORTC,2                     ; Pull PWM out to GND
	call		TFT_ClearScreen
	goto		logbook2					; start search

next_logbook3:
	incf		menupos,F				; +1
	movlw		logbook_row_number+.2
	cpfsgt		menupos					; =logbook_row_number+.3?
	bra			next_logbook3a			; No
	movlw		.1
	movwf		menupos
	bra			next_logbook3b

next_logbook3a:
	incf		menupos3,W				; last entry in current page +1
	cpfseq		menupos					; same as cursor pos.?
	bra			next_logbook3b			; No
	movlw		logbook_row_number+.1	; Yes, ...
	movwf		menupos					; ... jump directly to "next page" if page is not full

	movlw		logbook_row_number
	cpfseq		menupos3				; Last dive was row logbook_row_number?
	bsf			all_dives_shown			; No, set flag to load first page again (full reset)

next_logbook3b:
	clrf		timeout_counter2
	call		TFT_logbook_cursor

	bcf			switch_left
	goto		logbook_loop

display_listdive:
	bsf			logbook_page_not_empty		; Page not empty
	incf		menupos3,F					

	bsf			leftbind
	WIN_FONT	FT_SMALL
	WIN_LEFT	logbook_list_left
	
	decf		menupos3,W		; -1 into wreg
	mullw		logbook_row_offset
	movff		PRODL,win_top

	lfsr		FSR2,buffer
	call		do_logoffset_common_read	; Read into lo:hi
	tstfsz		lo							; lo=0?
	bra			display_listdive1       	; No, adjust offset
	tstfsz		hi							; hi=0?
	bra			display_listdive1           ; No, adjust offset
	bra			display_listdive1b          ; Display now

display_listdive1:
	; Check limit (lo:hi must be <1000)
	movlw	LOW		d'1000'            ; Compare to 1000
	subwf   lo,W
	movlw	HIGH	d'1000'
	subwfb  hi,W
	bc      display_listdive1b ; carry = no-borrow = > 1000, skip!

	infsnz      lo,F
	incf        hi,F						; hi:lo = hi:lo + 1
	movff		lo,sub_a+0
	movff		hi,sub_a+1
	movff		logbook_divenumber,sub_b+0
	clrf		sub_b+1
	call		subU16						;  sub_c = sub_a - sub_b
	movff		sub_c+0,lo
	movff		sub_c+1,hi
    bra         display_listdive1a

display_listdive1b:
    clrf        hi
	movff		logbook_divenumber,lo		; lo=0 and hi=0 -> show without applied offset
display_listdive1a:
	output_16_3                     ; displays only last three figures from a 16Bit value (0-999), # of dive
	PUTC		' '
    LOG_POINT_TO    log_date+1              ; Point to month
	call		ext_flash_byte_read_plus	
	movff		temp1,lo					; read month

display_listdive2:
	movff		lo,convert_value_temp+0		; Month (in lo, see above)
	call		ext_flash_byte_read_plus	; Day 
	movff		temp1,convert_value_temp+1	
	call		TFT_convert_date_short		; converts into "DD/MM" or "MM/DD" or "MM/DD" into buffer
	PUTC	' '

    LOG_POINT_TO    log_max_depth           ; Point to max. depth
	call		ext_flash_byte_read_plus	; max. Depth
	movff		temp1,lo
	call		ext_flash_byte_read_plus
	movff		temp1,hi

	TSTOSS      opt_units			; 0=Meters, 1=Feets
	bra			display_listdive2_metric
;display_listdive2_imperial:
	call		convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
	PUTC	' '
	bcf			leftbind
    output_16_3                         ; limit to 999 and display only (0-999)
	STRCAT_TEXT tFeets1
	bra			display_listdive3

display_listdive2_metric:
    bsf     ignore_digit5               ; no cm...
	movlw		d'1'					; +1
	movff		WREG,ignore_digits		; no 1000m
	bcf			leftbind
	output_16dp .3  					; xxx.y
	STRCAT_TEXT tMeters
	PUTC	' '

display_listdive3:
	call		ext_flash_byte_read_plus
	movff		temp1,lo					; read divetime minutes
	call		ext_flash_byte_read_plus
	movff		temp1,hi
	output_16_3								; Divetime minutes (0-999min)
	STRCAT_TEXT tMinutes                    
    clrf    WREG
    movff   WREG,buffer+.21                 ; limit to 21 chars
    STRCAT_PRINT ""                         ; Display header-row in list
	return

logbook_show_divenumber:
	call		do_logoffset_common_read	; Read into lo:hi
	tstfsz		lo							; lo=0?
	bra			logbook_show_divenumber2	; No, adjust offset
	tstfsz		hi							; hi=0?
	bra			logbook_show_divenumber2	; No, adjust offset
	movff		divesecs,lo					; lo=0 and hi=0 -> skip Offset routine
	bra			logbook_show_divenumber3	; Display now

logbook_show_divenumber2:
    infsnz      lo,F
	incf        hi,F                        ; hi:lo = hi:lo + 1
	movff		lo,sub_a+0
	movff		hi,sub_a+1
	movff		divesecs,sub_b+0
	clrf		sub_b+1
	call		subU16						;  sub_c = sub_a - sub_b
	movff		sub_c+0,lo
	movff		sub_c+1,hi

logbook_show_divenumber3:
	WIN_MEDIUM	logbook_divenumer_column, logbook_divenumer_row
	bsf			leftbind
	output_16									; # of dive in logbook
	bcf			leftbind
	STRCAT_PRINT	""
    return


logbook_page2: ; Show more info
    rcall   log_details_header	; Shows number, time/date and basic dive info
	
	; Deco model
    WIN_SMALL   .5,.65
    LOG_POINT_TO    log_decomodel
    call	ext_flash_byte_read_plus	; read deco model
    movff       temp1,lo
    decfsz      temp1,F
    bra         logbook_decomodel1
    ; Deco model GF Version
    STRCAT_TEXT_PRINT    tZHL16GF
    LOG_POINT_TO    log_gf_lo
    WIN_SMALL   .5,.90
    STRCPY_TEXT tGF
    call	ext_flash_byte_read_plus            ; Read GF lo
    movff       temp1,lo
    output_8
    STRCAT	"%/"
    bra         logbook_decomodel_common
logbook_decomodel1:
    ; Deco model NON-GF Version
    STRCAT_TEXT_PRINT    tZHL16
    LOG_POINT_TO    log_sat_mult
    WIN_SMALL   .5,.90
    call	ext_flash_byte_read_plus            ; Read sat_mult
    movff       temp1,lo
    output_8
    STRCAT	"%/"
logbook_decomodel_common:
    call	ext_flash_byte_read_plus            ; Read desat_mult or GF_hi
    movff       temp1,lo
    output_8
    STRCAT_PRINT	"%"
    
    ; CNS
    LOG_POINT_TO    log_cns_start
    WIN_SMALL    .5,.115
    STRCPY_TEXT tCNS2
    call	ext_flash_byte_read_plus	; read cns low
    movff       temp1,lo
    call	ext_flash_byte_read_plus	; read cns high
    movff       temp1,hi
    output_16
    LOG_POINT_TO    log_cns_end
    STRCAT      "->"
    call	ext_flash_byte_read_plus	; read CNS low
    movff       temp1,lo
    call	ext_flash_byte_read_plus	; read CNS high
    movff       temp1,hi
    output_16
    STRCAT_PRINT	"%"

    ; Salinity
    WIN_SMALL	.5,.140
    LOG_POINT_TO    log_salinity
    STRCPY_TEXT tDvSalinity
    bsf         leftbind
    call	ext_flash_byte_read_plus	; read salinity
    movff       temp1,lo
    movff       temp1,total_divetime_seconds+1  ; backup for average depth display
    output_8
    STRCAT_PRINT	"%"

    ; Average depth
    WIN_SMALL	.5,.165
    STRCPY_TEXT tAVG
    LOG_POINT_TO    log_avr_depth
    call	ext_flash_byte_read_plus	; read avr low
    movff       temp1,lo
    call	ext_flash_byte_read_plus	; read avr high
    movff       temp1,hi
    movf        total_divetime_seconds+1,W         ; salinity for this dive
    call        adjust_depth_with_salinity_log     ; computes salinity setting (FROM WREG!) into lo:hi [mbar]
    output_16dp .3
    STRCAT_PRINT    "m"
    
    ; Last deco
    LOG_POINT_TO    log_last_stop
    WIN_SMALL	.5,.190
    STRCPY_TEXT tLastDecostop
    call	ext_flash_byte_read_plus            ; Read last stop
    movff       temp1,lo
    output_8
    STRCAT_PRINT    "m"

    movlw       color_lightblue
    call	TFT_set_color
    WIN_FRAME_COLOR16   .63,.220,.2,.105; Top, Bottom, Left, Right

    ; Firmware
    call	TFT_standard_color
    WIN_SMALL	.110,.65
    STRCAT  "V:"
    LOG_POINT_TO    log_firmware
    call	ext_flash_byte_read_plus	; read firmware xx
    movff       temp1,lo
    bsf		neg_flag    ; set flag for 2.15 or newer
    movlw	.1
    cpfsgt	lo	    ; >1?
    bcf		neg_flag    ; No, clear flag
    movlw	.9
    cpfslt	lo	    ; <9?
    bcf		neg_flag    ; No, clear flag (When unit was updgraded from hwOS Sport (10.xx))
    bsf         leftbind
    output_8
    PUTC        "."
    call	ext_flash_byte_read_plus	; read firmware yy
    movff       temp1,lo
    movlw	.14
    cpfsgt	lo	    ; >14?
    bcf		neg_flag    ; No, clear flag
    output_99x
    STRCAT_PRINT	""

    btfss   neg_flag    ; set flag for 2.15 or newer
    bra	    logbook_no_batt_info
    
    ; Battery percent (for dives with 2.15 or newer)
    WIN_SMALL	.110,.140
    LOG_POINT_TO    log_batt_info	    ; Battery percent
    call	ext_flash_byte_read_plus	; read battery low
    movff       temp1,lo
    output_8
    STRCAT_PRINT	"%"

logbook_no_batt_info:		; dives with firmware <2.15

    ; Battery voltage
    WIN_SMALL	.110,.90
    STRCAT_PRINT     "Batt:"
    WIN_SMALL	.110,.115
    LOG_POINT_TO    log_battery	    ; Battery voltage...
    call	ext_flash_byte_read_plus	; read battery low
    movff       temp1,lo
    call	ext_flash_byte_read_plus	; read battery high
    movff       temp1,hi
    output_16dp  .2
    STRCAT_PRINT	"V"
   
    ; surface pressure in mbar
    LOG_POINT_TO    log_surface_press
    call	ext_flash_byte_read_plus	; read surface pressure
    movff       temp1,lo
    call	ext_flash_byte_read_plus	; read surface pressure
    movff       temp1,hi
    WIN_SMALL	.110,.165
    lfsr        FSR2,buffer
    bsf		leftbind
    output_16							; Air pressure before dive
    STRCAT_TEXT    tMBAR
    clrf    WREG
    movff   WREG,buffer+7	    ; limit to 7 chars
    STRCAT_PRINT	""

    movlw       color_greenish
    call	TFT_set_color
    WIN_FRAME_COLOR16   .63,.220,.107,.159; Top, Bottom, Left, Right

    rcall       logbook_preloop_tasks       ; Clear some flags and set to Speed_eco
display_details_loop:
    btfsc		switch_left		; SET/MENU?
    bra			logbook_page3		; Details, 2nd page
    btfsc		switch_right		; ENTER?
    bra			exit_profileview	; back to list
    rcall		log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
    btfsc		sleepmode		; Timeout?
    bra			exit_profileview	; back to list
    bra			display_details_loop    ; wait for something to do

    global  logbook_preloop_tasks
logbook_preloop_tasks:
	movlw       CCP1CON_VALUE               ; See hwos.inc
	movwf       CCP1CON                     ; Power-on backlight
	call	    TFT_standard_color
	bcf	    sleepmode			; clear some flags
	bcf	    switch_right
	bcf	    switch_left
	clrf	    timeout_counter2
	goto	    speed_normal                ; and return

    
logbook_page3: ; Show even more info
    rcall   log_details_header	; Shows number, time/date and basic dive info

    ; OC/CC Gas List
    LOG_POINT_TO    log_divemode
    call	ext_flash_byte_read_plus            ; 0=OC, 1=CC, 2=Gauge, 3=Apnea into temp1
    WIN_SMALL   .5,.65
    WIN_COLOR   color_greenish
    STRCPY_TEXT_PRINT   tGaslist
    bcf		leftbind
    LOG_POINT_TO    log_gas1
    movlw	color_white				; Color for Gas 1
    call	TFT_set_color				; Set Color...
    WIN_SMALL   .5,.90
    rcall       log_show_gas_common2
    movlw	color_green				; Color for Gas 2
    call	TFT_set_color				; Set Color...
    WIN_SMALL   .5,.115
    rcall       log_show_gas_common2
    movlw	color_red				; Color for Gas 3
    call	TFT_set_color				; Set Color...
    WIN_SMALL   .5,.140
    rcall       log_show_gas_common2
    movlw	color_yellow				; Color for Gas 4
    call	TFT_set_color				; Set Color...
    WIN_SMALL   .5,.165
    rcall       log_show_gas_common2
    movlw	color_cyan  				; Color for Gas 5
    call	TFT_set_color				; Set Color...
    WIN_SMALL   .5,.190
    rcall       log_show_gas_common2

    movlw       color_lightblue
    call	TFT_set_color
    WIN_FRAME_COLOR16   .63,.220,.2,.90; Top, Bottom, Left, Right

    rcall       logbook_preloop_tasks       ; Clear some flags and set to Speed_eco
display_details2_loop:
    btfsc		switch_left		; SET/MENU?
    goto		logbook_page4		; Show more info
    btfsc		switch_right		; ENTER?
    bra			exit_profileview	; back to list
    rcall		log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
    btfsc		sleepmode		; Timeout?
    bra			exit_profileview	; back to list
    bra			display_details2_loop   ; wait for something to do	

logbook_page4: ; Show even more info
    rcall   log_details_header	; Shows number, time/date and basic dive info
    
        ; Setpoint list
    LOG_POINT_TO    log_sp1
    WIN_SMALL   .5,.65
    WIN_COLOR   color_greenish
    STRCPY_TEXT_PRINT   tFixedSetpoints
    call	    TFT_standard_color
    WIN_SMALL   .5,.90
    rcall       log_show_sp_common
    WIN_SMALL   .5,.115
    rcall       log_show_sp_common
    WIN_SMALL   .5,.140
    rcall       log_show_sp_common
    WIN_SMALL   .5,.165
    rcall       log_show_sp_common
    WIN_SMALL   .5,.190
    rcall       log_show_sp_common

    movlw       color_greenish
    call	TFT_set_color
    WIN_FRAME_COLOR16   .63,.220,.2,.112; Top, Bottom, Left, Right

    rcall       logbook_preloop_tasks       ; Clear some flags and set to Speed_eco
display_details3_loop:
    btfsc		switch_left		; SET/MENU?
    goto		display_profile2        ; Show the profile view again
    btfsc		switch_right		; ENTER?
    bra			exit_profileview	; back to list
    rcall		log_screendump_and_onesecond    ; Check if we need to make a screenshot and check for new second
    btfsc		sleepmode		; Timeout?
    bra			exit_profileview	; back to list
    bra			display_details3_loop   ; wait for something to do	

    
    
log_details_header:    
    clrf        CCP1CON				; stop PWM
    bcf         PORTC,2				; Pull PWM out to GND
    call	TFT_ClearScreen			; Clear screen

; Set ext_flash pointer to "#divesecs-oldest" dive
; compute read_int_eeprom .2 - divesecs
; Read required header data for profile display
; look in header for pointer to begin of diveprofile (Byte 2-4)
; Set pointer (ext_flash_log_pointer:3) to this address, start drawing

	decf	divesecs,F		;-1
	read_int_eeprom .2
	movf	EEDATA,W
	bcf		STATUS,C
	subfwb	divesecs,W		; max. dives (low value) - divesecs
	movwf	lo				; result
	incf	divesecs,F		;+1
	; Set ext_flash_address:3 to TOC entry of this dive
	; 1st: 200000h-200FFFh -> lo=0
	; 2nd: 201000h-201FFFh -> lo=1
	; 3rd: 202000h-202FFFh -> lo=2
	; 256: 2FF000h-2FFFFFh -> lo=255 (And hi>0...)
	clrf		ext_flash_address+0
	clrf		ext_flash_address+1
	movlw		0x20
	movwf		ext_flash_address+2
	movlw		.16
	mulwf		lo					; lo*16 = offset to 0x2000 (up:hi)
	movf		PRODL,W
	addwf		ext_flash_address+1,F
	movf		PRODH,W
	addwfc		ext_flash_address+2,F
	; pointer at the first 0xFA of header
	rcall        logbook_show_divenumber             ; Show the dive number in medium font
	; Show date and time in first row
    	WIN_SMALL	.59,.10
	LOG_POINT_TO    log_date
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+2		; Year
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+0		; Month
	call		ext_flash_byte_read_plus
	movff		temp1,convert_value_temp+1		; Day
	call		TFT_convert_date			; converts into "DD/MM/YY" or "MM/DD/YY" or "YY/MM/DD" in postinc2
	PUTC		"-"
	call		ext_flash_byte_read_plus		; hour
	movff		temp1,lo
	call		ext_flash_byte_read_plus		; Minutes
	movff		temp1,hi
	output_99x						; hour
	PUTC		':'
	movff		hi,lo			
	output_99x						; minute
	STRCAT_PRINT	""					; Display 1st row of details

	; Show max depth and dive time
	WIN_SMALL	.5,.35
	STRCAT		"Max:"
	LOG_POINT_TO    log_max_depth
	call		ext_flash_byte_read_plus	; read max depth
	movff		temp1,lo				
	call		ext_flash_byte_read_plus	; read max depth
	movff		temp1,hi

	TSTOSS		opt_units			; 0=Meters, 1=Feets
	bra		logbook_page2_depth_metric
	; imperial
	call		convert_mbar_to_feet       	; convert value in lo:hi from mbar to feet
	PUTC	' '
	bcf		leftbind
	output_16_3
	STRCAT_TEXT	tFeets
	bra		logbook_page2_depth_common

logbook_page2_depth_metric:
	bsf		leftbind
	output_16dp	d'3'					; max. depth
	STRCAT_TEXT     tMeters

logbook_page2_depth_common:	
	STRCAT		" - "
	call		ext_flash_byte_read_plus		; divetime in minutes	
	movff		temp1,lo
	call		ext_flash_byte_read_plus	
	movff		temp1,hi				; divetime in minutes

	bsf		leftbind
	output_16						; divetime minutes
	PUTC		"m"
        LOG_POINT_TO    log_divetime+.2
	call		ext_flash_byte_read_plus				; read divetime seconds
	movff		temp1,lo
	bsf		leftbind
	output_99x							; divetime seconds
	call	TFT_standard_color
	STRCAT_PRINT    "s"
;    ; Dive mode
;        LOG_POINT_TO    log_divemode
;        call		ext_flash_byte_read_plus            ; Read divemode
;        movff       temp1,lo
;	call        TFT_display_decotype_surface1       ; "strcat_print"s divemode (OC, CC, APNEA or GAUGE)
	return
    
    
log_show_sp_common:
    lfsr		FSR2,buffer
	call		ext_flash_byte_read_plus					; Read setpoint
    movff       temp1,lo
   	clrf        hi
    bsf         leftbind
    output_16dp d'3'
    bcf         leftbind
    STRCAT_TEXT tbar
    PUTC        " "
    call		ext_flash_byte_read_plus					; change depth
    movff		temp1,lo

	TSTOSS	opt_units               ; 0=Meters, 1=Feets
	bra		log_show_sp_common_metric
    movf    lo,W
    mullw   .100                    ; convert meters to mbar
    movff   PRODL,lo
    movff   PRODH,hi
	call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
    output_16
    STRCAT_TEXT	 tFeets				; "ft"
    bra     log_show_sp_common_common
log_show_sp_common_metric:
    output_8
    STRCAT_TEXT	tMeters				; "m"
log_show_sp_common_common:
	STRCAT_PRINT	""
    return

log_show_gas_common2:   ; as log_show_gas_common but with change depth
	lfsr		FSR2,buffer
	call		ext_flash_byte_read_plus					; current O2
	movff		temp1,lo
	call		ext_flash_byte_read_plus					; current He
	movff		temp1,hi
    call		customview_show_mix                         ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
	call		ext_flash_byte_read_plus					; change depth
    movff		temp1,lo

	TSTOSS	opt_units               ; 0=Meters, 1=Feets
	bra		log_show_gas_common2_metric
    movf    lo,W
    mullw   .100                    ; convert meters to mbar
    movff   PRODL,lo
    movff   PRODH,hi
	call	convert_mbar_to_feet    ; convert value in lo:hi from mbar to feet
    output_16
    STRCAT_TEXT	 tFeets				; "ft"
    bra     log_show_gas_common2_common
log_show_gas_common2_metric:
    output_8
    STRCAT_TEXT	tMeters				; "m"
log_show_gas_common2_common:
	bcf		leftbind
    call	ext_flash_byte_read_plus	; Gas Type
	STRCAT_PRINT	""
    return

	END