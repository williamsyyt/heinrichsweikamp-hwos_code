;=============================================================================
;
;   File customview.inc
;
;   Customview for divemode and surfacemode
;
;   Copyright (c) 2011, Matthias Heinrichs, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-08-10 : [mH] Import into OSTC3 sources

    ; Dive mode
	extern	customview_toggle
	extern	customview_second
	extern	customview_minute
    extern  customview_show_mix             ; Put "Nxlo", "Txlo/hi", "Air" or "O2" into Postinc2
    extern  customview_show_change_depth    ; Put " in lo m" or " in lo ft" into Postinc2
    extern  menuview_toggle
    extern  menuview_toggle_reset
    extern  customview_mask

    ; Surface mode
    extern  surf_customview_toggle
    extern  surf_customview_mask

;	extern	surf_customview_second
;	extern	surf_customview_minute
