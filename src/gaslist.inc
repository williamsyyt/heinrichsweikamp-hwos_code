;=============================================================================
;
;   File gaslist.inc
;
;   Interface to OSTC3 gas list management.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-08-11 : [jDG] Creation.

    ; Utils
    extern  gaslist_strcat_gas
    extern  gaslist_strcat_gas_mod
    extern  gaslist_strcat_setpoint

    ;Setpoint Setup
    extern  gaslist_spplus,gaslist_spdepthplus, gaslist_spdepthminus,gaslist_setSP

    ; Select currently edited gas
    extern  gaslist_setgas, gaslist_gastitle
    
    ; Main gaslist menu
    extern  gaslist_pO2, gaslist_mO2
    extern  gaslist_pHe, gaslist_mHe
    extern  gaslist_show_type
    extern  gaslist_toggle_type
    extern  gaslist_cleanup_list    ; Takes care that only one gas can be first and first has 0m change depth

    ; Depth submennu
    extern  gaslist_pDepth, gaslist_mDepth
    extern  gaslist_MOD_END
    extern  gaslist_reset_mod_title,  gaslist_reset_mod
