;=============================================================================
;
;   File menu_processor.asm
;
;   Routines to handle all OSTC3 graphic/text menus.
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;   2011-05-30 : [jDG] Creation.

    ; Restart menu-system from first icon/line:
    extern  menu_processor_reset
    
    ; Recal last (automatically) saved icon/line when returning from submenu.
    extern  menu_processor_pop

    ; Rexecute the menu block
    extern  menu_processor

    extern  menu_processor_bottom_line

;=============================================================================
; Menus parameters

;NOTE: should be idenric in .inc and .asm !
#define MENU_LINES_MAX  .7              ; Number of lines per screen?
#define MENU_HEIGHT     .27             ; Spacing on screen.
#define MENU_VCENTER    .125            ; Position on screen.

;=============================================================================

COMMON_BEGIN_MENU  MACRO    dynamic, txt, nb_items
        local center
        If nb_items > MENU_LINES_MAX
center      set MENU_VCENTER - (MENU_HEIGHT/2) * MENU_LINES_MAX
        Else
center      set MENU_VCENTER - (MENU_HEIGHT/2) * nb_items
        Endif
        If nb_items <= 0
            Error "Zero items in menu ", x
        Endif

        extern  txt
        call    menu_processor
        ; Push 6 bytes of menu header data.
        db      nb_items, dynamic
        db      LOW(txt), HIGH(txt)
        db      UPPER(txt), center
    ENDM

;=============================================================================
; Macro to generat (and check) menu vertical menu blocks with data.
;
MENU_BEGIN  MACRO   menu_title, nb_items
        COMMON_BEGIN_MENU    0, menu_title, nb_items
    ENDM

MENU_BEGIN_DYNAMIC macro title_proc, nb_items
        COMMON_BEGIN_MENU    1, title_proc, nb_items
    ENDM

;=============================================================================

; Submenu
MENU_CALL   MACRO   txt, proc
        extern  txt
        db      0, 0, 0, 0
        db      LOW(proc),  HIGH(proc), UPPER(proc),     0
        db      LOW(txt),   HIGH(txt)
    ENDM

; Generic option menu
MENU_OPTION MACRO   txt, option, callback
        extern  txt
        extern  option
        db      2, LOW(callback), HIGH(callback), UPPER(callback)
        db      LOW(option),HIGH(option),UPPER(option), 0
        db      LOW(txt),   HIGH(txt)
    ENDM

MENU_DYNAMIC MACRO  callback, proc
        extern  callback
        db      3, LOW(callback), HIGH(callback), UPPER(callback)
        db      LOW(proc),     HIGH(proc),      UPPER(proc),    0
        db      0, 0
    ENDM

MENU_END    MACRO
    ENDM