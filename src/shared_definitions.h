#ifdef xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;
;    shared_definitions.h
;
;    Declare variables used both in C and ASM code
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;    Copyright (c) 2010, JD Gascuel.
;=============================================================================
; RATIONAL
;
; We must enforce good data passing between the C and the ASM parts of code.
; The previous design used two independant definitions of each variable,
; one in C, one in ASM. If they did not match, no error was generated, and
; anything can happend at runtime...
;
; The new design use LINKING to ensure the variables defined in C are at the
; same address when used in ASM code. And it uses a unique declaration file
; (with suitable macros) to make sure they have the same size in both language.
;
; HISTORY
;  2011-01-20: [jDG] Creation.
;
; NOTE
;
; This file have to obey both ASM and C syntax. The only common directives
; are #if/#ifdef/#endif and the #define, so they are eavily used to do the
; trick.
;
; BUGS
;=============================================================================
; HISTORY
#endif

#ifdef __18CXX
    //------------------------------------------------------------------------
    // C-style declarations:
#   ifndef TEST_MAIN
#       define VAR_UCHAR(n)      extern unsigned  char n
#       define TAB_UCHAR(n,size) extern unsigned  char n[size]
#       define VAR_UINT(n)       extern unsigned short n
#       define TAB_UINT(n,size)  extern unsigned short n[size]
#   else
#       define VAR_UCHAR(n)      unsigned  char n
#       define TAB_UCHAR(n,size) unsigned  char n[size]
#       define VAR_UINT(n)       unsigned short n
#       define TAB_UINT(n,size)  unsigned short n[size]
#   endif
#else
    ;-------------------------------------------------------------------------
    ; ASM-style declarations:
#define VAR_UCHAR(n)       n    res     1
#define TAB_UCHAR(n,size)  n    res     size
#define VAR_UINT(n)        n    res     2
#define TAB_UINT(n,size)   n    res     2*size
#endif

#ifdef __18CXX
    //---- BANK 3 DATA -------------------------------------------------------
    // Gather all data C-code --> ASM-code
#   pragma udata overlay bank3=0x300
#else
bank3   udata_ovr  0x300
#endif


#ifdef xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    Define model dimensions.
    NUM_COMP  is the number of compartiments in the Bühlmann ZH-L16 model, ie 16.
    NUM_STOPS is the maximum number of stops computed by decoplanning. 
              Note that the deapest stop is roughly limited to 3m * NUM_STOPS
                   (this is assuming all stops up to the surface are used).
              Note also that if the table overflow, extra stops are ignored,
                   and not reported in TTS summing.
    NUM_GAS   is the number of (potentially) active gas considered during
              ascent simulation.
#endif
#define NUM_COMP    0x10
#define NUM_STOPS   0x20
#define NUM_GAS     5

VAR_UINT  (int_O_gtissue_press);
VAR_UINT  (int_O_desaturation_time);       // 
VAR_UINT  (int_O_ascenttime);              // TTS (in minutes)
VAR_UINT  (int_O_extra_ascenttime);        // TTS for @+5min variant (in minutes)
VAR_UINT  (int_O_CNS_fraction);            // new in v.101

VAR_UCHAR (char_O_nullzeit);               // 
VAR_UCHAR (char_O_deco_status);            // Deko state-machine state.
VAR_UCHAR (char_O_deco_last_stop);         // Depth reached during deko planning.
VAR_UCHAR (char_O_gradient_factor);        // 
VAR_UCHAR (char_O_gtissue_no);             // 
VAR_UCHAR (char_O_diluent);                // new in v.101
VAR_UCHAR (char_O_flush_ppO2);             // 2011-05-01: ppO2 from diluant (CCR mode).
VAR_UCHAR (char_O_EAD);                    // 2011-05-01: Added EAD/END in deco model.
VAR_UCHAR (char_O_END);                    // 2011-05-01: Added EAD/END in deco model.
VAR_UCHAR (char_O_relative_gradient_GF);   // new in v.102

VAR_UCHAR (char_O_first_deco_depth);        // Depth of first stop.
VAR_UCHAR (char_O_first_deco_time) ;        // Duration of first stop.
TAB_UCHAR (char_O_deco_depth, NUM_STOPS);   // Fusionned decompression table:
TAB_UCHAR (char_O_deco_time,  NUM_STOPS);   // Both ZH-L16 and L16-GF models.
TAB_UCHAR (char_O_deco_gas,   NUM_STOPS);   // Both ZH-L16 and L16-GF models.
TAB_UCHAR (char_O_deco_time_for_log, NUM_STOPS); // For logging the full decoplan

TAB_UCHAR (char_O_tissue_N2_saturation, NUM_COMP); // Nitrogen compartiment desaturation time, in min.
TAB_UCHAR (char_O_tissue_He_saturation, NUM_COMP); // Helium compartiment desaturation time, in min.

TAB_UINT (int_O_gas_volumes, 5);            // Volumes evaluation for each gas tank, in 0.1 liters.
VAR_UINT (int_O_ceiling);                  // in mbar

#ifdef __18CXX
    //---- BANK 4 DATA -------------------------------------------------------
    // Gather all data ASM-code --> C-code
#   pragma udata overlay bank4=0x400
#else
    ; In ASM, put the same bank, in overlay mode, at the same address
bank4   udata_ovr  0x400
#endif

VAR_UCHAR (char_I_step_is_1min);           // Use 1min integration for tissue and CNS.

VAR_UINT  (int_I_pres_respiration);        // 
VAR_UINT  (int_I_pres_surface);            // 
VAR_UINT  (int_I_temp);                    // new in v101
VAR_UINT  (int_I_divemins);                // Dive time (minutes)
VAR_UCHAR (char_I_temp);                   // new in v101
VAR_UCHAR (char_I_actual_ppO2);            // 
VAR_UCHAR (char_I_first_gas);              // Gas used at start of dive (bottom mix)
VAR_UCHAR (char_I_current_gas);            // Current gas breathed (1..6).
VAR_UCHAR (char_I_N2_ratio);               //
VAR_UCHAR (char_I_He_ratio);               //
VAR_UCHAR (char_I_O2_ratio);               //
VAR_UCHAR (char_I_saturation_multiplier);  // for conservatism/safety values 1.0  no conservatism to 1.5  50% faster saturation
VAR_UCHAR (char_I_desaturation_multiplier);// for conservatism/safety values 0.66  50% slower desaturation to 1.0  no conservatism// consveratism used in calc_tissue , calc_tissue_step_1_min  and sim_tissue_1min 
VAR_UCHAR (char_I_GF_High_percentage);     // new in v.102
VAR_UCHAR (char_I_GF_Low_percentage);      // new in v.102
VAR_UCHAR (char_I_deco_distance);          // 
VAR_UCHAR (char_I_depth_last_deco);        // new in v.101 unit: [m]
VAR_UCHAR (char_I_deco_model);             // new in v.102. 0 == ZH-L16, 1 = ZH-L16-GF (Gradiant factor)
VAR_UCHAR (char_I_bottom_depth);           // Bottom depth for planning (used in gas volume evaluation).
VAR_UCHAR (char_I_bottom_time);            // Bottom time for planning (used in gas volume evaluation).
VAR_UCHAR (char_I_dive_interval);          // Delay before next dive simulation.
VAR_UCHAR (char_I_const_ppO2);             // new in v.101

TAB_UCHAR (char_I_deco_gas_change,NUM_GAS); // new in v.101
TAB_UCHAR (char_I_dil_change,NUM_GAS);      // Must be placed after char_I_deco_gas_change!

TAB_UCHAR (char_I_setpoint_change,NUM_GAS); // Change depths in m
TAB_UCHAR (char_I_deco_N2_ratio, NUM_GAS);  // new in v.101
TAB_UCHAR (char_I_deco_He_ratio, NUM_GAS);  // new in v.101

TAB_UCHAR (char_I_setpoint_cbar,NUM_GAS);         // Setpoints in cbar

VAR_UCHAR (char_I_bottom_usage);            // [l/min]
VAR_UCHAR (char_I_deco_usage);              // [l/min]

VAR_UCHAR (char_I_extra_time);              // [min]
VAR_UCHAR (temp_bankx400);                  // temp
