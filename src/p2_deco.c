// **************************************************************
// p2_deco.c
//
//  Created on: 12.05.2009
//  Author: chsw
//
// **************************************************************

//////////////////////////////////////////////////////////////////////////////
// OSTC - diving computer code
// Copyright (C) 2011 HeinrichsWeikamp GbR
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////////

// *****************************
// ** I N T R O D U C T I O N **
// *****************************
//
// OSTC
//
// code:
// p2_deco_main_c_v101.c
// part2 of the OSTC code
// code with constant O2 partial pressure routines
// under construction !!
//
// summary:
// decompression routines
// for the OSTC experimental project
// written by Christian Weikamp
// last revision __________
// comments added _________
//
// additional files:
// p2_tables_v100.romdata (other files)
// 18f4685_ostc_v100.lkr (linker script)
//
// history:
// 01/03/08 v100: first release candidate
// 03/13/08 v101: start of programming ppO2 code
// 03/13/25 v101a: backup of interrim version with ppO2 calculation
// 03/13/25 v101: open circuit gas change during deco
// 03/13/25 v101: CNS_fraction calculation
// 03/13/26 v101: optimization of tissue calc routines
// 07/xx/08 v102a: debug of bottom time routine
// 09/xx/08 v102d: Gradient Factor Model implemenation
// 10/10/08 v104: renamed to build v103 for v118 stable
// 10/14/08	v104: integration of char_I_depth_last_deco for Gradient Model
// 03/31/09 v107: integration of FONT Incon24
// 05/23/10 v109: 5 gas changes & 1 min timer
// 07/13/10 v110: cns vault added
// 12/25/10 v110: split in three files (deco.c, main.c, definitions.h)
// 2011/01/20: [jDG] Create a common file included in ASM and C code.
// 2011/01/24: [jDG] Make ascenttime an short. No more overflow!
// 2011/01/25: [jDG] Fusion deco array for both models.
// 2011/01/25: [jDG] Use CF(54) to reverse deco order.
// 2011/02/11: [jDG] Reworked gradient-factor implementation.
// 2011/02/15: [jDG] Fixed inconsistencies introduced by gas switch delays.
// 2011/03/21: [jDG] Added gas consumption (CF56 & CF57) evaluation for OCR mode.
// 2011/04/15: [jDG] Store low_depth in 32bits (w/o rounding), for a better stability.
// 2011/04/25: [jDG] Added 1mn mode for CNS calculation, to allow it for decoplanning.
// 2011/04/27: [jDG] Fixed char_O_gradient_factor calculation when model uses gradient-factor.
// 2011/05/02: [jDG] Added "Future TTS" function (CF58).
// 2011/05/17: [jDG] Various cleanups.
// 2011/08/08: [jDG] Computes CNS during deco planning ascent.
// 2011/11/24: [jDG] Slightly faster and better NDL computation.
// 2011/12/17: [mH]  Remove of the useless debug stuff
// 2012/02/24: [jDG] Remove missed stop bug.
// 2012/02/25: [jDG] Looking for a more stable LOW grad factor reference.
// 2012/09/10: [mH]  Fill char_O_deco_time_for_log for logbook write
// 2012/10/05: [jDG] Better deco_gas_volumes accuracy (average depth, switch between stop).
// 2013/03/05: [jDG] Should vault low_depth too.
// 2013/03/05: [jDG] Wrobell remark: ascent_to_first_stop works better with finer steps (2sec).
// 2013/05/08: [jDG] A. Salm remark: NOAA tables for CNS are in ATA, not bar.
// 2013/12/21: [jDG] Fix CNS calculation in decoplan w/o marked gas switch
// 2014/06/16: [jDG] Fix Helium diluant. Fix volumes with many travel mix.
// 2014/06/29: [mH] Compute int_O_ceiling
//
// TODO:
//
// Literature:
// Buhlmann, Albert: Tauchmedizin; 4. Auflage [2002];
// Schr"oder, Kai & Reith, Steffen; 2000; S"attigungsvorg"ange beim Tauchen, das Modell ZH-L16, Funktionsweise von Tauchcomputern; http://www.achim-und-kai.de/kai/tausim/saett_faq
// Morrison, Stuart; 2000; DIY DECOMPRESSION; http://www.lizardland.co.uk/DIYDeco.html
// Balthasar, Steffen; Dekompressionstheorie I: Neo Haldane Modelle; http://www.txfreak.de/dekompressionstheorie_1.pdf
// Baker, Erik C.; Clearing Up The Confusion About "Deep Stops"
// Baker, Erik C.; Understanding M-values; http://www.txfreak.de/understanding_m-values.pdf
//
//

// *********************
// ** I N C L U D E S **
// *********************
#include <math.h>

// ***********************************************
// ** V A R I A B L E S   D E F I N I T I O N S **
// ***********************************************

#include "p2_definitions.h"
#define TEST_MAIN
#include "shared_definitions.h"

// Water vapour partial pressure in the lumb.
#define ppWater        0.0627
#define METER_TO_BAR   0.09985
#define BAR_TO_METER   10.0150      // (1.0/METER_TO_BAR)

// Surface security factor
#define SURFACE_DESAT_FACTOR    0.7042

// *************************
// ** P R O T O T Y P E S **
// *************************

static void calc_hauptroutine(void);
static void calc_nullzeit(void);

static void calc_tissue(PARAMETER unsigned char period);
static void calc_limit(void);

static void clear_tissue(void);
static void calc_ascenttime(void);
static void update_startvalues(void);
static void clear_deco_table(void);
static unsigned char update_deco_table(void);

static void sim_tissue(PARAMETER unsigned char period);
static void sim_limit(PARAMETER float GF_current);
static void sim_extra_time(void);
static void calc_dive_interval(void);

static void calc_gradient_factor(void);
static void calc_wo_deco_step_1_min(void);

static void calc_hauptroutine_data_input(void);
static void calc_hauptroutine_update_tissues(void);
static void calc_hauptroutine_calc_deco(void);
static void sim_ascent_to_first_stop(void);

static unsigned char gas_switch_deepest(void);
static void gas_switch_set(void);

static unsigned char calc_nextdecodepth(void);

//---- Bank 5 parameters -----------------------------------------------------
#ifndef UNIX
#   pragma udata bank5=0x500
#endif

static float			GF_low;
static float			GF_high;
static float			GF_delta;
static float			locked_GF_step;             // GF_delta / low_depth

static unsigned char    temp_depth_limit;
static unsigned char    low_depth;                  // Depth of deepest stop

// Simulation context: used to predict ascent.
static unsigned char	sim_lead_tissue_no;         // Leading compatiment number.
static float			sim_lead_tissue_limit;      // Buhlmann tolerated pressure.

// Real context: what we are doing now.
static float			calc_lead_tissue_limit;     //

static unsigned char	internal_deco_time[NUM_STOPS];
static unsigned char	internal_deco_depth[NUM_STOPS];

static float cns_vault;
static float low_depth_vault;
static float pres_tissue_N2_vault[NUM_COMP];
static float pres_tissue_He_vault[NUM_COMP];

//---- Bank 6 parameters -----------------------------------------------------
#ifndef UNIX
#   pragma udata bank6=0x600
#endif

static unsigned char	ci;
static float 		pres_respiration;
static float		pres_surface;
static float		temp_deco;
static float		ppN2;
static float		ppHe;
static float		temp_tissue;
static float		N2_ratio;       // Breathed gas nitrogen ratio.
static float		He_ratio;       // Breathed gas helium ratio.
static float 		var_N2_a;       // Buhlmann a, for current N2 tissue.
static float 		var_N2_b;       // Buhlmann b, for current N2 tissue.
static float 		var_He_a;       // Buhlmann a, for current He tissue.
static float 		var_He_b;       // Buhlmann b, for current He tissue.
static float  		var_N2_e;       // Exposition, for current N2 tissue.
static float  		var_He_e;       // Exposition, for current He tissue.
static float            var_N2_ht;      // Half-time for current N2 tissue.
static float            var_He_ht;      // Half-time for current N2 tissue.

static float            pres_diluent;                   // new in v.101
static float            const_ppO2;                     // new in v.101

static unsigned char    sim_gas_last_depth;             // Depth of last used gas, to detected a gas switch.
static unsigned char    sim_gas_last_used;              // Number of last used gas, to detected a gas switch.
static unsigned short   sim_dive_mins;                  // Simulated dive time.
static float		calc_N2_ratio;                  // Simulated (switched) nitrogen ratio.
static float		calc_He_ratio;                  // Simulated (switched) helium ratio.
static float		CNS_fraction;			// new in v.101
static float		float_saturation_multiplier;    // new in v.101
static float		float_desaturation_multiplier;  // new in v.101
static float		float_deco_distance;            // new in v.101

static unsigned char    deco_gas_change[NUM_GAS];       // new in v.109
static unsigned char	internal_deco_gas  [NUM_STOPS];

//---- Bank 7 parameters -----------------------------------------------------
#ifndef UNIX
#   pragma udata bank7=0x700
#endif
    // Keep order of 0x700 variables
float  pres_tissue_N2[NUM_COMP];
float  pres_tissue_He[NUM_COMP];
float  sim_pres_tissue_N2[NUM_COMP];             // 16 floats = 64 bytes.
float  sim_pres_tissue_He[NUM_COMP];             // 16 floats = 64 bytes.

//---- Bank 8 parameters -----------------------------------------------------
#ifndef UNIX
#   pragma udata overlay bank8=0x800
    static char	  md_pi_subst[256];
#   define C_STACK md_pi_subst      // Overlay C-code data stack here, too.
#endif

// Back to bank6 for further tmp data
#ifndef UNIX
#   pragma udata bank6
#endif

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
///////////////////////////// THE LOOKUP TABLES //////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//
// End of PROM code is 17F00, So push tables on PROM top...
//
#ifndef UNIX
#   pragma romdata buhlmann_tables = 0x1DD00  // Needs to be in UPPER bank.
#endif

#include "p2_tables.romdata" 		        // new table for deco_main_v.101 (var_N2_a modified)

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
////////////////////////////// THE SUBROUTINES ///////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//
// all new in v.102
// moved from 0x0D000 to 0x0C000 in v.108
#ifndef UNIX
#   pragma code p2_deco = 0x0C000
#endif

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
///////////////////////  U T I L I T I E S   /////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// Bump to blue-screen when an assert is wrong
#ifdef __DEBUG
void assert_failed(PARAMETER short int line)
{
}
#endif

//////////////////////////////////////////////////////////////////////////////
// When calling C code from ASM context, the data stack pointer and
// frames should be reset. Bank8 is used by stack

#ifdef CROSS_COMPILE
#       define RESET_C_STACK
#else
#   ifdef __DEBUG
#       define RESET_C_STACK fillDataStack();
        void fillDataStack(void)
        {
            _asm
                LFSR    1,C_STACK
                MOVLW   0xCC
        loop:   MOVWF   POSTINC1,0
                TSTFSZ  FSR1L,0
                BRA     loop

                LFSR    1,C_STACK
                LFSR    2,C_STACK
            _endasm
        }
#   else
#       define RESET_C_STACK    \
        _asm                    \
            LFSR    1, C_STACK  \
            LFSR    2, C_STACK  \
        _endasm
#   endif
#endif

//////////////////////////////////////////////////////////////////////////////
// Fast subroutine to read timer 5.
// Note: result is in 1/32 of msecs (30,51757813 us/bit to be precise)
static unsigned short tmr5(void)
{
#ifndef CROSS_COMPILE
    _asm
        movff   0xf7c,PRODL     // TMR5L
        movff   0xf7d,PRODH     // TMR5H
    _endasm                     // result in PRODH:PRODL.
#else
    return 0;
#endif
}


//////////////////////////////////////////////////////////////////////////////
// read buhlmann tables A and B for compatriment ci
//
static void read_buhlmann_coefficients(void)
{
#ifndef CROSS_COMPILE
    // Note: we don't use far rom pointer, because the
    //       24 bits is too complex, hence we have to set
    //       the UPPER page ourself...
    //       --> Set zero if tables are moved to lower pages !
    _asm
        movlw 1
        movwf TBLPTRU,0
    _endasm
#endif

    assert( ci < NUM_COMP );

    // Use an interleaved array (AoS) to access coefficients with a
    // single addressing.
    {
        overlay rom const float* ptr = &buhlmann_ab[4*ci];
        var_N2_a = *ptr++;
        var_N2_b = *ptr++;
        var_He_a = *ptr++;
        var_He_b = *ptr++;
    }
}

//////////////////////////////////////////////////////////////////////////////
// read buhlmann tables for compatriment ci
// If period == 0 : 2sec interval
//              1 : 1 min interval
//              2 : 10 min interval.
static void read_buhlmann_times(PARAMETER char period)
{
#ifndef CROSS_COMPILE
    // Note: we don't use far rom pointer, because the
    //       24 bits is to complex, hence we have to set
    //       the UPPER page ourself...
    //       --> Set zero if tables are moved to lower pages !
    _asm
        movlw 1
        movwf TBLPTRU,0
    _endasm
#endif

    assert( ci < NUM_COMP );

    // Integration intervals.
    switch(period)
    {
    case 0: //---- 2 sec -----------------------------------------------------
        {
            overlay rom const float* ptr = &e2secs[2*ci];
            var_N2_e = *ptr++;
            var_He_e = *ptr++;
        }
        break;

    case 1: //---- 1 min -----------------------------------------------------
       {
            overlay rom const float* ptr = &e1min[2*ci];
            var_N2_e = *ptr++;
            var_He_e = *ptr++;
        }
        break;

    case 2: //---- 10 min ----------------------------------------------------
        {
            overlay rom const float* ptr = &e10min[2*ci];
            var_N2_e = *ptr++;
            var_He_e = *ptr++;
        }
        break;

    default:
        assert(0);  // Never go there...
    }
}

//////////////////////////////////////////////////////////////////////////////
// read buhlmann tables for compatriment ci
//
static void read_buhlmann_ht(void)
{

#ifndef CROSS_COMPILE
    // Note: we don't use far rom pointer, because the
    //       24 bits is to complex, hence we have to set
    //       the UPPER page ourself...
    //       --> Set zero if tables are moved to lower pages !
    _asm
        movlw 1
        movwf TBLPTRU,0
    _endasm
#endif

    assert( ci < NUM_COMP );
    {
        overlay rom const float* ptr = &buhlmann_ht[2*ci];
        var_N2_ht = *ptr++;
        var_He_ht = *ptr++;
    }

    assert( 4.0    <= var_N2_ht && var_N2_ht <= 635.0 );
    assert( 1.5099 <= var_He_ht && var_He_ht <= 240.03 );
}

//////////////////////////////////////////////////////////////////////////////
// calc_nextdecodepth
//
// new in v.102
//
// INPUT, changing during dive:
//      temp_deco
//      low_depth
//
// INPUT, fixed during dive:
//      pres_surface
//      GF_delta
//      GF_high
//      GF_low
//      char_I_depth_last_deco
//      float_deco_distance
//
// RETURN TRUE iff a stop is needed.
//
// OUTPUT
//      locked_GF_step
//      temp_depth_limt
//      low_depth
//
static unsigned char calc_nextdecodepth(void)
{
    //--- Max ascent speed ---------------------------------------------------
    // Recompute leading gas limit, at current depth:
    overlay float depth = (temp_deco - pres_surface) * BAR_TO_METER;

    // At most, ascent 1 minute, at 10m/min == 10.0 m.
    overlay float min_depth = (depth > 10.0) ? (depth - 10.0) : 0.0;

    // Do we need to stop at current depth ?
    overlay unsigned char need_stop = 0;

    assert( depth >= -0.2 );        // Allow for 200mbar of weather change.

    //---- ZH-L16 + GRADIENT FACTOR model ------------------------------------
    if( char_I_deco_model != 0 )
    {
        overlay unsigned char first_stop = 0;
        overlay float p;

        sim_limit( GF_low );
        p = sim_lead_tissue_limit - pres_surface;
        if( p <= 0.0f )
            goto no_deco_stop;          // We can surface directly...

        p *= BAR_TO_METER;
        if( p < min_depth )
            goto no_deco_stop;          // First stop is higher than 1' ascent.

        first_stop = 3 * (short)(0.99999 + p*0.333333);
        assert( first_stop < 128 );

        // Apply correction for the shallowest stop.
        if( first_stop == 3 )                           // new in v104
            first_stop = char_I_depth_last_deco;        // Use last 3m..6m instead.

        // Store the deepest point needing a deco stop as the LOW reference for GF.
        // NOTE: following stops will be validated using this LOW-HIGH gf scale,
        //       so if we want to keep coherency, we should not validate this stop
        //       yet, but apply the search to it, as for all the following stops afterward.
        if( first_stop > low_depth )
        {
            low_depth = first_stop;
            locked_GF_step = GF_delta / first_stop;
        }

        // We have a stop candidate.
        // But maybe ascending to the next stop will diminish the constraint,
        // because the GF might decrease more than the preassure gradient...
        while(first_stop > 0)
        {
            overlay unsigned char next_stop;            // Next depth (0..90m)
            overlay float pres_stop;                    // Next pressure (bar)

            // Check max speed, or reaching surface.
            if( first_stop <= min_depth )
                goto no_deco_stop;

            if( first_stop <= char_I_depth_last_deco )  // new in v104
                next_stop = 0;
            else if( first_stop == 6 )
                next_stop = char_I_depth_last_deco;
            else
                next_stop = first_stop - 3;             // Index of next (upper) stop.

            // Just a check we are indeed above LOW ref.
            assert( next_stop < low_depth );

            // Total preassure at the new stop candidate:
            pres_stop =  next_stop * METER_TO_BAR
                      + pres_surface;

            // Keep GF_low until a first stop depth is found:
            sim_limit( GF_high - next_stop * locked_GF_step );

            // Check upper limit (lowest pressure tolerated):
            if( sim_lead_tissue_limit >= pres_stop )    // check if ascent to next deco stop is ok
                goto deco_stop_found;

            // Else, validate that stop and loop...
            first_stop = next_stop;
        }
        assert( first_stop == 0 );

no_deco_stop:
        temp_depth_limit = min_depth;
        goto done;

        // next stop is the last validated depth found, aka first_stop
deco_stop_found:
        need_stop = 1;                  // Hit.
        temp_depth_limit = first_stop;  // Stop depth, in meter.

done:
        ;
    }
    else //---- ZH-L16 model -------------------------------------------------
    {
        overlay float pres_gradient;

        // Original model
        // optimized in v.101
        // char_I_depth_last_deco included in v.101

        // Compute sim_lead_tissue_limit too, but just once.
        sim_limit(1.0);

        pres_gradient = sim_lead_tissue_limit - pres_surface;
        if (pres_gradient >= 0)
        {
            pres_gradient *= BAR_TO_METER/3;                        // Bar --> stop number;
            temp_depth_limit = 3 * (short) (pres_gradient + 0.99);  // --> metre : depth for deco
            need_stop = 1;                                          // Hit.

            // Implement last stop at 4m/5m/6m...
            if( temp_depth_limit == 3 )
                temp_depth_limit = char_I_depth_last_deco;
        }
        else
            temp_depth_limit = 0;
    }

    //---- Check gas change --------------------------------------------------
    need_stop |= gas_switch_deepest();  // Update temp_depth_limit if there is a change,

    return need_stop;
}

//////////////////////////////////////////////////////////////////////////////
// copy_deco_table
//
// Buffer the stops, once computed, so we can continue to display them
// while computing the next set.
//
static void copy_deco_table(void)
{
    // Copy depth of the first (deepest) stop, because when reversing
    // order, it will be hard to find...
    char_O_first_deco_depth = internal_deco_depth[0];
    char_O_first_deco_time  = internal_deco_time [0];

    {
        overlay unsigned char x, y;

        for(x=0; x<NUM_STOPS; x++)
        {
            char_O_deco_depth[x] = internal_deco_depth[x];
            char_O_deco_time [x] = internal_deco_time [x];
            char_O_deco_gas  [x] = internal_deco_gas  [x];
        }

        //Now fill the char_O_deco_time_for_log array
        //---- First: search the first non-null depth
        for(x=(NUM_STOPS-1); x != 0; --x)
            if( internal_deco_depth[x] != 0 ) break;

        //---- Second: copy to output table (in reverse order)
        for(y=0; y<NUM_STOPS; y++, --x)
        {
            char_O_deco_time_for_log[y] = internal_deco_time [x];

            // Stop only once the last transfer is done.
            if( x == 0 ) break;
        }

        //---- Third: fill table end with null
        for(y++; y<NUM_STOPS; y++)
        {
            char_O_deco_time_for_log [y] = 0;
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// temp_tissue_safety //
//
// outsourced in v.102
//
// Apply safety factors for brand ZH-L16 model.
//
static void temp_tissue_safety(void)
{
    assert( 0.0 <  float_desaturation_multiplier && float_desaturation_multiplier <= 1.0 );
    assert( 1.0 <= float_saturation_multiplier   && float_saturation_multiplier   <= 2.0 );

    if( char_I_deco_model == 0 )
    {
        if( temp_tissue < 0.0 )
            temp_tissue *= float_desaturation_multiplier;
        else
            temp_tissue *= float_saturation_multiplier;
    }
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
// ** THE JUMP-IN CODE **
// ** for the asm code **
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// Called every 2 seconds during diving.
// update tissues every time.
//
// Every 6 seconds (or slower when TTS > 16):
//    - update deco table (char_O_deco_time/depth) with new values.
//    - update ascent time,
//    - set status to zero (so we can check there is new results).
//
void deco_calc_hauptroutine(void)
{
    RESET_C_STACK
    calc_hauptroutine();
    int_O_desaturation_time = 65535;
}

//////////////////////////////////////////////////////////////////////////////
// Reset decompression model:
// + Set all tissues to equilibrium with Air at ambient pressure.
// + Reset last stop to 0m
// + Reset all model output.
void deco_clear_tissue(void)
{
    RESET_C_STACK
    clear_tissue();
}

//////////////////////////////////////////////////////////////////////////////
// Called every 1 min during decoplanning.
// Update tissues for 1 min.
//
void deco_calc_tissue(void)
{
    RESET_C_STACK
    calc_hauptroutine_update_tissues();
}

//////////////////////////////////////////////////////////////////////////////

void deco_calc_wo_deco_step_1_min(void)
{
    RESET_C_STACK
    calc_wo_deco_step_1_min();
    deco_calc_desaturation_time();
}

//////////////////////////////////////////////////////////////////////////////

void deco_calc_dive_interval(void)
{
    RESET_C_STACK
    calc_dive_interval();
}

//////////////////////////////////////////////////////////////////////////////
// Find current gas in the list (if any).
//
// Input:  char_I_current_gas = 1..6
//
// Output: sim_gas_last_depth = 0..5, temp_depth_limit.
//
static void gas_switch_find_current(void)
{
    assert( 0 < char_I_current_gas && char_I_current_gas <= (2*NUM_GAS) );

    if( char_I_current_gas <= NUM_GAS )                 // Gas1..Gas5
    {
        sim_gas_last_used  = char_I_current_gas;

        // Note: if current is first gas, we must find it, but not set
        //       last depth change to surface.
        if( char_I_deco_gas_change[sim_gas_last_used-1] )
            sim_gas_last_depth = char_I_deco_gas_change[sim_gas_last_used-1];
    }
    else
        sim_gas_last_used = 0;                          // Gas 6 = manual set
}

//////////////////////////////////////////////////////////////////////////////
// Find deepest available gas.
//
// Input:  temp_depth_limit,
//         deco_gas_change[]
//         sim_gas_depth_used, sim_dive_mins.
//
// RETURNS TRUE if a stop is needed for gas switch.
//
// Output: temp_depth_limit, sim_gas_depth_used IFF the is a switch.
//
// NOTE: might be called from bottom (when sim_gas_delay and sim_gas_depth_used
//       are null), or during the ascent to make sure we are not passing a
//       stop (in which case both can be already set).
//
static unsigned char gas_switch_deepest(void)
{
    overlay unsigned char switch_deco = 0, switch_last = 0;

    if (char_I_const_ppO2 == 0)
    {
        overlay unsigned char j;

        // Loop over all enabled gas, to find the deepest one,
        // above last used gas, but below temp_depth_limit.
        for(j=0; j<NUM_GAS; ++j)
        {
            // Gas not (yet) allowed ? Skip !
            if( temp_depth_limit > deco_gas_change[j] )
                continue;

            // Gas deeper (or equal) than the current one ? Skip !
            if( sim_gas_last_depth && deco_gas_change[j] >= sim_gas_last_depth )
                continue;

            // First, or deeper ?
            if( switch_deco < deco_gas_change[j] )
            {
                switch_deco = deco_gas_change[j];
                switch_last = j+1;  // 1..5
            }
        }
    }

    // If there is a better gas available
    if( switch_deco )
    {
        assert( !sim_gas_last_depth || sim_gas_last_depth > switch_deco );

        sim_gas_last_depth = switch_deco;
        sim_gas_last_used  = switch_last;
        return 0;
    }
    return 0;
}

//////////////////////////////////////////////////////////////////////////////
// Calculate gas switches
//
//
// Input:  N2_ratio, He_ratio.
//         sim_gas_last_used
//
// Output: calc_N2_ratio, calc_He_ratio
//
static void gas_switch_set(void)
{
    assert( sim_gas_last_used <= NUM_GAS );

    if( sim_gas_last_used == 0 )    // Gas6 = manualy set gas.
    {
        calc_N2_ratio = N2_ratio;
        calc_He_ratio = He_ratio;
    }
    else
    {
        calc_N2_ratio = char_I_deco_N2_ratio[sim_gas_last_used-1] * 0.01;
        calc_He_ratio = char_I_deco_He_ratio[sim_gas_last_used-1] * 0.01;
    }

    assert( 0.0 <= calc_N2_ratio && calc_N2_ratio <= 0.95 );
    assert( 0.0 <= calc_He_ratio && calc_He_ratio <= 1.00 );
    assert( (calc_N2_ratio + calc_He_ratio) <= 1.00 );
}

//////////////////////////////////////////////////////////////////////////////
//
// Input: calc_N2_ratio, calc_He_ratio : simulated gas mix.
//        temp_deco : simulated respiration pressure
//        float_deco_distance : security factor.
//        Water-vapor pressure inside limbs (ppWater).
//
// Output: ppN2, ppHe.
//
static void sim_alveolar_presures(void)
{
    overlay float deco_diluent = temp_deco;                 // new in v.101

    // Take deco offset into account, but not at surface.
    // Note: this should be done on ambiant pressure, hence before
    //       computing the diluant partial pressure...
    if( deco_diluent > pres_surface )
        deco_diluent += float_deco_distance;

    //---- CCR mode : deco gas switch ? --------------------------------------
    if( char_I_const_ppO2 != 0 )
    {
        // In CCR mode, use calc_XX_ratio instead of XX_ratio.
        // Note: PPO2 and ratios are known outside the lumbs, so there is no
        //       ppWater in the equations below:
        deco_diluent -= const_ppO2;
        deco_diluent /= calc_N2_ratio + calc_He_ratio;

        if (deco_diluent > temp_deco)
            deco_diluent = temp_deco;
    }

    if( deco_diluent > ppWater )
    {
        ppN2 = calc_N2_ratio * (deco_diluent - ppWater);
        ppHe = calc_He_ratio * (deco_diluent - ppWater);
    }
    else
    {
        ppN2 = 0.0;
        ppHe = 0.0;
    }
    assert( 0.0 <= ppN2 && ppN2 < 14.0 );
    assert( 0.0 <= ppHe && ppHe < 14.0 );
}

//////////////////////////////////////////////////////////////////////////////
// clear_tissue
//
// optimized in v.101 (var_N2_a)
//
// preload tissues with standard pressure for the given ambient pressure.
// Note: fixed N2_ratio for standard air.
//
static void clear_tissue(void)
{
    overlay float p;

    // Kludge: the 0.0002 of 0.7902 are missing with standard air.
    N2_ratio = 0.7902;
    pres_respiration = int_I_pres_respiration * 0.001;

    p = N2_ratio * (pres_respiration -  ppWater);
    for(ci=0; ci<NUM_COMP; ci++)
    {
        // cycle through the 16 Buhlmann N2 tissues
        pres_tissue_N2[ci] = p;

        // cycle through the 16 Buhlmann tissues for Helium
        pres_tissue_He[ci] = 0.0;
    }

    clear_deco_table();
    char_O_deco_status = 0;
    char_O_nullzeit = 0;
    int_O_ascenttime = 0;
    char_O_gradient_factor = 0;
    char_O_relative_gradient_GF = 0;

    calc_lead_tissue_limit = 0.0;
    char_O_gtissue_no = 0;
}

//////////////////////////////////////////////////////////////////////////////
// calc_hauptroutine
//
// this is the major code in dive mode calculates:
// 		the tissues,
//		the bottom time,
//		and simulates the ascend with all deco stops.
//
// The deco_state sequence is :
//       3 (at surface)
// +---> 0 : calc nullzeit
// |     2 : simulate ascent to first stop (at 10m/min, less that 16x 1min simu)
// | +-> 1 : simulate up to 16min of stops.
// | +------< not finished
// +--------< finish
//
// Added steps 6,5 for @+5 calculation:
//      6 = ascent to first stop (same as 2), except continue to 7
//      7 = same as 1, except loop to 7.
//
static void calc_hauptroutine(void)
{
    static unsigned char backup_gas_used;
    static unsigned char backup_gas_depth;

    calc_hauptroutine_data_input();

    calc_hauptroutine_update_tissues();
    calc_gradient_factor();

    // toggle between calculation for nullzeit (bottom time),
    //                deco stops
    //                and more deco stops (continue)
    switch( char_O_deco_status )
    {
    case 3: //---- At surface: start a new dive ------------------------------
        clear_deco_table();
        copy_deco_table();
        int_O_ascenttime = 0;       // Reset DTR.
        int_O_extra_ascenttime = 0;
        char_O_nullzeit = 0;        // Reset bottom time.
        char_O_deco_status = 0;     // Calc bottom-time/nullzeit next iteration.

        // Values that should be reset just once for the full real dive.
        // This is used to record the lowest stop for the whole dive,
        // Including ACCROSS all simulated ascent.
        low_depth = 0;
        locked_GF_step = 0.0;

        // Reset gas switch history.
        backup_gas_used  = sim_gas_last_used  = 0;
        backup_gas_depth = sim_gas_last_depth = 0;
        sim_dive_mins = 0;
        break;

    case 0: //---- bottom time -----------------------------------------------
    default:
        gas_switch_find_current();              // Lookup for current gas & time.
        gas_switch_set();                       // setup calc_ratio's

        calc_nullzeit();
        if( char_O_nullzeit > 0 )               // Some NDL time left ?
        {
            char_O_deco_status = 0;             // YES: recalc ndl next time.
            clear_deco_table();                 // Also clear stops !
            copy_deco_table();
            char_O_deco_last_stop = 0;          // And last stop (OSTC menu anim)
        }
        else
            char_O_deco_status = 2;             // NO: calc ascent next time.
        break;

    case 2: //---- Simulate ascent to first stop -----------------------------
    case 6: // @+5min variation
        // Check proposed gas at begin of ascent simulation
        sim_dive_mins = int_I_divemins;         // Init current time.

        gas_switch_find_current();              // Lookup for current gas & time.
        gas_switch_set();                       // setup calc_ratio's

        backup_gas_used  = sim_gas_last_used;   // And save for later simu steps.
        backup_gas_depth = sim_gas_last_depth;  // And save for later simu steps.

        sim_ascent_to_first_stop();

        // Calc stops next time (deco or gas switch).
        char_O_deco_status = 1 | ( char_O_deco_status & 4 );
        break;

    case 1: //---- Simulate stops --------------------------------------------
    case 5: // @+5 variation.
        calc_hauptroutine_calc_deco();

        // If simulation is finished, restore the GF low reference, so that
        // next ascent simulation is done from the current depth:
        if( (char_O_deco_status & 3) == 0 )
        {
            sim_gas_last_used  = backup_gas_used;
            sim_gas_last_depth = backup_gas_depth;
        }
        break;
    }
}

//////////////////////////////////////////////////////////////////////////////
// calc_hauptroutine_data_input
//
// Reset all C-code dive parameters from their ASM-code values.
// Detect gas change condition.
//
void calc_hauptroutine_data_input(void)
{
    overlay short int_temp;
    overlay unsigned char g;

    pres_respiration    = int_I_pres_respiration * 0.001;
    pres_surface        = int_I_pres_surface     * 0.001;
    N2_ratio            = char_I_N2_ratio        * 0.01;
    He_ratio            = char_I_He_ratio        * 0.01;
    float_deco_distance = char_I_deco_distance   * 0.01;     // Get offset in BAR

    // ____________________________________________________
    //
    // _____________ G A S _ C H A N G E S ________________
    // ____________________________________________________

    // Keep a margin of 150mbar = 1.50m
    int_temp = (int_I_pres_respiration - int_I_pres_surface)
             + MBAR_REACH_GASCHANGE_AUTO_CHANGE_OFF;

    // Gas are selectable if we did not pass the change depth by more than 1.50m:
    for(g=0; g < NUM_GAS; ++g)
    {
        deco_gas_change[g] = 0;
        if(char_I_deco_gas_change[g])
            if( int_temp > 100 *(short)char_I_deco_gas_change[g] )
                deco_gas_change[g] = char_I_deco_gas_change[g];
    }

    const_ppO2 = char_I_const_ppO2 * 0.01;
    float_desaturation_multiplier = char_I_desaturation_multiplier * 0.01;
    float_saturation_multiplier   = char_I_saturation_multiplier   * 0.01;
    GF_low   = char_I_GF_Low_percentage  * 0.01;
    GF_high  = char_I_GF_High_percentage * 0.01;
    GF_delta = GF_high - GF_low;
}

//////////////////////////////////////////////////////////////////////////////
//
//
void calc_hauptroutine_update_tissues(void)
{
    assert( 0.00 <= N2_ratio && N2_ratio <= 1.00 );
    assert( 0.00 <= He_ratio && He_ratio <= 1.00 );
    assert( (N2_ratio + He_ratio) <= 1.00 );
    assert( 0.800 < pres_respiration && pres_respiration < 14.0 );

    pres_diluent = pres_respiration;
    if( char_I_const_ppO2 != 0 )
    {
        overlay float flush_ppO2 = pres_respiration * (1.0 - N2_ratio - He_ratio);

        pres_diluent -= const_ppO2;
        pres_diluent /= N2_ratio + He_ratio;
        if( pres_diluent < 0.0 )
            pres_diluent = 0.0;
        if( pres_diluent > pres_respiration )
            pres_diluent = pres_respiration;

        char_O_diluent = (unsigned char)(pres_diluent/pres_respiration*100.0 + 0.5);

        if( flush_ppO2 > 2.545) flush_ppO2 = 2.55;
        if( flush_ppO2 < 0.0  ) flush_ppO2 = 0.0;
        char_O_flush_ppO2 = (unsigned char)(flush_ppO2*100.0 + 0.5);
    }

    if( pres_diluent > ppWater )
    {
        overlay float EAD, END;

        ppN2 = N2_ratio * (pres_diluent - ppWater);
        ppHe = He_ratio * (pres_diluent - ppWater);

        // EAD : Equivalent Air Dive. Equivalent depth for the same N2 level
        //       with plain air.
        //       ppN2 = 79% * (P_EAD - ppWater)
        //       EAD = (P_EAD - Psurface) * 10
        //   ie: EAD = (ppN2 / 0.7902 + ppWater -Psurface) * 10
        EAD = (ppN2 / 0.7902 + ppWater - pres_surface) * BAR_TO_METER;
        if( EAD < 0.0 || EAD > 245.5 ) EAD = 0.0;
        char_O_EAD = (unsigned char)(EAD + 0.5);

        // END : Equivalent Narcotic Dive.
        //       Here we count O2 as narcotic too. Hence everything but helium (has a narcosis factor of
        //       0.23 btw). Hence the formula becomes:
        //       END * BarPerMeter * (1.0 - 0.0) - ppWater + Psurface == Pambient - ppHe - ppWater
        //  ie:  END = (Pambient - ppHe - Psurface) * BAR_TO_METER
        //
        // Source cited:
        //       The Physiology and Medicine of Diving by Peter Bennett and David Elliott,
        //       4th edition, 1993, W.B.Saunders Company Ltd, London.
        END = (pres_respiration - ppHe - pres_surface) * BAR_TO_METER;
        if( END < 0.0 || END > 245.5 ) END = 0.0;
        char_O_END = (unsigned char)(END  + 0.5);
    }
    else																		// new in v.101
    {
        ppN2 = 0.0;
        ppHe = 0.0;
        char_O_EAD = char_O_END = 0;
    }

    if(!char_I_step_is_1min)
        calc_tissue(0);
    else
        calc_tissue(1);

    // Calc limit for surface, ie. GF_high.
    calc_limit();

    // Fill int_O_ceiling if ceiling is below the surface
    if ((calc_lead_tissue_limit-pres_surface)>0)
        int_O_ceiling = (short)((calc_lead_tissue_limit-pres_surface)*1000);
    else
        int_O_ceiling = 0;

    int_O_gtissue_press = (short)((pres_tissue_N2[char_O_gtissue_no] + pres_tissue_He[char_O_gtissue_no]) * 1000);
}


//////////////////////////////////////////////////////////////////////////////
// Compute stops.
//
// Note: because this can be very long, break on 16 iterations, and set state
//       to 0 when finished, or to 1 when needing to continue.
// Note: because each iteration might be very long too (~ 66 ms in 1.84beta),
//       break the loop when total time > 512msec.
//
void calc_hauptroutine_calc_deco(void)
{
    overlay unsigned char loop;

    for(loop = 0; loop < 16; ++loop)
    {
        // Limit loops to 512ms, using timer 5:
        if( tmr5() & (512*32) )
            break;

            if( calc_nextdecodepth() )
            {
                if( temp_depth_limit == 0 )
                    goto Surface;

                //---- We hit a stop at temp_depth_limit ---------------------
                temp_deco = temp_depth_limit * METER_TO_BAR // Convert to relative bar,
                              + pres_surface;                   // To absolute.
                if( !update_deco_table() )                  // Adds a one minute stops.
                    goto Surface;                           // Deco table full: abort...
            }
            else
            {
                //---- No stop -----------------------------------------------
                temp_deco -= (10*METER_TO_BAR);             // Ascend 10m, no wait.

                //---- Finish computations once surface is reached -----------
                if( temp_deco <= pres_surface )
                {
Surface:
                    if( char_O_deco_status == 1 )   // Don't in @+5min variant.
                        copy_deco_table();

                    calc_ascenttime();
                    char_O_deco_status = 0;         // calc nullzeit next time.
                    char_O_deco_last_stop = 0;      // Surface reached (to animate menu)
                        return;
                }
            }
        //---- Then update tissue --------------------------------------------
        sim_dive_mins++;            // Advance simulated time by 1 minute.
        gas_switch_set();           // Apply any simulated gas change, once validated.
        sim_alveolar_presures();    // Updates ppN2 and ppHe.
        sim_tissue(1);              // Simulate compartiments for 1 minute.
    }

    // Surface not reached, need more stops... for menu animation.
    char_O_deco_last_stop = temp_depth_limit;   // Reached depth.
}


//////////////////////////////////////////////////////////////////////////////
// Simulation ascention to first deco stop.
//
// Note: because we ascent with a constant speed (10m/mn, ie. 1bar/mn),
//       there is no need to break on more that 16 iterations
//       (or we are already in deep shit).
//
// Input:  pres_respiration
// Output: temp_deco
//
// if char_O_deco_status indicate @+5 variant, add extra time at current depth,
// before ascent.
void sim_ascent_to_first_stop(void)
{
    overlay unsigned char fast = 1; // 1min or 2sec steps.

    update_startvalues();
    clear_deco_table();

    temp_deco = pres_respiration;       // Starts from current real depth.

    // Are we doing the special @+5min variation ?
    if(char_O_deco_status & 4)
        sim_extra_time();

    //---- Loop until first stop, gas switch, or surface is reached ----------
    for(;;)
    {
        overlay float old_deco = temp_deco;     // Pamb backup (bars)

        // Try ascending 1 full minute (fast) or 2sec (!fast):
        if( fast )
            temp_deco -= 10*METER_TO_BAR;   // 1 min, at 10m/min. ~ 1bar.
        else
            temp_deco -= (10.0/30.0)*METER_TO_BAR;  // 2sec at 10m/min.

        if( temp_deco < pres_surface )  // But don't go over surface.
            temp_deco = pres_surface;

        // Recompute sim_lead_tissue_limit at GF_low (deepest stop), because
        // one minute passed.
        sim_limit(GF_low);

        // Did we reach deepest remaining stop ?
        if( temp_deco < sim_lead_tissue_limit )
        {
            temp_deco = old_deco;           // Restore last correct depth,

            if( fast )
            {
                fast = 0;                   // Retry with 2sec steps.
                continue;
            }
            else
                break;                      // Done...
        }

        // Did we reach surface ?
        // NOTE: we should round BEFORE checking surface is reached.
        temp_depth_limit = (unsigned char)(0.5 + (temp_deco - pres_surface) * BAR_TO_METER);
        if( temp_depth_limit == 0 )
        {
            temp_deco = pres_surface;   // Yes: finished !
            break;
        }

        // Check for gas change below new depth ?
        if( gas_switch_deepest() )
        {
            assert( temp_depth_limit > 0);

            temp_deco = temp_depth_limit * METER_TO_BAR + pres_surface;
            break;
        }

        if( fast )
            sim_dive_mins++;            // Advance simulated time by 1 minute.
        sim_alveolar_presures();        // temp_deco --> ppN2/ppHe
        sim_tissue(fast);               // and update tissues for 1 min.
    }
}

//////////////////////////////////////////////////////////////////////////////
// Simulation extra time at the current depth.
//
// This routine is used for @+5min feature.
void sim_extra_time(void)
{
    overlay unsigned char extra = char_I_extra_time;
    do {
        sim_dive_mins++;                // Advance simulated time by 1 minute.
        sim_tissue(1);                  // and update tissues for 1 min.
    } while( --extra != 0 );
}

//////////////////////////////////////////////////////////////////////////////
// calc_tissue
//
// optimized in v.101
//
static void calc_tissue(PARAMETER unsigned char period)
{
    assert( 0.00 <= ppN2 && ppN2 < 11.2 );  // 80% N2 at 130m
    assert( 0.00 <= ppHe && ppHe < 12.6 );  // 90% He at 130m

    for (ci=0;ci<NUM_COMP;ci++)
    {
        read_buhlmann_times(period);        // 2 sec or 1 min period.

        // N2
        temp_tissue = (ppN2 - pres_tissue_N2[ci]) * var_N2_e;
        temp_tissue_safety();
        pres_tissue_N2[ci] += temp_tissue;

        // He
        temp_tissue = (ppHe - pres_tissue_He[ci]) * var_He_e;
        temp_tissue_safety();
        pres_tissue_He[ci] += temp_tissue;
    }
}

//////////////////////////////////////////////////////////////////////////////
// calc_limit
//
// New in v.111 : separated from calc_tissue(), and depends on GF value.
//
static void calc_limit(void)
{
    char_O_gtissue_no = 255;
    calc_lead_tissue_limit = 0.0;

    for(ci=0; ci<NUM_COMP;ci++)
    {
        overlay float N2 = pres_tissue_N2[ci];
        overlay float He = pres_tissue_He[ci];
        overlay float p = N2 + He;

        read_buhlmann_coefficients();
        var_N2_a = (var_N2_a * N2 + var_He_a * He) / p;
        var_N2_b = (var_N2_b * N2 + var_He_b * He) / p;

        // Apply the Eric Baker's varying gradient factor correction.
        // Note: the correction factor depends both on GF and b,
        //       Actual values are in the 1.5 .. 1.0 range (for a GF=30%),
        //       so that can change who is the leading gas...
        // Note: Also depends of the GF. So the calcul is different for
        //       GF_low, current GF, or GF_high...
        //       *BUT* calc_tissue() is used to compute bottom time,
        //       hence what would happend at surface,
        //       hence at GF_high.
        if( char_I_deco_model != 0 )
            p = ( p - var_N2_a * GF_high) * var_N2_b
              / (GF_high + var_N2_b * (1.0 - GF_high));
        else
            p = (p - var_N2_a) * var_N2_b;
        if( p < 0.0 ) p = 0.0;

        if( p > calc_lead_tissue_limit )
        {
            char_O_gtissue_no = ci;
            calc_lead_tissue_limit = p;
        }
    }

    assert( char_O_gtissue_no < NUM_COMP );
    assert( 0.0 <= calc_lead_tissue_limit && calc_lead_tissue_limit <= 14.0);
}

//////////////////////////////////////////////////////////////////////////////
// calc_nullzeit
//
// calculates the remaining bottom time
//
// NOTE: Erik Baker's closed formula works for Nitroxes. Trimix adds a second
//       exponential term to the M-value equation, making it impossible to
//       invert... So we have to make a fast-simu until we find a better way.
//
// Input:  pres_respiration
// Output: char_O_nullzeit
//
static void calc_nullzeit(void)
{
    //---- Compute ppN2 and ppHe ---------------------------------------------
    temp_deco = pres_respiration;
    sim_alveolar_presures();

    char_O_nullzeit = 240;
    for(ci=0; ci<NUM_COMP; ci++)
    {
        //---- Read A/B values and loading factor for N2 and He --------------
        overlay float tN2 = pres_tissue_N2[ci];
        overlay float tHe = pres_tissue_He[ci];
        overlay float t = tN2 + tHe;
        overlay unsigned char ndl;
        overlay unsigned char period = 10;

        read_buhlmann_coefficients();
        read_buhlmann_times(2);             // Starts with a 10min period.

        //---- Simulate for that tissue --------------------------------------
        // NOTE: No need to simulate for longuer than the already found NDL.
        for(ndl=0; ndl<char_O_nullzeit;)
        {
            //---- Compute updated mix M-value at surface
            overlay float a = (var_N2_a * tN2 + var_He_a * tHe) / t;
            overlay float b = (var_N2_b * tN2 + var_He_b * tHe) / t;
            overlay float M0 = (a + pres_surface/b);

            //---- Add 10min/1min to N2/He tissues
            overlay float dTN2 = (ppN2 - tN2) * var_N2_e;
            overlay float dTHe = (ppHe - tHe) * var_He_e;

            //---- Apply security margin when using the non-GF model
            if( char_I_deco_model == 0 )
            {
                dTN2 *= float_saturation_multiplier;
                dTHe *= float_saturation_multiplier;
            }
            else // Or GF-based model
                M0 = GF_high * (M0 - pres_surface) + pres_surface;

            //---- Simulate off-gasing while going to surface
            // TODO !
            // dTN2 -= exp( ... ascent time ... ppN2...)
            // dTHe -= exp( ... ascent time ... ppHe...)

            //---- Ok now, and still ok to surface after 1 or 10 minutes ?
            if( (t <= M0) && (t + dTN2 + dTHe <= M0) )
            {
                tN2 += dTN2;    // YES: apply gas loadings,
                tHe += dTHe;
                t = tN2 + tHe;
                ndl += period;  // increment NDL,
                continue;       // and loop.
            }

            //---- Should we retry with smaller steps ?
            if( period == 10 )
            {
                read_buhlmann_times(1); // 1min coefs.
                period = 1;
                continue;
            }

            //---- ELSE make a linear approx for the last minute
            // Usefull to have a meaningfull rounding of NDL.
            // But ONLY it positive (negativ casted to unsigned is bad).
            if( M0 > t )
                ndl += (unsigned char)(0.5f + (M0-t)/(dTN2+dTHe));
            break;
        }

        // Keep the shortest NDL found
        if( ndl < char_O_nullzeit )
            char_O_nullzeit = ndl;
    }
}

//////////////////////////////////////////////////////////////////////////////
// calc_ascenttime
//
// Summup ascent from bottom to surface, at 1 bar/min, 1min for last 3 meters,
// and all stops.
//
// Result in int_O_ascenttime, or int_O_extra_ascenttime if in @+5min variant.
static void calc_ascenttime(void)
{
    overlay unsigned char x;
    overlay unsigned short sum;

    // + 0.7 to count 1 minute ascent time from 3 metre to surface
    overlay float ascent = pres_respiration - pres_surface + 0.7;
    if (ascent < 0.0)
        ascent = 0.0;
    sum = (unsigned short)(ascent + 0.99);

    for(x=0; x<NUM_STOPS && internal_deco_depth[x]; x++)
        sum += (unsigned short)internal_deco_time[x];

    if( char_O_deco_status == 1 )
        int_O_ascenttime = sum;
    else
        int_O_extra_ascenttime = sum;

}

//////////////////////////////////////////////////////////////////////////////
// update_startvalues
//
// updated in v.102
//
void update_startvalues(void)
{
    overlay unsigned char x;

    // Start ascent simulation with current tissue partial pressures.
    for(x=0; x<NUM_COMP; x++)
    {
        sim_pres_tissue_N2[x] = pres_tissue_N2[x];
        sim_pres_tissue_He[x] = pres_tissue_He[x];
    }

    // No leading tissue (yet) for this ascent simulation.
    sim_lead_tissue_limit = 0.0;
    sim_lead_tissue_no = 255;
}

//////////////////////////////////////////////////////////////////////////////
// sim_tissue
//
// optimized in v.101
//
// Function very simular to calc_tissue, but:
//   + Use a 1min or 10min period.
//   + Do it on sim_pres_tissue, instead of pres_tissue.
static void sim_tissue(PARAMETER unsigned char period)
{
    assert( 0.00 <= ppN2 && ppN2 < 11.2 );  // 80% N2 at 130m
    assert( 0.00 <= ppHe && ppHe < 12.6 );  // 90% He at 130m

    for(ci=0; ci<NUM_COMP; ci++)
    {
        read_buhlmann_times(period);        // 1 or 10 minute(s) interval

        // N2
        temp_tissue = (ppN2 - sim_pres_tissue_N2[ci]) * var_N2_e;
        temp_tissue_safety();
        sim_pres_tissue_N2[ci] += temp_tissue;

        // He
        temp_tissue = (ppHe - sim_pres_tissue_He[ci]) * var_He_e;
        temp_tissue_safety();
        sim_pres_tissue_He[ci] += temp_tissue;
    }
}

//////////////////////////////////////////////////////////////////////////////
// sim_limit()
//
// New in v.111
//
// Function separated from sim_tissue() to allow recomputing limit on
// different depth, because it depends on current gradient factor.
//
static void sim_limit(PARAMETER float GF_current)
{
    assert( 0.0 < GF_current && GF_current <= 1.0f);

    sim_lead_tissue_limit = 0.0;
    sim_lead_tissue_no = 0;             // If no one is critic, keep first tissue.

    for(ci=0; ci<NUM_COMP; ci++)
    {
        overlay float N2 = sim_pres_tissue_N2[ci];
        overlay float He = sim_pres_tissue_He[ci];
        overlay float p = N2 + He;

        read_buhlmann_coefficients();
        var_N2_a = (var_N2_a * N2 + var_He_a * He) / p;
        var_N2_b = (var_N2_b * N2 + var_He_b * He) / p;

        // Apply the Eric Baker's varying gradient factor correction.
        // Note: the correction factor depends both on GF and b,
        //       Actual values are in the 1.5 .. 1.0 range (for a GF=30%),
        //       so that can change who is the leading gas...
        // Note: Also depends of the GF_current...
        if( char_I_deco_model != 0 )
            p = ( p - var_N2_a * GF_current)
              / (GF_current / var_N2_b + 1.0 - GF_current);
        else
            p = (p - var_N2_a) * var_N2_b;
        if( p < 0.0 ) p = 0.0;

        if( p > sim_lead_tissue_limit )
        {
            sim_lead_tissue_no = ci;
            sim_lead_tissue_limit = p;
        }
    } // for ci

    assert( sim_lead_tissue_no < NUM_COMP );
    assert( 0.0 <= sim_lead_tissue_limit && sim_lead_tissue_limit <= 14.0 );
}

//////////////////////////////////////////////////////////////////////////////
// clear_deco_table
//
// unchanged in v.101
//
static void clear_deco_table(void)
{
    overlay unsigned char x;

    for(x=0; x<NUM_STOPS; ++x)
    {
        internal_deco_time [x] = 0;
        internal_deco_depth[x] = 0;
    }
}

//////////////////////////////////////////////////////////////////////////////
// update_deco_table
//
// Add 1 min to current stop.
//
// Inputs:
//      temp_depth_limit = stop's depth, in meters.
// In/Out:
//      internal_deco_depth[] : depth (in metres) of each stops.
//      internal_deco_time [] : time (in minutes) of each stops.
//
static unsigned char update_deco_table()
{
    overlay unsigned char x;
    assert( temp_depth_limit < 128 );   // Can't be negativ (overflown).
    assert( temp_depth_limit > 0 );     // No stop at surface...

    for(x=0; x<NUM_STOPS; ++x)
    {
        // Make sure deco-stops are recorded in order:
        assert( !internal_deco_depth[x] || temp_depth_limit <= internal_deco_depth[x] );

        if( internal_deco_depth[x]== temp_depth_limit )
        {
            // Do not overflow (max 255')
            if( internal_deco_time[x] < 255 )
            {
                internal_deco_time[x]++;
                return 1;
            }
            // But store extra in the next stop...
        }

        if( internal_deco_depth[x] == 0 )
        {
            internal_deco_depth[x] = temp_depth_limit;

            internal_deco_time[x]  = 1;
            internal_deco_gas[x] = sim_gas_last_used;
            return 1;
        }
    }

    // Can't store stops at more than 96m.
    // Or stops at less that 3m too.
    // Just do nothing with that...
    return 0;
}

//////////////////////////////////////////////////////////////////////////////
// calc_gradient_factor
//
// optimized in v.101 (var_N2_a)
// new code in v.102
//
static void calc_gradient_factor(void)
{
    overlay float gf;
    overlay float N2 = pres_tissue_N2[char_O_gtissue_no];
    overlay float He = pres_tissue_He[char_O_gtissue_no];

    assert( char_O_gtissue_no < NUM_COMP );
    assert( 0.800 <= pres_respiration && pres_respiration < 14.0 );

    // tissue > respiration (currently off-gasing)
    // GF =   0% when respiration == tissue, ie. bubbles are at equilibrium.
    // GF = 100% when respiration == limit.
    temp_tissue = N2 + He;
    if( temp_tissue <= pres_respiration )
        gf = 0.0;
    else
    {
        overlay float limit = calc_lead_tissue_limit;
        // NOTE: in GF model, calc_lead_tissue_limit include already the
        //       correction due to gradient factor. To compute the actual
        //       current GF, we need to (re-)compute the raw ambiant-pressure
        //       limit from the Buhlmann model.
        if( char_I_deco_model != 0 )
        {
            ci = char_O_gtissue_no;
            read_buhlmann_coefficients();
            var_N2_a = (var_N2_a * N2 + var_He_a * He) / temp_tissue;
            var_N2_b = (var_N2_b * N2 + var_He_b * He) / temp_tissue;
            limit = (temp_tissue - var_N2_a) * var_N2_b;
        }

        gf = (temp_tissue  - pres_respiration)
           / (temp_tissue  - limit)
           * 100.0;
        if( gf > 254.5 ) gf = 255.0;
        if( gf < 0.0   ) gf = 0.0;
    }
    char_O_gradient_factor = (unsigned char)(gf+0.5f);

    if( char_I_deco_model != 0 )        // calculate relative gradient factor
    {
        overlay float rgf;

        if( low_depth < 3 )
            rgf = GF_high;
        else
        {
            overlay float temp1 = low_depth * METER_TO_BAR;
            overlay float temp2 = pres_respiration - pres_surface;

            if (temp2 <= 0)
                rgf = GF_high;
            else if (temp2 >= temp1)
                rgf = GF_low;
            else
                rgf = GF_low + (temp1 - temp2)/temp1*GF_delta;
        }

        rgf = gf / rgf; // gf is already in percent
        if( rgf <   0.0 ) rgf =   0.0;
        if( rgf > 254.5 ) rgf = 255.0;
            char_O_relative_gradient_GF  = (unsigned char)(rgf+0.5f);
    }
    else
        char_O_relative_gradient_GF = char_O_gradient_factor;
}

//////////////////////////////////////////////////////////////////////////////
// deco_calc_desaturation_time
//
// FIXED N2_ratio
// unchanged in v.101
// Inputs:  int_I_pres_surface, ppWater, char_I_desaturation_multiplier
// Outputs: int_O_desaturation_time, char_O_tissue_saturation[0..31]
//
void deco_calc_desaturation_time(void)
{
    RESET_C_STACK

    assert( 800 < int_I_pres_surface && int_I_pres_surface < 1100 );
    assert( 0 < char_I_desaturation_multiplier && char_I_desaturation_multiplier <= 100 );

    N2_ratio = 0.7902; // FIXED sum as stated in buhlmann
    pres_surface = int_I_pres_surface * 0.001;
    ppN2 = N2_ratio * (pres_surface - ppWater);
    int_O_desaturation_time = 0;
    float_desaturation_multiplier = char_I_desaturation_multiplier * (0.01 * SURFACE_DESAT_FACTOR);

    for(ci=0; ci<NUM_COMP; ci++)
    {
        overlay unsigned short desat_time;    // For a particular compartiment, in min.
        overlay float temp1;
        overlay float temp2;
        overlay float temp3;
        overlay float temp4;

        read_buhlmann_ht();

        // saturation_time (for flight) and N2_saturation in multiples of halftime
        // version v.100: 1.1 = 10 percent distance to totally clean (totally clean is not possible, would take infinite time )
        // new in version v.101: 1.07 = 7 percent distance to totally clean (totally clean is not possible, would take infinite time )
        // changes in v.101: 1.05 = 5 percent dist to totally clean is new desaturation point for display and NoFly calculations
        // N2
        temp1 = 1.05 * ppN2 - pres_tissue_N2[ci];
        temp2 = ppN2 - pres_tissue_N2[ci];
        if (temp2 >= 0.0)
            temp1 = 0.0;
        else
            temp1 = temp1 / temp2;

        if( 0.0 < temp1 && temp1 < 1.0 )
        {
            // 0.6931 is ln(2), because the math function log() calculates with a base of e not 2 as requested.
            // minus because log is negative.
            temp1 = log(1.0 - temp1) / -0.6931; // temp1 is the multiples of half times necessary.
            temp2 = var_N2_ht * temp1 / float_desaturation_multiplier; // time necessary (in minutes ) for complete desaturation (see comment about 5 percent)
        }
        else
        {
            temp1 = 0.0;
            temp2 = 0.0;
        }

        // He
        temp3 = 0.1 - pres_tissue_He[ci];
        if (temp3 >= 0.0)
            temp3 = 0.0;
        else
            temp3 = - temp3 / pres_tissue_He[ci];

        if( 0.0 < temp3 && temp3 < 1.0 )
        {
            temp3 = log(1.0 - temp3) / -0.6931; // temp1 is the multiples of half times necessary.
                                                // 0.6931 is ln(2), because the math function log() calculates with a base of e  not 2 as requested.
                                                // minus because log is negative
            temp4 = var_He_ht * temp3 / float_desaturation_multiplier; // time necessary (in minutes ) for "complete" desaturation, new in v.101 float_desaturation_multiplier
        }
        else
        {
            temp3 = 0.0;
            temp4 = 0.0;
        }

        // saturation_time (for flight)
        if (temp4 > temp2)
            desat_time = (unsigned short)temp4;
        else
            desat_time = (unsigned short)temp2;

        if(desat_time > int_O_desaturation_time)
            int_O_desaturation_time = desat_time;

        // N2 saturation in multiples of halftime for display purposes
        temp2 = temp1 * 20.0;   // 0 = 1/8, 120 = 0, 249 = 8
        temp2 = temp2 + 80.0;   // set center
        if (temp2 < 0.0)
            temp2 = 0.0;
        if (temp2 > 255.0)
            temp2 = 255.0;
        char_O_tissue_N2_saturation[ci] = (char)temp2;

        // He saturation in multiples of halftime for display purposes
        temp4 = temp3 * 20.0;   // 0 = 1/8, 120 = 0, 249 = 8
        temp4 = temp4 + 80.0;   // set center
        if (temp4 < 0.0)
            temp4 = 0.0;
        if (temp4 > 255.0)
            temp4 = 255.0;
        char_O_tissue_He_saturation[ci] = (char)temp4;
    } // for
}

//////////////////////////////////////////////////////////////////////////////
// calc_wo_deco_step_1_min
//
// FIXED N2 Ratio
// optimized in v.101 (...saturation_multiplier)
// desaturation slowed down to 70,42%
//
static void calc_wo_deco_step_1_min(void)
{
    assert( 800 < int_I_pres_surface && int_I_pres_surface < 1100 );
    assert( 800 < int_I_pres_respiration && int_I_pres_respiration < 1100 );
    assert( 100 <= char_I_saturation_multiplier && char_I_saturation_multiplier < 200 );
    assert( 0 < char_I_desaturation_multiplier && char_I_desaturation_multiplier <= 100 );

    N2_ratio = 0.7902; // FIXED, sum lt. buehlmann
    pres_respiration = pres_surface = int_I_pres_surface * 0.001;
    ppN2 = N2_ratio * (pres_respiration - ppWater);
    ppHe = 0.0;
    float_desaturation_multiplier = char_I_desaturation_multiplier * (0.01 * SURFACE_DESAT_FACTOR);
    float_saturation_multiplier   = char_I_saturation_multiplier   * 0.01;

    calc_tissue(1);  // update the pressure in the tissues N2/He in accordance with the new ambient pressure

    clear_deco_table();
    char_O_deco_status = 3;     // surface new in v.102 : stays in surface state.
    char_O_nullzeit = 0;
    int_O_ascenttime = 0;
    int_O_extra_ascenttime = 0;
    calc_gradient_factor();
}

//////////////////////////////////////////////////////////////////////////////
// calc_dive_interval
//
// Prepare tissue for delay before the next dive simulation.
//
// Inputs:  char_I_dive_interval == delay before dive (in 10' steps).
// Outputs: pres_tissue_N2/He[], CNS_fraction
//
// Should be protected by deco_push_tissues_to_vault(),
//                        deco_pull_tissues_from_vault()
//
// desaturation slowed down to 70,42%.
//
static void calc_dive_interval(void)
{
    overlay unsigned char t;
    overlay unsigned char backup_model;

    //---- Initialize simulation parameters ----------------------------------
    N2_ratio = 0.7902; // FIXED, sum lt. buehlmann
    pres_respiration = pres_surface = int_I_pres_surface * 0.001;
    ppN2 = N2_ratio * (pres_respiration - ppWater);
    ppHe = 0.0;
    float_desaturation_multiplier = char_I_desaturation_multiplier * (0.01 * SURFACE_DESAT_FACTOR);
    float_saturation_multiplier   = char_I_saturation_multiplier   * 0.01;

    // Make sure SURFACE_DESAT_FACTOR is applied:
    backup_model = char_I_deco_model;
    char_I_deco_model = 0;

    //---- Perform simulation ------------------------------------------------
    for(t=0; t<char_I_dive_interval; ++t)
    {
        calc_tissue(2);  // period = 10min.
        CNS_fraction =  0.92587471 * CNS_fraction;  // Half-time = 90min: (1/2)^(1/9)
    }
    assert( 0.0 <= CNS_fraction && CNS_fraction <= 9.99 ); // 999 %
    int_O_CNS_fraction = (unsigned short)(CNS_fraction * 100.0 + 0.5);

    //---- Restore model -----------------------------------------------------
    char_I_deco_model = backup_model;
}

//////////////////////////////////////////////////////////////////////////////
// deco_clear_CNS_fraction
//
// new in v.101
//
void deco_clear_CNS_fraction(void)
{
    RESET_C_STACK

    CNS_fraction = 0.0;
    int_O_CNS_fraction = 0;
}

//////////////////////////////////////////////////////////////////////////////
// deco_calc_CNS_fraction
//
// Input:  char_I_actual_ppO2   : Current condition (in decibars).
//         char_I_step_is_1min  : use 1min or 10min steps instead of 2sec.
//         CNS_fraction         : velue before period.
// Output: CNS_fraction, int_O_CNS_fraction
//
void deco_calc_CNS_fraction(void)
{
    overlay float time_factor = 1.0f;
    RESET_C_STACK

    assert( 0.0 <= CNS_fraction && CNS_fraction <= 9.99 );
    assert( char_I_actual_ppO2 > 15 );

    if( char_I_step_is_1min == 1 )
        time_factor = 30.0f;
    else if( char_I_step_is_1min == 2  )
        time_factor = 300.0f;
    //------------------------------------------------------------------------
    // Don't increase CNS below 0.5 bar, but keep it steady.
    if (char_I_actual_ppO2 < 50)
        ;   // no changes
    //------------------------------------------------------------------------
    // Below (and including) 1.60 bar
    else if (char_I_actual_ppO2 < 61)
        CNS_fraction += time_factor/(-533.07 * char_I_actual_ppO2 + 54000.0);
    else if (char_I_actual_ppO2 < 71)
        CNS_fraction += time_factor/(-444.22 * char_I_actual_ppO2 + 48600.0);
    else if (char_I_actual_ppO2 < 81)
        CNS_fraction += time_factor/(-355.38 * char_I_actual_ppO2 + 42300.0);
    else if (char_I_actual_ppO2 < 91)
        CNS_fraction += time_factor/(-266.53 * char_I_actual_ppO2 + 35100.0);
    else if (char_I_actual_ppO2 < 111)
        CNS_fraction += time_factor/(-177.69 * char_I_actual_ppO2 + 27000.0);
    else if (char_I_actual_ppO2 < 152)
        CNS_fraction += time_factor/( -88.84 * char_I_actual_ppO2 + 17100.0);
    else if (char_I_actual_ppO2 < 167)
        CNS_fraction += time_factor/(-222.11 * char_I_actual_ppO2 + 37350.0);
    //------------------------------------------------------------------------
    // Arieli et all.(2002): Modeling pulmonary and CNS O2 toxicity:
    // J Appl Physiol 92: 248--256, 2002, doi:10.1152/japplphysiol.00434.2001
    // Formula (A1) based on value for 1.55 and c=20
    // example calculation: Sqrt((1.7/1.55)^20)*0.000404
    else if (char_I_actual_ppO2 < 172)
        CNS_fraction += time_factor*0.00102;
    else if (char_I_actual_ppO2 < 177)
        CNS_fraction += time_factor*0.00136;
    else if (char_I_actual_ppO2 < 182)
        CNS_fraction += time_factor*0.00180;
    else if (char_I_actual_ppO2 < 187)
        CNS_fraction += time_factor*0.00237;
    else if (char_I_actual_ppO2 < 192)
        CNS_fraction += time_factor*0.00310;
    else if (char_I_actual_ppO2 < 198)
        CNS_fraction += time_factor*0.00401;
    else if (char_I_actual_ppO2 < 203)
        CNS_fraction += time_factor*0.00517;
    else if (char_I_actual_ppO2 < 233)
        CNS_fraction += time_factor*0.0209;
    else
        CNS_fraction += time_factor*0.0482; // value for 2.5

    if( CNS_fraction > 9.99)    // Limit display to 999%
        CNS_fraction = 9.99;
    if( CNS_fraction < 0.0 )
        CNS_fraction = 0.0;

    int_O_CNS_fraction = (unsigned short)(100.0 * CNS_fraction + 0.5);
}

//////////////////////////////////////////////////////////////////////////////
// deco_calc_CNS_planning
//
// Compute CNS during predicted ascent.
//
// Note:    Needs a call to deco_push_tissues_to_vault(),
//          deco_pull_tissues_from_vault() to avoid trashing everything...
//
// Input:   CNS_fraction, char_O_deco_time[], char_O_deco_depth[]
// Output:  CNS_fraction, int_O_CNS_fraction
//
void deco_calc_CNS_planning(void)
{
    overlay unsigned char  backup_gas_last_depth;
    overlay unsigned char  backup_gas_last_used;
    overlay unsigned short backup_dive_mins;
    overlay unsigned char  backup_actual_ppO2;

    RESET_C_STACK

    // Backup state machine
    backup_gas_last_depth = sim_gas_last_depth;
    backup_gas_last_used  = sim_gas_last_used;
    backup_dive_mins      = sim_dive_mins;
    backup_actual_ppO2    = char_I_actual_ppO2;

    // Uses 1min CNS period:
    char_I_step_is_1min = 1;

    //---- Retrieve bottom Gas used, and set variables.
    sim_gas_last_used  = char_I_first_gas;
    sim_gas_last_depth = 0;             // Surface gas marker.
    gas_switch_set();                   // Sets initial calc_N2/He_ratio

    //---- CCR mode : do the full TTS at once --------------------------------
    if( char_I_const_ppO2 != 0 )
    {
        overlay unsigned short t;       // Needs 16bits here !
        char_I_actual_ppO2 = char_I_const_ppO2;
        for(t=0; t<int_O_ascenttime; ++t)
            deco_calc_CNS_fraction();
    }
    else //---- OC mode : have to follow all gas switches... -----------------
    {
        overlay unsigned char i = 0;    // Decostop loop counter
        overlay float actual_ppO2;
        overlay unsigned char time, t;

        //---- Ascent to surface delay
        // NOTE: count as if time is spent with bottom pressure,
        //       AND the bottom gas
        actual_ppO2 = (pres_surface + char_I_bottom_depth * METER_TO_BAR)
                    * (1.0 - calc_N2_ratio - calc_He_ratio);
        if( actual_ppO2 < 0.0  ) actual_ppO2 = 0.0;
        if( actual_ppO2 > 2.50 ) actual_ppO2 = 2.55;
        char_I_actual_ppO2 = (unsigned char)(100.0 * actual_ppO2 + 0.5);

        // Ascent time (rounded up):
        time = (unsigned char)(0.1 * char_I_bottom_depth + 0.5);

        for(t=0; t<time; ++t)
        {
            deco_calc_CNS_fraction();
            sim_dive_mins++;
        }

        //---- Do all further stops ------------------------------------------
        for(i=0; i<NUM_STOPS; ++i)
        {
            overlay unsigned char stop_gas;

            //---- Get next stop ---------------------------------------------
            {
                time             = char_O_deco_time[(NUM_STOPS-1)-i];
                temp_depth_limit = char_O_deco_depth[(NUM_STOPS-1)-i];
                stop_gas         = char_O_deco_gas[(NUM_STOPS-1)-i];
            }
            if( time == 0 ) continue;

            //---- Gas Switch ? ----------------------------------------------
            if( stop_gas != sim_gas_last_used )
            {
                sim_gas_last_depth = deco_gas_change[stop_gas-1];
                sim_gas_last_used  = stop_gas;
                gas_switch_set();
            }

            //---- Convert Depth and N2_ratio to ppO2 ------------------------
            actual_ppO2 = (pres_surface + temp_depth_limit * METER_TO_BAR)
                        * (1.0 - calc_N2_ratio - calc_He_ratio);
            if( actual_ppO2 < 0.0  ) actual_ppO2 = 0.0;
            if( actual_ppO2 > 2.50 ) actual_ppO2 = 2.55;
            char_I_actual_ppO2 = (unsigned char)(100.0 * actual_ppO2 + 0.5);

            //---- Apply the stop
            for(t=0; t<time; ++t)
            {
                deco_calc_CNS_fraction();
                sim_dive_mins++;
            }
        }
    }

    //---- Back to normal mode... --------------------------------------------
    char_I_step_is_1min = 0;
    sim_gas_last_depth  = backup_gas_last_depth;
    sim_gas_last_used   = backup_gas_last_used;
    sim_dive_mins       = backup_dive_mins;
    char_I_actual_ppO2  = backup_actual_ppO2;
}

//////////////////////////////////////////////////////////////////////////////
// deco_calc_CNS_decrease_15min
//
// new in v.101
//
// calculates the half time of 90 minutes in 6 steps of 15 min
// (Used in sleepmode, for low battery mode).
//
// Output: int_O_CNS_fraction
// Uses and Updates: CNS_fraction
//
void deco_calc_CNS_decrease_15min(void)
{
    RESET_C_STACK
    assert( 0.0 <= CNS_fraction && CNS_fraction <= 9.99 );

    CNS_fraction =  0.890899 * CNS_fraction;
    int_O_CNS_fraction = (unsigned short)(CNS_fraction * 100.0 + 0.5);
}

//////////////////////////////////////////////////////////////////////////////
// deco_calc_percentage
//
// new in v.101
//
// calculates int_I_temp * char_I_temp / 100
// output is int_I_temp
//
// Used to compute NoFly remaining time.
//
void deco_calc_percentage(void)
{
    RESET_C_STACK

    assert( 60 <= char_I_temp && char_I_temp <= 100 );
    assert( int_I_temp  < 5760 );      // Less than 4 days = 96h...

    int_I_temp = (unsigned short)(((float)int_I_temp * (float)char_I_temp) * 0.01 );

    assert( int_I_temp < 5760 );                            // Less than 96h too...
}

//////////////////////////////////////////////////////////////////////////////
// deco_gas_volumes
//
// new in v.111
//
// calculates volumes for each gas.
//
// Input:   char_I_bottom_depth, char_I_bottom_time for planned dive.
//          Gas list.
//          char_I_first_gas is the bottom gas.
//          decoplan (char_O_deco_depth, char_O_deco_time).
//          char_I_bottom_usage is bottom liters/minutes (5 .. 50) or bar/min.
//          char_I_deco_usage is deco liters/minutes (5 .. 50) or bar/min.
// Output:  int_O_gas_volumes[0..4] in litters * 0.1
//
void deco_gas_volumes(void)
{
    overlay float volumes[NUM_GAS];
    overlay float bottom_usage, deco_usage;
    overlay unsigned char i;
    overlay unsigned char gas, depth;
    RESET_C_STACK

    //---- initialize --------------------------------------------------------
    for(i=0; i<NUM_GAS; ++i)                            // Nothing yet...
        volumes[i] = 0.0;

    bottom_usage = char_I_bottom_usage;      // In liter/minutes.
    deco_usage   = char_I_deco_usage;        // In liter/minutes.

    // Early return if not defined:
    if( deco_usage <= 0.0 || bottom_usage <= 0.0 )
        goto done;

    //---- Bottom usage -----------------------------------------------------
    assert(1 <= char_I_first_gas && char_I_first_gas <= NUM_GAS);
    gas = char_I_first_gas - 1;

    if( char_I_const_ppO2 == 0 )
        volumes[gas]
            = (char_I_bottom_depth*0.1 + 1.0)           // Use Psurface = 1.0 bar.
            * char_I_bottom_time                        // in minutes.
            * bottom_usage;                             // In liter/minutes.

    //---- Ascent usage ------------------------------------------------------
    depth = char_I_bottom_depth;

    for(i=0; i<NUM_STOPS; ++i)
    {
        overlay unsigned char newDepth, time, newGas;

        time = char_O_deco_time[i];
        if( time == 0 ) break;       // End of stops.

        newDepth = char_O_deco_depth[i];
        newGas   = char_O_deco_gas  [i]-1;

        assert(0 < newDepth && newDepth <= depth);
        assert(0 <= newGas && newGas < NUM_GAS);

        //---- usage BEFORE gas switch (if any), at 10m/min:
        volumes[gas] += ((depth+newDepth)*0.05 + 1.0)    // average depth --> bar.
                      * (depth-newDepth)*0.1             // metre --> min
                      * deco_usage;

        //---- Do gas switch, at new depth:
        gas   = newGas;
        depth = newDepth;

        //---- Usage at stop:
        volumes[gas] += (depth*0.1 + 1.0)   // depth --> bar.
                      * time                // in minutes.
                      * deco_usage;         // in xxx / min @ 1bar.
    }

    // From last stop to surface
    volumes[gas] += (depth*0.05 + 1.0)      // avg depth --> bar.
                  * depth * 0.1             // time to surface, in minutes.
                  * deco_usage;             // in xxx / min @ 1bar.

    //---- convert results for the ASM interface -----------------------------
done:
    for(i=0; i<NUM_GAS; ++i)
        if( volumes[i] > 65534.0 )
            int_O_gas_volumes[i] = 65535;
        else
            int_O_gas_volumes[i] = (unsigned short)(volumes[i] + 0.5);
}

//////////////////////////////////////////////////////////////////////////////

void deco_push_tissues_to_vault(void)
{
    overlay unsigned char x;
    RESET_C_STACK

    cns_vault = CNS_fraction;
    low_depth_vault = low_depth;

    for (x=0;x<NUM_COMP;x++)
    {
        pres_tissue_N2_vault[x] = pres_tissue_N2[x];
        pres_tissue_He_vault[x] = pres_tissue_He[x];
    }
}

void deco_pull_tissues_from_vault(void)
{
    overlay unsigned char x;
    RESET_C_STACK

    for (x=0; x<NUM_COMP; x++)
    {
        pres_tissue_N2[x] = pres_tissue_N2_vault[x];
        pres_tissue_He[x] = pres_tissue_He_vault[x];
    }

    // Restore both CNS variable, too.
    CNS_fraction = cns_vault;
    int_O_CNS_fraction = (unsigned short)(CNS_fraction * 100.0 + 0.5);

    // GF history too:
    low_depth = low_depth_vault;
    locked_GF_step = GF_delta / low_depth;
}

//////////////////////////////////////////////////////////////////////////////
//
#ifndef CROSS_COMPILE
void main() {}
#endif
