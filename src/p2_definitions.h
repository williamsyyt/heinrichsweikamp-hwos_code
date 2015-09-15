// *********************************************************
// ** Common definitions for the OSTC decompression code  **
// *********************************************************

//////////////////////////////////////////////////////////////////////////////
// OSTC - diving computer code
// Copyright (C) 2008 HeinrichsWeikamp GbR
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
// history:
// 2010-12-25 v110: [jDG] split in three files (deco.c, main.c, definitions.h)

#define	MBAR_REACH_GASCHANGE_AUTO_CHANGE_OFF	150

// *************************
// ** P R O T O T Y P E S **
// *************************

extern void calc_percentage(void);
extern void deco_calc_hauptroutine(void);
extern void deco_clear_tissue(void);
extern void deco_calc_percentage(void);
extern void deco_calc_wo_deco_step_1_min(void);
extern void deco_calc_dive_interval(void);
extern void deco_gradient_array(void);
extern void deco_calc_desaturation_time(void);
extern void deco_calc_CNS_fraction(void);
extern void deco_calc_CNS_planning(void);
extern void deco_calc_CNS_decrease_15min(void);
extern void deco_clear_CNS_fraction(void);
extern void deco_push_tissues_to_vault(void);
extern void deco_pull_tissues_from_vault(void);
extern void deco_gas_volumes(void);

// ***********************************************
// **         Allow compile on VisualC          **
// ***********************************************

#if defined(WIN32) || defined(UNIX)
    // Some keywords just dont exists on Visual C++:
#   define CROSS_COMPILE
#   define __18CXX
#   define ram
#   define rom
#   define overlay
#   define PARAMETER

#   include <assert.h>
#else
#   define PARAMETER static
#   ifdef __DEBUG
#       define assert(predicate) if( !(predicate) ) assert_failed(__LINE__)
#   else
#       define assert(predicate)
#   endif
#endif

//////////////////////////////////////////////////////////////////////////////
