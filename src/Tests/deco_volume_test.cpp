//////////////////////////////////////////////////////////////////////////////
/// deco_volume_test.cpp
/// Unit test for gas consumption c code.
/// Copyright (c) 2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  HISTORY
// 2015-05-27 jDG: Creation for gas volum re-introduction in hwOS 1.80

extern "C" {
#   include "p2_deco.c"
}

#include <gtest/gtest.h>

//////////////////////////////////////////////////////////////////////////////
/// \brief Defines a default OC gas list
static void setup_gas()
{
    char_I_first_gas = 1;
    char_I_const_ppO2 = 0;  // Default to OC mode

#define DEFINE_GAS(gas, o2, he, depth, role)    \
    char_I_deco_N2_ratio  [gas-1] = 100 - o2 - he;  \
    char_I_deco_He_ratio  [gas-1] = he;             \
    char_I_deco_gas_change[gas-1] = depth;

    DEFINE_GAS(1, 21,  0, 0, 1);    // Gas#1 : Air       FIRST
    DEFINE_GAS(2, 18, 30, 0, 2);    // Gas#2 : Tx18/30   TRAVEL
    DEFINE_GAS(3, 80,  0, 9, 3);    // Gas#3 : Nx80 @ 9m DECO
    DEFINE_GAS(4, 21, 0, 10, 0);    // Gas#2 : air @ 10m DISABLED
    DEFINE_GAS(5, 21, 0, 40, 0);    // Gas#2 : air @ 40m DISABLED
}

//////////////////////////////////////////////////////////////////////////////
/// \brief Define a default deco plan.
static void setup_plan()
{
    // 1 min at 12m
    char_O_deco_time [0] = 1;
    char_O_deco_depth[0] =12;
    char_O_deco_gas  [0] = 1; // Gas#1
    // 1 min at 9m
    char_O_deco_time [1] = 1;
    char_O_deco_depth[1] = 9;
    char_O_deco_gas  [1] = 3; // Gas#3
    // 3min at 6m
    char_O_deco_time [2] = 3;
    char_O_deco_depth[2] = 6;
    char_O_deco_gas  [2] = 3; // Gas#3
    // 12 min at 3m
    char_O_deco_time [3] =12;
    char_O_deco_depth[3] = 3;
    char_O_deco_gas  [3] = 3; // Gas#3
    // Done
    for(int s=4; s<NUM_STOPS; ++s) {
        char_O_deco_time [s] = 0;
        char_O_deco_depth[s] = 0;
        char_O_deco_gas  [s] = 0;
    }
}

static void setup_dive(int bottom, int depth)
{
    setup_gas();
    setup_plan();

    char_I_bottom_depth = bottom;
    char_I_bottom_time  = depth;
}

//////////////////////////////////////////////////////////////////////////////

TEST(gas_volume, run)
{
    setup_dive(30, 40);     // 30' @ 40m

    EXPECT_NO_THROW( deco_gas_volumes() );
}
