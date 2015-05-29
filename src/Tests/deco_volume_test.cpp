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
    DEFINE_GAS(4, 21, 0,  0, 0);    // Gas#2 : air @ 10m DISABLED
    DEFINE_GAS(5, 21, 0,  0, 0);    // Gas#2 : air @ 40m DISABLED
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

    char_I_bottom_depth = depth;
    char_I_bottom_time  = bottom;
}

//////////////////////////////////////////////////////////////////////////////
/// \brief Gas consumption at a fixed depth
static float fixed(int rmv, int time, int depth) {
    return rmv * time * (1 + 0.1f*depth);
}

TEST(gas_volume, fixed)
{
    EXPECT_EQ(20*30*1, fixed(20,30, 0));    // 30' @  0m
    EXPECT_EQ(20*30*5, fixed(20,30,40));    // 30' @ 40m
}

//////////////////////////////////////////////////////////////////////////////
/// \brief Gas consumption during an ascent at 10m/min.
static float ascent(int rmv, int oldDepth, int newDepth)
{
    return rmv
         * abs(oldDepth-newDepth)*0.1f          // Ascent time
         * (1 + 0.05f*(oldDepth + newDepth));   // Avg pressure.
}

TEST(gas_volume, ascent)
{
    EXPECT_EQ(0,          ascent(20, 30, 30));  // 30m -> 30m : no time, no conso
    EXPECT_EQ(20*4*(1+2), ascent(20, 40,  0));  // 40m ->  0m : 4min, avg 20m
    EXPECT_EQ(20*4*(1+2), ascent(20,  0, 40));  // 0m  -> 40m : 4min, avg 20m
}

//////////////////////////////////////////////////////////////////////////////

TEST(gas_volume, 30min40m_no_stops)
{
    setup_dive(30, 40);     // 30' @ 40m
    for(int s=0; s<32; ++s)
        char_O_deco_time[s] = 0;

    ASSERT_NO_THROW( deco_gas_volumes() );
    EXPECT_EQ(fixed(20,30,40) + ascent(20,40,0),
              int_O_gas_volumes[0]);
    EXPECT_EQ(0,  int_O_gas_volumes[1]);
    EXPECT_EQ(0,  int_O_gas_volumes[2]);
    EXPECT_EQ(0,  int_O_gas_volumes[3]);
    EXPECT_EQ(0,  int_O_gas_volumes[4]);
}

//////////////////////////////////////////////////////////////////////////////

TEST(gas_volume, 30min40m_1min_1min_3min_12min)
{
    setup_dive(30, 40);     // 30' @ 40m

    ASSERT_NO_THROW( deco_gas_volumes() );
    EXPECT_NEAR(fixed(20,30,40) + ascent(20,40,12)
              + fixed(20, 1,12) + ascent(20,12,9),
              int_O_gas_volumes[0], 1);
    EXPECT_EQ(0, int_O_gas_volumes[1]);
    EXPECT_NEAR(fixed(20, 1,9) + ascent(20,9,6)
              + fixed(20, 3,6) + ascent(20,6,3)
              + fixed(20,12,3) + ascent(20,3,0),
              int_O_gas_volumes[2], 1);
    EXPECT_EQ(0, int_O_gas_volumes[3]);
    EXPECT_EQ(0, int_O_gas_volumes[4]);
}
