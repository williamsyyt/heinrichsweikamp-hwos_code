//////////////////////////////////////////////////////////////////////////////
/// deco_volume_test.cpp
/// Unit test for gas consumption c code.
/// Copyright (c) 2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  HISTORY
// 2015-05-27 jDG: Creation for gas volum re-introduction in hwOS 1.82

extern "C" {
#   include "p2_deco.c"
}

#include <gtest/gtest.h>
#include <iostream>

//////////////////////////////////////////////////////////////////////////////
/// \brief Defines a default OC gas list
static void setup_gas()
{
    char_I_first_gas = 1;

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
static void setup_plan(const char* stops,
                       const char* gas)
{
    int depth = 3 * (stops ? strlen(stops) : 0);

    int s = 0;
    while( depth > 0 && s < NUM_STOPS ) {
        char_O_deco_time [s] = stops[s];
        char_O_deco_depth[s] = depth;
        char_O_deco_gas  [s] = gas ? gas[s] : 1; // Gas#1 by default
        ++s;
        depth -= 3;
    }
    // Done
    for(; s<NUM_STOPS; ++s) {
        char_O_deco_time [s] = 0;
        char_O_deco_depth[s] = 0;
        char_O_deco_gas  [s] = 0;
    }
}

static void setup_dive(int bottom, int depth,
                       const char* stops = 0, const char* gas = 0)
{
    setup_gas();
    setup_plan(stops, gas);

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

static void check_volumes(float G1, const char* L1,
                          float G2, const char* L2,
                          float G3, const char* L3,
                          float G4, const char* L4,
                          float G5, const char* L5)
{
    std::cout << "             " << std::setw(6) << G1 << " = " << L1 << std::endl;
    std::cout << "             " << std::setw(6) << G2 << " = " << L2 << std::endl;
    std::cout << "             " << std::setw(6) << G3 << " = " << L3 << std::endl;
    std::cout << "             " << std::setw(6) << G4 << " = " << L4 << std::endl;
    std::cout << "             " << std::setw(6) << G5 << " = " << L5 << std::endl;

    EXPECT_NEAR(G1, int_O_gas_volumes[0], 1) << L1;
    EXPECT_NEAR(G2, int_O_gas_volumes[1], 1) << L2;
    EXPECT_NEAR(G3, int_O_gas_volumes[2], 1) << L3;
    EXPECT_NEAR(G4, int_O_gas_volumes[3], 1) << L4;
    EXPECT_NEAR(G5, int_O_gas_volumes[4], 1) << L5;
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, OC, 30%/85%
TEST(gas_volume, OC_13min30m)
{
    char_I_const_ppO2 = 0;  // OC
    setup_dive(13, 30);     // 13' @ 30m --> no deco

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(fixed(20,13,30) + ascent(20,30,0), "Gas1: 1190 L",
                  0, "",
                  0, "",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, OC, 30%/85%
TEST(gas_volume, OC_15min30m)
{
    char_I_const_ppO2 = 0;  // OC
    char stops[] = {1, 0};
    char gas[]   = {3, 0};
    setup_dive(15, 30, stops, gas);     // 15' @ 30m --> 1min at 3m

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(fixed(20,15,30) + ascent(20,30,3), "Gas1: 1343 L",
                  0, "",
                  fixed(20, 1, 3) + ascent(20, 3,0), "Gas3: 33",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, OC, 30%/85%
TEST(gas_volume, OC_29min30m)
{
    char_I_const_ppO2 = 0;  // OC
    char stops[] = {1, 1, 2, 4, 0};
    char gas[]   = {1, 3, 3, 3, 0};
    setup_dive(29, 30, stops, gas);     // 29' @ 30m --> 1' 1' 2' 4'

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(fixed(20,29,30) + ascent(20,30,12) +
                  fixed(20, 1,12) + ascent(20,12, 9), "Gas1: 2488 L",
                  0, "",
                  fixed(20, 1, 9) + ascent(20, 9, 6) +
                  fixed(20, 2, 6) + ascent(20, 6, 3) +
                  fixed(20, 4, 3) + ascent(20, 3, 0), "Gas3: 232 L",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, OC, 30%/85%
TEST(gas_volume, OC_15min60m)
{
    char_I_const_ppO2 = 0;  // OC
    char stops[] = {2, 1, 2, 4, 3, 4, 9, 0};
    char gas[]   = {1, 1, 1, 1, 3, 3, 3, 0};
    setup_dive(15, 60, stops, gas);     // 15' @ 60m --> DTR 32'

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(fixed(20,15,60) + ascent(20,60,21) +
                  fixed(20, 2,21) + ascent(20,21,18) +
                  fixed(20, 1,18) + ascent(20,18,15) +
                  fixed(20, 2,15) + ascent(20,15,12) +
                  fixed(20, 4,12) + ascent(20,12, 9), "Gas1: 3010 L",
                  0, "",
                  fixed(20, 3, 9) + ascent(20, 9, 6) +
                  fixed(20, 4, 6) + ascent(20, 6, 3) +
                  fixed(20, 9, 3) + ascent(20, 3, 0), "Gas3: 502 L",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, CCR, 30%/85%
TEST(gas_volume, CCR_13min30m)
{
    char_I_const_ppO2 = 140;// SP 1.4 bar
    setup_dive(13, 30);     // 13' @ 30m --> no deco

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(/*NO BTM CONSO*/ ascent(20,30,0), "Gas1: 1190 L",
                  0, "",
                  0, "",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, CCR, 30%/85%
TEST(gas_volume, CCR_15min30m)
{
    char_I_const_ppO2 = 140;    // SP 1.4 bar
    char stops[] = {1, 0};
    char gas[]   = {3, 0};
    setup_dive(15, 30, stops, gas);     // 15' @ 30m --> 1min at 3m

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(/*NO BTM CONSO*/  ascent(20,30,3), "Gas1: 1343 L",
                  0, "",
                  fixed(20, 1, 3) + ascent(20, 3,0), "Gas3: 33",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, CCR, 30%/85%
TEST(gas_volume, CCR_29min30m)
{
    char_I_const_ppO2 = 140;    // SP 1.4 bar
    char stops[] = {1, 1, 2, 4, 0};
    char gas[]   = {1, 3, 3, 3, 0};
    setup_dive(29, 30, stops, gas);     // 29' @ 30m --> 1' 1' 2' 4'

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(/*NO BTM CONSO*/  ascent(20,30,12) +
                  fixed(20, 1,12) + ascent(20,12, 9), "Gas1: 2488 L",
                  0, "",
                  fixed(20, 1, 9) + ascent(20, 9, 6) +
                  fixed(20, 2, 6) + ascent(20, 6, 3) +
                  fixed(20, 4, 3) + ascent(20, 3, 0), "Gas3: 232 L",
                  0, "",
                  0, "");
}

//////////////////////////////////////////////////////////////////////////////
// v1.82 ZH-L16+GF, CCR, 30%/85%
TEST(gas_volume, CCR_15min60m)
{
    char_I_const_ppO2 = 140;    // SP 1.4 bar
    char stops[] = {2, 1, 2, 4, 3, 4, 9, 0};
    char gas[]   = {1, 1, 1, 1, 3, 3, 3, 0};
    setup_dive(15, 60, stops, gas);     // 15' @ 60m --> DTR 32'

    ASSERT_NO_THROW( deco_gas_volumes() );
    check_volumes(/*NO BTM CONSO*/  ascent(20,60,21) +
                  fixed(20, 2,21) + ascent(20,21,18) +
                  fixed(20, 1,18) + ascent(20,18,15) +
                  fixed(20, 2,15) + ascent(20,15,12) +
                  fixed(20, 4,12) + ascent(20,12, 9), "Gas1: 3010 L",
                  0, "",
                  fixed(20, 3, 9) + ascent(20, 9, 6) +
                  fixed(20, 4, 6) + ascent(20, 6, 3) +
                  fixed(20, 9, 3) + ascent(20, 3, 0), "Gas3: 502 L",
                  0, "",
                  0, "");
}
