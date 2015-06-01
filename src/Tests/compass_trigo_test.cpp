//////////////////////////////////////////////////////////////////////////////
/// compass_trigo_test.cpp
/// Unit test for compass various operations.
/// Copyright (c) 2012-2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  HISTORY
// 2015-05-23 jDG: Rewrite compass testing, to allow reducing code size.

extern "C" {
#   include "compass.h"
}

#include <gtest/gtest.h>

//////////////////////////////////////////////////////////////////////////////
// Fake assembleur fixed point multiplies.
extern "C" Int16 compass_umul(void);
extern "C" Int16 compass_imul(void);
extern "C" Int16 compass_a, compass_b;

// The (filtered) components of the magnetometer sensor:
Int16 compass_DX_f;
Int16 compass_DY_f;
Int16 compass_DZ_f;

// Found soft-iron calibration values, deduced from already filtered values.
Int16 compass_CX_f;
Int16 compass_CY_f;
Int16 compass_CZ_f;

// The (filtered) components of the accelerometer sensor:
Int16 accel_DX_f;
Int16 accel_DY_f;
Int16 accel_DZ_f;

// The compass result value.
Int16 compass_heading;
Int16 compass_roll;
Int16 compass_pitch;

Int16 compass_a, compass_b;
Int16 compass_umul()
{
    unsigned int a = compass_a;
    unsigned int b = compass_b;
    a *= b;
    a >>= 15;
    return (Int16)a;
}

Int16 compass_imul()
{
    int a = compass_a;
    int b = compass_b;
    a *= b;
    a >>= 15;
    return (Int16)a;
}

//////////////////////////////////////////////////////////////////////////////

TEST(ops, multiply) {
    // Check basic sign handling:
    EXPECT_EQ(umul( 8000,  4000), (Int16)0x03D0);   // 8000/2**15 * 4000/2**15 = 0x3D0
    EXPECT_EQ(imul(-8000,  4000), (Int16)0xFC2F);   // -976 = 0xFC2F
    EXPECT_EQ(imul( 8000, -4000), (Int16)0xFC2F);   // -976 = 0xFC2F
    EXPECT_EQ(imul(-8000, -4000), (Int16)0x03D0);   // +976 = 0x3D0
}

TEST(ops, divide) {
    // Check basic divides:
    EXPECT_EQ(udiv(32000, 32001), (Int16)32766);     // 0.99997 ~ 32766
    EXPECT_EQ(udiv( 4000,  8000), (Int16)16384);
    EXPECT_EQ(udiv( 2000,  8000), (Int16) 8192);
    EXPECT_EQ(udiv( 1000,  8000), (Int16) 4096);
    EXPECT_EQ(udiv(  500,  8000), (Int16) 2048);
}

TEST(trigo, atan) {
    // Check angles returned by the SINGLE QUADRANT atan() function:
    EXPECT_EQ(utan(100, 100), (Int16)4501);  // +1
    EXPECT_EQ(utan( 90, 100), (Int16)4195);  // -4
    EXPECT_EQ(utan( 80, 100), (Int16)3864);  // -2
    EXPECT_EQ(utan( 70, 100), (Int16)3500);  // +1
    EXPECT_EQ(utan( 60, 100), (Int16)3099);  // +3
    EXPECT_EQ(utan( 50, 100), (Int16)2658);  // +1
    EXPECT_EQ(utan( 40, 100), (Int16)2179);  // -1
    EXPECT_EQ(utan( 30, 100), (Int16)1667);  // -3
    EXPECT_EQ(utan( 20, 100), (Int16)1127);  // -4
    EXPECT_EQ(utan( 10, 100), (Int16) 569);  // -2
    EXPECT_EQ(utan(  0, 100), (Int16)   0);
}

TEST(trigo, cosx2h2) {
    // Check ONE-OCTANT pseudo-cosinus function
    // Note: cosxh(x**2, x**2+y**2) is computing cos(atan(y/x))
    EXPECT_EQ(cosxh(12769, 13169), (Int16)32268);   // 113,  20 --> 10.0369° --> 32267 +1
    EXPECT_EQ(cosxh(10000, 12500), (Int16)29310);   // 100,  50 --> 26.5650° --> 29309 +1
    EXPECT_EQ(cosxh(10000, 20000), (Int16)23171);   // 100, 100 --> 45.0000° --> 23170 +1
    EXPECT_EQ(cosxh( 2500, 12500), (Int16)14658);   //  50, 100 --> 63.4349° --> 14654 +4
    EXPECT_EQ(cosxh(  400, 13169), (Int16) 5718);   //  20, 113 --> 79.9631° -->  5711 +7
}

TEST(trigo, sinCos) {
    Int16 sin, cos;

    //---- Check sincos() FIRST QUADRANT ---------------------------------
    sincos( 20, 113, &sin, &cos);   // 80°
    EXPECT_EQ(sin, (Int16)32269);   // +2
    EXPECT_EQ(cos, (Int16) 5727);   // +16

    sincos( 50, 100, &sin, &cos);   // 63°
    EXPECT_EQ(sin, (Int16)29311);   // +2
    EXPECT_EQ(cos, (Int16)14660);   // +6

    sincos(100, 100, &sin, &cos);   // 45°
    EXPECT_EQ(sin, (Int16)23173);   // +3
    EXPECT_EQ(cos, (Int16)23173);   // +3

    sincos(100,  50, &sin, &cos);   // 27°
    EXPECT_EQ(sin, (Int16)14660);   // +6
    EXPECT_EQ(cos, (Int16)29311);   // +2

    sincos(113,  20, &sin, &cos);   // 10°
    EXPECT_EQ(sin, (Int16) 5727);   // +16
    EXPECT_EQ(cos, (Int16)32269);   // +2

    //---- Check sincos() OTHER QUADRANTS --------------------------------
    sincos(-20, 113, &sin, &cos);   // 90+80°
    EXPECT_EQ(sin, (Int16) 32269);  // +2
    EXPECT_EQ(cos, (Int16) -5727);  // +16

    sincos(-20,-113, &sin, &cos);   // 180+80°
    EXPECT_EQ(sin, (Int16)-32269);  // +2
    EXPECT_EQ(cos, (Int16) -5727);  // +16

    sincos( 20,-113, &sin, &cos);   // 270+80°
    EXPECT_EQ(sin, (Int16)-32269);  // +2
    EXPECT_EQ(cos, (Int16)  5727);  // +16
}
