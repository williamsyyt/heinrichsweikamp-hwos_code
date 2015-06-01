//////////////////////////////////////////////////////////////////////////////
/// compass_test.cpp
/// Unit test for compass calibration.
/// Copyright (c) 2012-2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  HISTORY
// 2015-05-23 jDG: Rewrite compass testing, to allow reducing code size.

extern "C" {
#   include "compass.h"
}

#include <gtest/gtest.h>

#include <math.h>
#include <iostream>

//////////////////////////////////////////////////////////////////////////////

inline float uniform() {
    return (rand() & 0xFFFF) / 65536.0f;
}
inline float sqr(float x) {
    return x*x;
}

static float radius = 0.21f;
static float cx = 0, cy = 0, cz = 0;

//////////////////////////////////////////////////////////////////////////////

static void check_calib()
{
    compass_reset_calibration();

    //---- Generates random points on a sphere -------------------------------
    // of radius,center (cx, cy, cz):
    for(int i=0; i<100; ++i)
    {
        float theta = uniform()*360.0f;
        float phi   = uniform()*180.0f - 90.0f;

        float x = cx + radius * cosf(phi)*cosf(theta);
        float y = cy + radius * cosf(phi)*sinf(theta);
        float z = cz + radius * sinf(phi);

        compass_DX_f = short(32768 * x);
        compass_DY_f = short(32768 * y);
        compass_DZ_f = short(32768 * z);
        compass_add_calibration();
    }

    compass_solve_calibration();
    float r2 = sqr(compass_CX_f/32768.0f - cx)
             + sqr(compass_CY_f/32768.0f - cy)
             + sqr(compass_CZ_f/32768.0f - cz);

    // Calibration error less than 2 bits:
    EXPECT_NEAR(0, sqrtf(r2), 4.0f/32768.0f)
        << "Center at (" << compass_CX_f/32768.0f << ", "
                         << compass_CY_f/32768.0f << ", "
                         << compass_CZ_f/32768.0f << ")."
        << " Error = " << sqrtf(r2);
}

//////////////////////////////////////////////////////////////////////////////

TEST(compass, calibration_centered)
{
    compass_CX_f = compass_CY_f = compass_CZ_f = 0;

    // Half-unit, centered, sphere:
    radius = 0.5f;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
        check_calib();
}

//////////////////////////////////////////////////////////////////////////////

TEST(compass, calibration_near_centered)
{
    // Put magnetic center elsewhere, but keep position+radius < 1.0, to
    // avoid Q15 overflow...
    radius = 0.21f;
    cx = 0.019f, cy = -0.026f, cz = 0.004f;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
        check_calib();
}

//////////////////////////////////////////////////////////////////////////////

TEST(compass, calibration_far_centered)
{
    // Put magnetic center elsewhere, but keep position+radius < 1.0, to
    // avoid Q15 overflow...
    radius = 0.21f;
    cx = -0.79f, cy = 0.79f, cz = 0.79f;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
        check_calib();
}

//////////////////////////////////////////////////////////////////////////////

TEST(compass, calibration_small_centered_signal)
{
    // use a very very small magnetic signal, centered:
    radius = 0.001f;
    cx = 0.000f, cy = 0.000f, cz = 0.000f;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
        check_calib();
}

//////////////////////////////////////////////////////////////////////////////

TEST(compass, calibration_small_off_centered_signal)
{
    // Have a rather small sphere radius (20%), off-centered buy 80%
    radius = 0.200f;
    cx = 0.800f, cy = -0.800f, cz = 0.800f;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
        check_calib();
}
