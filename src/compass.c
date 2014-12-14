//////////////////////////////////////////////////////////////////////////////
// HISTORY
//  2012-12-01  [jDG] Creation
//  2012-12-23  [jDG] Added filtering.
//  2012-12-30  [jDG] Added calibration (spherical best fit).

#include "compass.h"

//////////////////////////////////////////////////////////////////////////////
// mH: Crude work-around, needs to be made right
#ifndef UNIX
#   pragma udata overlay bank8=0x800
    static char	  C_STACK[256];  // Overlay C-code data stack here.
#       define RESET_C_STACK    \
        _asm                    \
            LFSR    1, 0x800    \
            LFSR    2, 0x800    \
        _endasm
#   pragma udata overlay bank9_compass
#else
#       define RESET_C_STACK
#endif

//////////////////////////////////////////////////////////////////////////////
// fifth order of polynomial approximation of atan(), giving 0.05 deg max error
//
#define K1  (5701)  // Needs K1/2**16
#define K2  (1645)  // Needs K2/2**48  WAS NEGATIV
#define K3  ( 446)  // Needs K3/2**80

//////////////////////////////////////////////////////////////////////////////
// Interface to assembleur multiplies
Int16 umul(PARAMETER Int16 a, PARAMETER Int16 b)
{
    extern Int16 compass_umul(void);
    extern Int16 compass_a, compass_b;
    compass_a = a;
    compass_b = b;
    return compass_umul();
}

Int16 imul(PARAMETER Int16 a, PARAMETER Int16 b)
{
    extern Int16 compass_imul(void);
    extern Int16 compass_a, compass_b;
    compass_a = a;
    compass_b = b;
    return compass_imul();
}

//////////////////////////////////////////////////////////////////////////////
/// Returns a / b * 2**16
///
/// A 16/16 -> 16 bits divide, returning a scalled result.
/// Used to multiply fractional numbers in the range 0..1,
/// represented as 0..32767.
Int16 udiv(PARAMETER Int16 a, PARAMETER Int16 b)
{
    OVERLAY Int16 d, r;

    //---- Pre-scale both numerator and denominator --------------------------
    while( (((a>>8) | (b>>8)) & 0xC0) == 0 )
    {
        a <<= 1;
        b <<= 1;
    }

    //---- Make division trials ----------------------------------------------
    d = 0x4000;             // Starts with 0.5, because 1.0 is sign bit.
    b >>= 1;                // Hence pre-shift b.
    r = 0;
    do {
        if( a >= b ) {      // a is big enough ?
            a -= b;         // then count d times b out of it.
            r |= d;         // and accumulate that bit.
        }
        b >>= 1;            // then loop trying twice smaller.
        d >>= 1;
    } while( b );
    return r;
}

//////////////////////////////////////////////////////////////////////////////
/// Computes atan(y/x) in Angle, for x, y in range 0..32767
///
/// Results a single quadrant Angle, in the range 0 .. Q_PI/2
Angle utan(PARAMETER Int16 y, PARAMETER Int16 x)
{
    OVERLAY Int16 ratio, angle, x2, x3;

    //---- Handle zero divisor -----------------------------------------------
    if( x == 0 )
        return (y == 0) ? 0 : Q_PIO2;

    //---- Make it half-quadrant : 0 .. 45 deg -------------------------------
    ratio = (x > y) ? udiv(y, x) : udiv(x, y);

    //---- Then apply the polynomial approximation ---------------------------
    angle = umul(K1, ratio);            // r*K1 / 2**16
    x2 = umul(ratio, ratio);            // r**2 / 2**16
    x3 = umul(x2, ratio);               // r**3 / 2**32
    angle -= umul(x3, K2);              // K2*r**3 / 2**48: NEGATIV.

    x3 = umul(x3, x2);                  // r**5 / 2**64
    angle += umul(x3, K3);              // K3*r**5 / 2**80

    //---- Recover the full quadrant -----------------------------------------
    return (x < y) ? (Angle)(Q_PIO2 - angle)
                   : (Angle)(angle);
}

//////////////////////////////////////////////////////////////////////////////
/// Computes atan2(y/x) in Angle, for x, y in range -32768 to 32767
///
/// Results a four quadrant Angle, in the range -Q_PI .. +Q_PI
Angle itan(PARAMETER Int16 y, PARAMETER Int16 x)
{
    // Beware: -32768 is not properly handled (sgn error).
    if( x == -32768 ) x = -32767;
    if( y == -32768 ) y = -32767;

    if( x >= 0 )
        if( y >= 0 )    // First quadrant: 0..90 deg.
            return utan(y,x);
        else            // Fourth quadrant: 0..-90 deg
            return -utan(-y,x);
    else
        if( y >= 0 )    // Second quadrant: 90..180 deg
            return Q_PI - utan(y, -x);
        else            // Third quadrant: -90..-180 deg;
            return -Q_PI + utan(-y, -x);
}

//////////////////////////////////////////////////////////////////////////////
/// Computes cos(theta) = sqrtf(x2/h2),
/// when theta = atan(y/x) and h2=x*x+y*y
///
Int16 cosxh(PARAMETER Int16 x2, PARAMETER Int16 h2)
{
    OVERLAY Int16 r = 0;
    OVERLAY Int16 d = 0x4000;

    do {
        OVERLAY Int16 a = r + d;
        a = umul(a, a);
        a = umul(a, h2);
        if( a <= x2 ) r += d;
        d >>= 1;
    } while( d );

    return r;
}

//////////////////////////////////////////////////////////////////////////////
/// Computes both sin and cos of angle y/x,
/// with h = sqrt(x**2+y**2).
///
void sincos(PARAMETER Int16 x, PARAMETER Int16 y, Int16* sin, Int16* cos)
{
    OVERLAY Int16 x2, y2, h2;

    //---- Fold into one quadant ---------------------------------------------
    OVERLAY char neg = 0;
    if( x < 0 )
    {
        neg |= 1;
        x = -x;
    }
    if( y < 0 )
    {
        neg |= 2;
        y = -y;
    }

    //---- Pre-scale both numerator and denominator ----------------------
    while( (((x>>8) | (y>>8)) & 0xE0) == 0 )
    {
        x <<= 1;
        y <<= 1;
    }

    //---- Uses trig() to do the stuff one on quadrant -------------------
    x2 = umul(x,x);
    y2 = umul(y,y);
    h2 = x2 + y2;
    x2 = cosxh(x2, h2);

    //---- Results back in four quadrants --------------------------------
    *cos = (neg & 1) ? -x2 : x2;
    y2 = cosxh(y2, h2);
    *sin = (neg & 2) ? -y2 : y2;
}

//////////////////////////////////////////////////////////////////////////////
//

void compass(void)
{
    OVERLAY Int16 sin, cos;
    OVERLAY Int16 iBfx, iBfy, Gz;
    OVERLAY Int16 iBpx, iBpy, iBpz;
    RESET_C_STACK;

    //---- Make hard iron correction -----------------------------------------
    // Measured magnetometer orientation, measured ok.
    // From matthias drawing: (X,Y,Z) --> (X,Y,Z) : no rotation.
    iBpx = compass_DX_f - compass_CX_f; // X
    iBpy = compass_DY_f - compass_CY_f; // Y
    iBpz = compass_DZ_f - compass_CZ_f; // Z

    //---- Calculate sine and cosine of roll angle Phi -----------------------
    sincos(accel_DZ_f, accel_DY_f, &sin, &cos);
//    compass_roll = itan(sin, cos) / 100;

    //---- rotate by roll angle (-Phi) ---------------------------------------
    iBfy = imul(iBpy, cos) - imul(iBpz, sin);
    iBpz = imul(iBpy, sin) + imul(iBpz, cos);
    Gz = imul(accel_DY_f, sin) + imul(accel_DZ_f, cos);

    //---- calculate sin and cosine of pitch angle Theta ---------------------
    sincos(Gz, -accel_DX_f, &sin, &cos);     // NOTE: changed sin sign.
//    compass_pitch = itan(sin, cos) / 100;

    /* correct cosine if pitch not in range -90 to 90 degrees */
    if( cos < 0 ) cos = -cos;

    ///---- de-rotate by pitch angle Theta -----------------------------------
    iBfx = imul(iBpx, cos) + imul(iBpz, sin);

    //---- Detect uncalibrated compass ---------------------------------------
    if( !compass_CX_f && !compass_CY_f && !compass_CZ_f )
    {
        compass_heading = -1;
        return;
    }

    //---- calculate current yaw = e-compass angle Psi -----------------------
    // Result in degree (no need of 0.01 deg precision...
    compass_heading = itan(-iBfy, iBfx) / 100;

    // Result in 0..360 range:
    if( compass_heading < 0 )
        compass_heading += 360;
}
