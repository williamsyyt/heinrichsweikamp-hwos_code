//////////////////////////////////////////////////////////////////////////////
/// compass_calib.c
/// Calibrate hard-iron for magnetic compass measurements.
/// Copyright (c) 2012-2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  2015-05-22  [jDG] Make a smaller calibration code (15.6 --> 6.7 KB).

#include "compass.h"

//////////////////////////////////////////////////////////////////////////////
// mH: Put compass data into bank 8 (stack) and bank 9 (variables)
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
#   define RESET_C_STACK
#endif

//////////////////////////////////////////////////////////////////////////////

static unsigned short int compass_N;

static float Su, Sv, Sw;                    // First order moments.
static float Suu, Svv, Sww, Suv, Suw, Svw;  // Second order moments.
static float Saa;  // Suu + Svv + Sww
static float Saau; // Suuu + Svvu + Swwu    // Third order moment.
static float Saav; // Suuv + Svvv + Swwv
static float Saaw; // Suuw + Svvw + Swww
static float yu, yv, yw;                    // temp solution vector.
static float uc, vc, wc;                    // temp sphere's center.

//////////////////////////////////////////////////////////////////////////////

void compass_reset_calibration()
{
    RESET_C_STACK;

    compass_N = 0;
    Su = Sv = Sw = 0.0;
    Suu = Svv = Sww = Suv = Suw = Svw = 0.0;
    Saau = Saav = Saaw = 0.0;
    compass_CX_f = compass_CY_f = compass_CZ_f = 0;
}

//////////////////////////////////////////////////////////////////////////////

void compass_add_calibration()
{
    RESET_C_STACK;

    // Get filtered/calibrated magnetic direction:
    yu = (compass_DX_f - compass_CX_f) / 32768.0f;
    yv = (compass_DY_f - compass_CY_f) / 32768.0f;
    yw = (compass_DZ_f - compass_CZ_f) / 32768.0f;

    // Add to all moments:
    compass_N++;

    Su += yu;
    Sv += yv;
    Sw += yw;

    Suu += yu*yu;
    Suv += yu*yv;
    Suw += yu*yw;
    Svv += yv*yv;
    Svw += yv*yw;
    Sww += yw*yw;

    Saa = yu*yu + yv*yv + yw*yw;
    Saau += yu * Saa;
    Saav += yv * Saa;
    Saaw += yw * Saa;
}

//////////////////////////////////////////////////////////////////////////////

static float compass_discriminent(PARAMETER char column)
{
    // Basic symetric matrix:
    OVERLAY float a = Suu, d = Suv, g = Suw;
    OVERLAY float b = Suv, e = Svv, h = Svw;
    OVERLAY float c = Suw, f = Svw, i = Sww;

    // Substitute a column, if asked to:
    if( column==1 ) { a = yu; b = yv; c = yw; }
    if( column==2 ) { d = yu; e = yv; f = yw; }
    if( column==3 ) { g = yu; h = yv; i = yw; }

    // Do the math:
    return   a * (e * i - f * h)
           - b * (d * i - f * g)
           + c * (d * h - e * g);
}

//////////////////////////////////////////////////////////////////////////////

static float compass_dotc(PARAMETER float u, float v, float w)
{
    return u*uc + v*vc + w*wc;
}

//////////////////////////////////////////////////////////////////////////////

void compass_solve_calibration()
{
    OVERLAY float delta;
    RESET_C_STACK;

    //---- Compute center of measured magnetic directions --------------------
    uc = Su/compass_N;
    vc = Sv/compass_N;
    wc = Sw/compass_N;

    //---- Normalize partial sums --------------------------------------------
    //
    // We measured the (u, v, w) values, and need the centered (x, y, z) ones
    // around the sphere center's (uc, vc, wc) as:
    // uc = Su / N;     The mean value
    // x  = u - uc;     The differnce to the mean.
    //
    // So:
    // x**2 = (u - uc)**2 = u**2 - 2u*uc + uc**2
    //
    // We need the Sxx sum of 2nd orders:
    // Sxx = Suu - 2 uc Su + N*uc*(Su/N) = Suu - uc Su
    Suu -= Su*uc;
    Svv -= Sv*vc;
    Sww -= Sw*wc;

    // (u - uc)(v - vc) = uv - u vc - v uc + uc vc
    // Sxy = Suv - Su vc -   Sv uc + N uc vc
    //     = Suv - Su vc - N vc uc + N uc vc
    //     = Suv - Su vc
    Suv -= Su*vc;
    Suw -= Su*wc;
    Svw -= Sv*wc;

    // (u + um)**3 = u**3 + 3 u**2 um + 3 u um**2 + um**3
    // Sxxx = Suuu + 3 um Suu + 3 um**2 Su + N.um**3
    // Su = 0, um = Sx/N:
    // Suuu = Sxxx - 3 Sx*Suu/N - N.(Sx/N)**3
    //      = Sxxx - 3 Sx*Suu/N - Sx**3/N**2

    // (u + um)**2 (v + vm) = (u**2 + 2 u um + um**2)(v + vm)
    // Sxxy = Suuv + vm Suu + 2 um (Suv + vm Su) + um**2 (Sv + N.vm)
    //
    // Su = 0, Sv = 0, vm = Sy/N:
    // Sxxy = Suuv + vm Suu + 2 um Suv + N um**2 vm
    //
    // Suuv = Sxxy - (Sy/N) Suu - 2 (Sx/N) Suv - (Sx/N)**2 Sy
    //      = Sxxy - Suu*Sy/N - 2 Suv*Sx/N - Sx*Sx*Sy/N/N
    //      = Sxxy - (Suu + Sx*Sx/N)*Sy/N - 2 Suv*Sx/N
    Saa = Suu + Svv + Sww;
    yu = Saau - Saa*uc - compass_dotc(Su*uc + 2*Suu, Sv*uc + 2*Suv, Sw*uc + 2*Suw);
    yv = Saav - Saa*vc - compass_dotc(Su*vc + 2*Suv, Sv*vc + 2*Svv, Sw*vc + 2*Svw);
    yw = Saaw - Saa*wc - compass_dotc(Su*wc + 2*Suw, Sv*wc + 2*Svw, Sw*wc + 2*Sww);

    //---- Solve the system --------------------------------------------------
    // uc Suu + vc Suv + wc Suw = (Suuu + Svvu + Swwu) / 2
    // uc Suv + vc Svv + wc Svw = (Suuv + Svvv + Swwv) / 2
    // uc Suw + vc Svw + wc Sww = (Suuw + Svvw + Swww) / 2
    // Note this is symetric, with a positiv diagonal, hence
    // discriminent is always not null.
    delta = 0.5f / compass_discriminent(0);

    // So computed new center, with offsetted values:
    uc += compass_discriminent(1) * delta;
    vc += compass_discriminent(2) * delta;
    wc += compass_discriminent(3) * delta;

    // Add correction due to already applyed calibration:
    compass_CX_f += (short)(32768 * uc);
    compass_CY_f += (short)(32768 * vc);
    compass_CZ_f += (short)(32768 * wc);
}
