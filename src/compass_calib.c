#include "compass.h"

static unsigned short int compass_N;

static float Su, Sv, Sw;
static float Suu, Svv, Sww, Suv, Suw, Svw;
static float Suuu, Svvv, Swww;
static float Suuv, Suuw, Svvu, Svvw, Swwu, Swwv;

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

void compass_reset_calibration()
{
    RESET_C_STACK;

    compass_N = 0;
    Su = Sv = Sw = 0.0;
    Suu = Svv = Sww = Suv = Suw = Svw = 0.0;
    Suuu = Svvv = Swww = 0.0;
    Suuv = Suuw = Svvu = Svvw = Swwu = Swwv = 0.0;
}

void compass_add_calibration()
{
    OVERLAY float u, v, w;
    RESET_C_STACK;

    u = (compass_DX_f - compass_CX_f) / 32768.0f;
    v = (compass_DY_f - compass_CY_f) / 32768.0f;
    w = (compass_DZ_f - compass_CZ_f) / 32768.0f;

    compass_N++;
    Su += u;
    Sv += v;
    Sw += w;
    Suv += u*v;
    Suw += u*w;
    Svw += v*w;
    Suu  += u*u;
    Suuu += u*u*u;
    Suuv += v*u*u;
    Suuw += w*u*u;
    Svv  += v*v;
    Svvv += v*v*v;
    Svvu += u*v*v;
    Svvw += w*v*v;
    Sww  += w*w;
    Swww += w*w*w;
    Swwu += u*w*w;
    Swwv += v*w*w;
}

//////////////////////////////////////////////////////////////////////////////

void compass_solve_calibration()
{
    OVERLAY float yu, yv, yw;
    OVERLAY float delta;
    OVERLAY float uc, vc, wc;
    RESET_C_STACK;

    //---- Normalize partial sums --------------------------------------------
    //
    // u, v, w should be centered on the mean value um, vm, wm:
    // x = u + um, with um = Sx/N
    //
    // So:
    // (u + um)**2 = u**2 + 2u*um + um**2
    // Su = 0, um = Sx/N
    // Sxx = Suu + 2 um Su + N*(Sx/N)**2 = Suu + Sx**2/N
    // Suu = Sxx - Sx**2/N
    yu = Su/compass_N;
    yv = Sv/compass_N;
    yw = Sw/compass_N;

    Suu -= Su*yu;
    Svv -= Sv*yv;
    Sww -= Sw*yw;

    // (u + um)(v + vm) = uv + u vm + v um + um vm
    // Sxy = Suv + N * um vm
    // Suv = Sxy - N * (Sx/N)(Sy/N);
    Suv -= Su*yv;
    Suw -= Su*yw;
    Svw -= Sv*yw;

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
    Suuu -= (3*Suu + Su*yu)*yu;
    Suuv -= (Suu + Su*yu)*yv + 2*Suv*yu;
    Suuw -= (Suu + Su*yu)*yw + 2*Suw*yu;

    Svvu -= (Svv + Sv*yv)*yu + 2*Suv*yv;
    Svvv -= (3*Svv + Sv*yv)*yv;
    Svvw -= (Svv + Sv*yv)*yw + 2*Svw*yv;

    Swwu -= (Sww + Sw*yw)*yu + 2*Suw*yw;
    Swwv -= (Sww + Sw*yw)*yv + 2*Svw*yw;
    Swww -= (3*Sww + Sw*yw)*yw;

    //---- Solve the system --------------------------------------------------
    // uc Suu + vc Suv + wc Suw = (Suuu + Svvu + Swwu) / 2
    // uc Suv + vc Svv + wc Svw = (Suuv + Svvv + Swwv) / 2
    // uc Suw + vc Svw + wc Sww = (Suuw + Svvw + Swww) / 2
    // Note this is symetric, with a positiv diagonal, hence
    // it always have a uniq solution.
    yu = 0.5f * (Suuu + Svvu + Swwu);
    yv = 0.5f * (Suuv + Svvv + Swwv);
    yw = 0.5f * (Suuw + Svvw + Swww);
    delta = Suu * (Svv * Sww - Svw * Svw)
          - Suv * (Suv * Sww - Svw * Suw)
          + Suw * (Suv * Svw - Svv * Suw);

    uc = (yu  * (Svv * Sww - Svw * Svw)
       -  yv  * (Suv * Sww - Svw * Suw)
       +  yw  * (Suv * Svw - Svv * Suw) )/delta;
    vc = (Suu * ( yv * Sww -  yw * Svw)
       -  Suv * ( yu * Sww -  yw * Suw)
       +  Suw * ( yu * Svw -  yv * Suw) )/delta;
    wc = (Suu * (Svv * yw  - Svw * yv )
       -  Suv * (Suv * yw  - Svw * yu )
       +  Suw * (Suv * yv  - Svv * yu ) )/delta;

    // Back to uncentered coordinates:
    // xc = um + uc
    uc = Su/compass_N + compass_CX_f/32768.0f + uc;
    vc = Sv/compass_N + compass_CY_f/32768.0f + vc;
    wc = Sw/compass_N + compass_CZ_f/32768.0f + wc;

    // Then save the new calibrated center:
    compass_CX_f = (short)(32768 * uc);
    compass_CY_f = (short)(32768 * vc);
    compass_CZ_f = (short)(32768 * wc);
}

////////////////////////////// TEST CODE /////////////////////////////////////

#ifdef TEST_COMPASS_CALIBRATION

#include <QtDebug>
#include <stdio.h>

#include <math.h>
#include <stdlib.h>

short compass_DX_f, compass_DY_f, compass_DZ_f;
short compass_CX_f, compass_CY_f, compass_CZ_f;

inline float uniform() {
    return (rand() & 0xFFFF) / 65536.0f;
}
inline float sqr(float x) {
    return x*x;
}

static const float radius = 0.21f;
static const float cx = 0.79f, cy = -0.46f, cz = 0.24f;
// const float cx = 0, cy = 0, cz = 0;

void check_compass_calib()
{

    // Starts with no calibration at all:
    compass_CX_f = compass_CY_f = compass_CZ_f = 0;

    // Try 10 recalibration passes:
    for(int p=0; p<10; ++p)
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
        qDebug() << "Center ="
                 << compass_CX_f/32768.0f
                 << compass_CY_f/32768.0f
                 << compass_CZ_f/32768.0f;

        float r2 = sqr(compass_CX_f/32768.0f - cx)
                 + sqr(compass_CY_f/32768.0f - cy)
                 + sqr(compass_CZ_f/32768.0f - cz);
        if( r2 > 0.01f*0.01f )
            qWarning() << "    calibration error: " << sqrtf(r2);
    }
}
#endif // TEST_COMPASS_CALIBRATION
