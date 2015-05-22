//////////////////////////////////////////////////////////////////////////////
// HISTORY
//  2013-03-29  [jDG] Creation for tests.

//---- Storage classes -------------------------------------------------------
// Declaring PIC18 and VC++/G++ functions.
#ifdef UNIX
#   define PARAMETER
#   define OVERLAY
#else
//---- Bank 9 parameters -----------------------------------------------------
#   pragma udata overlay bank9_compass = 0x900
#   define PARAMETER    static
#   define OVERLAY      overlay
#endif

//////////////////////////////////////////////////////////////////////////////

#define Q_PI    (18000)
#define Q_PIO2  (9000)

typedef short int Int16;
typedef signed char Int8;
typedef Int16 Angle;

// The (filtered) components of the magnetometer sensor:
extern Int16 compass_DX_f;
extern Int16 compass_DY_f;
extern Int16 compass_DZ_f;

// Found soft-iron calibration values, deduced from already filtered values.
extern Int16 compass_CX_f;
extern Int16 compass_CY_f;
extern Int16 compass_CZ_f;

// The (filtered) components of the accelerometer sensor:
extern Int16 accel_DX_f;
extern Int16 accel_DY_f;
extern Int16 accel_DZ_f;

// The compass result value.
extern Int16 compass_heading;
extern Int16 compass_roll;
extern Int16 compass_pitch;

extern Int16 umul(PARAMETER Int16 a, PARAMETER Int16 b);
extern Int16 imul(PARAMETER Int16 a, PARAMETER Int16 b);
extern Int16 udiv(PARAMETER Int16 a, PARAMETER Int16 b);
extern Angle utan(PARAMETER Int16 a, PARAMETER Int16 b);
extern Angle itan(PARAMETER Int16 a, PARAMETER Int16 b);
extern Angle cosxh(PARAMETER Int16 a, PARAMETER Int16 b);
extern void  sincos(PARAMETER Int16 a, PARAMETER Int16 b, Int16* sin, Int16* cos);

//////////////////////////////////////////////////////////////////////////////
// The user functions
extern void compass(void);
extern void compass_reset_calibration(void);
extern void compass_add_calibration(void);

extern void compass_solve_calibration(void);
