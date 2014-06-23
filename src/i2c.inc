;=============================================================================
;
;   File i2c.inc
;
;
;   Copyright (c) 2011, JD Gascuel, HeinrichsWeikamp, all right reserved.
;=============================================================================
; HISTORY
;  2012-10-24 : [mH] Creation

	extern	I2C_RX_accelerometer
	extern 	I2C_init_accelerometer
    extern  I2C_sleep_accelerometer
    extern  I2C_init_compass
    extern  I2C_init_compass_fast
    extern  I2C_sleep_compass
    extern  I2C_RX_compass
    extern  lt2942_get_voltage              ; Read battery voltage registers
    extern  lt2942_get_accumulated_charge   ; Get battery gauge registers
    extern  lt2942_init                     ; Init
    extern  lt2942_get_status               ; Get status