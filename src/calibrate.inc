;=============================================================================
;
;   File calibrate.inc
;
;=============================================================================

	extern		compute_ppo2			; compute mv_sensorX and ppo2_sensorX arrays
	extern		calibrate_mix			; Calibrate with any mix
    extern      check_sensors           ; Check O2 sensor thresholds for fallback
    extern      transmit_setpoint       ; Transmit current setpoint from WREG (in cbar) to external electronics