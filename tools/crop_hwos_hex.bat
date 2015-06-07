:: Crops hex file from MPLAP X to work with Tinybld
:: Downloaded from http://srecord.sourceforge.net/download.html
srec_cat ../hwos.X/dist/default/production/hwos.X.production.hex -intel -crop 0x00000 0x1E000 -o hwos_tiny.hex -intel