## convert lotek power value (0..255) to dB max,
## given gain setting.

## According to the SRX600 product sheet, maximum RSSI
## is -40 dBm, which we'll call 0 dBmax.

## Then, each unit of power is 0.25 dB.  Also, the gain
## has a range of 90 dB spaced over values 0..255 

##  We assume the power value is linear in dBm, but with unknown
## scale.  We also want to normalize to a gain setting of 80.  In the
## lab, we found the gain affects signal power by 4 units for each 1
## unit increase in gain.

## Note the dynamic range setting, which is probably generous

lotekPowerTodB = function(pwr, gain) {
  return(- (255 - pwr) * 0.25 - gain / 255 * 90)
}
