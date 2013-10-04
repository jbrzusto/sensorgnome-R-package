## convert lotek power value (0..255) to dBm,
## given gain setting.

## According to the SRX600 product sheet, maximum RSSI
## is -40 dBm, and the resolution on RSSI is 0.25 dB.
## Also, the gain has a range of 90 dB spaced (linearly?) over
## control values in the range 0..99

## So, if gain is 0, we map 255 -> -40 dBm and 0 -> -104 dBm.
## Each unit of gain setting drops the estimate of incoming power
## by 90 / 99 dB

## WARNING: this is only as good as our reading of the product sheet!

lotekPowerTodBm = function(pwr, gain) {
  return( -40 - (255 - pwr) * 0.25 - gain * (90 / 99))
}
