## with USRP E100 + WBX generating samples like this:

## /usr/share/uhd/examples/tx_waveforms --rate 1000000 --freq 166.380e6 --gain 0 --ampl 0.005 --wave-freq 4000 --wave-type=CONST

## we get a scope reading of 1.9 mV peak to peak into 50 Ohms, 
## which means power is -50.45 dBm (0.0019/2)^2 / 2 / 50
## and in Audacity, this shows peak-to-peak variation of 0.8817 units (on the -1 to 1 range)
## and so approximately -1.1 or -1.2 dBmax.

## i.e. the Funcube Pro Plus (with default gain & filtering) converts -50.34 dBm to -1.15 dBmax 

## on the Funcube Pro (with default gain & filtering), this converts to -2.8 dBmax

## So for now, until we do this more carefully, dBm = dBMax - 50

sgPowerTodBm = function(pwr, gain) {
  return (pwr - 50 - gain)
}
