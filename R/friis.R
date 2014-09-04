#' Friis power gain between two antennas.
#'
#' Given the position and orientation of two antennas, calculate the
#' one-way free space power gain between them, taking into account
#' distance, antenna patterns, and polarization, but not atmospheric
#' effects.
#' 
#' @param ant1 first antenna (a 'proto' object of class 'antenna')
#' 
#' @param ant2 second antenna (a 'proto' object of class 'antenna')
#'
#' @param waveLength in metres.  Defaults to the wavelength of the
#' telemetry band frequency 166.38 MHz
#'
#' @references \url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

friis = function(ant1, ant2, waveLength=3e8/166.38e6) {
    ## we use the closed-form formula from the reference.  The only
    ## parameter is \theta, the angle between antAxis and displacement
    ## to the remote position.

    if (!inherits(ant1, "antenna") || !inherits(ant2, "antenna"))
        stop("need an antenna object for each parameter; see e.g. makeOmni()")
    
    ## get displacement vector
    disp = ant2$pos - ant1$pos

    ## magnitude of displacement
    dispMag = mag(disp)

    ## for each antenna, calculate the angles between the antenna axis (axes)
    ## and the displacement vector

    ang1 = acos(dot(ant1$axis, disp) / dispMag)
    ang2 = acos(dot(ant2$axis, disp) / dispMag)

    ## calculate antenna pattern gains for each antenna
    g1 = ant1$gain(ang1)
    g2 = ant2$gain(ang2)

    ## gain factor due to distance:
    gd = (waveLength / (4 * pi * dispMag))^2

    ## gain factor due to polarization
    ## This is the theoretical value:
    gp = dot(ant1$axis, t(ant2$axis))^2

    ## In practice, we don't find the polarization effect to be this
    ## extreme, likely due to multipath transmission, interaction
    ## with the ground, polarization of the meta-antenna that includes
    ## the bird's body, etc.  So we saturate it at the arbitrary level
    ## of -30 dB: (0.001), which is likely still too harsh.

    gp = 0.001 + 0.999 * gp

    ## combine the factors
    return (g1*g2*gd*gp)
}

    
    

