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
    if (length(ant1$pos) == 3)
        disp = t(t(ant2$pos) - c(ant1$pos))
    else if (length(ant2$pos) == 3)
        disp = t(c(ant2$pos) - t(ant1$pos))
    else
        stop("FIXME: can't deal with both antennas having moving positions")

    ## magnitude of displacement
    dispMag = mag(disp)
    disp = disp / dispMag
    
    ## for each antenna, calculate the gain due to that antenna's pattern
    ## with respect to the displacement vector
    g1 = ant1$gain(disp)
    g2 = ant2$gain(-disp)

    ## gain factor due to distance:
    gd = (waveLength / (4 * pi * dispMag))^2

    ## gain factor due to polarization
    ## This is the theoretical value:
    gp = dot(ant1$axis[1,], ant2$axis[1,])^2

    ## In practice, we don't find the polarization effect to be this
    ## extreme, likely due to multipath transmission, interaction
    ## with the ground, polarization of the meta-antenna that includes
    ## the bird's body, etc.  So we saturate it at the arbitrary level
    ## of -30 dB: (0.001), which is likely still too harsh.

    gp = 0.001 + 0.999 * gp

    ## combine the factors
    return (g1*g2*gd*gp)
}

    
    

