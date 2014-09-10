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
#'
#'
#'@examples
#' 
#'
#' ##
#'##
#'## simple test: 4 omnis set up at corners of 1000 metre square
#'## bird flies past on random path at random speed
#'##
#'
#'## receivers: 4 vertical omni antennas at corners of 1000 metre square
#'## centred at (0, 0, 0)
#'
#'rx = list (
#'    makeOmni(c( -500, -500, 0), "vertical"),
#'    makeOmni(c( -500,  500, 0), "vertical"),
#'    makeOmni(c(  500, -500, 0), "vertical"),
#'    makeOmni(c(  500,  500, 0), "vertical")
#'    )
#'
#'## random uniform bird path (parallel to ground at random height)
#'
#'## uniform velocity (x, y) each component in range [-10, 10] m/s
#'## and component 0 in z direction
#'
#'velocity = c(runif(2, -10, 10), 0)
#'
#'## constant height
#'
#'height = runif(1, 0, 2000)
#'
#'## random (x, y) location at time zero, in 2000 x 2000 metre square
#'## centred at (0, 0, 0)
#'
#'loc0 = c(runif(2, -1000, 1000), height)
#'
#'## path time interval is [-100, 100] seconds; tag bursts every 5 seconds
#'
#'t = seq(from = -300, to = 300, by = 5)
#'n = length(t)
#'## path: n x 3 matrix giving x, y, z at each location
#'
#'path =  t(t(outer(t, velocity, `*`)) + loc0)
#'
#'## bird's transmitter antenna
#'
#'tx = makeOmni(pos=path[1,], axis=velocity)
#'
#'dB = function(x) 10*log10(x)
#'
#'## calculate relative signal strength at each receiver at each point in time
#'
#'tx$pos = path
#'
#'rss = lapply(rx, function(x, y) dB(friis(x, y)), tx)
#'
#'plot(t, rss[[1]], ylim=range(unlist(rss)))
#'for(i in 2:length(rx))
#'    points(t, rss[[i]], col=i)
#'plot(path[,1:2], xlim=c(-2000,2000), ylim=c(-2000,2000), type="l")
#'arrows(path[1,1], path[1,2], path[n,1], path[n,2])
#'for (i in seq(along=rx))
#'    points((t(rx[[i]]$pos[1:2])), pch=as.character(i), col=i)
#'
#'
#'## another test: each antenna oriented horizontally and perpendicular
#'## to displacement from origin
#'
#'rx = list (
#'    makeOmni(c( -500, -500, 0), c(1, -1, 0)),
#'    makeOmni(c( -500,  500, 0), c(1, 1, 0)),
#'    makeOmni(c(  500, -500, 0), c(1, 1, 0)),
#'    makeOmni(c(  500,  500, 0), c(1, -1, 0))
#'    )
#'
#'rss = lapply(rx, function(x, y) dB(friis(x, y)), tx)
#'
#'plot(t, rss[[1]], ylim=range(unlist(rss)))
#'for(i in 2:length(rx))
#'    points(t, rss[[i]], col=i)
#'plot(path[,1:2], xlim=c(-2000,2000), ylim=c(-2000,2000), type="l")
#'arrows(path[1,1], path[1,2], path[n,1], path[n,2])
#'for (i in seq(along=rx))
#'    points((t(rx[[i]]$pos[1:2])), pch=as.character(i), col=i)
#'
#'
#'
#'## now a Yagi antenna pointing NE at 45 degrees upward
#'
#'a = makeYagi(n=9, axis1=c(45, 45), axis2="horizontal", pos=c(0, 0, 0))
#'
#'## verify that the antenna pattern in the plane containing the antenna
#'## matches that of the antenna in standard position.
#'
#'th=rad(0:360)
#'
#'## a circle in the antenna's plane, starting in the antenna's major
#'## direction and going counter-clockwise (the antenna's axis matrix
#'## has the major axis on the second row; the cross-piece axis on the
#'## first row)
#'pl = cbind(cos(th), sin(th)) %*% a$axis[2:1,]
#'
#'g = a$gain(pl)
#'
#'scaled.g = 30 + pmax(-30, 10*log10(g))
#'
#'plot(cos(th) * scaled.g, sin(th) * scaled.g)
#'
#'
#' 


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

    
    

