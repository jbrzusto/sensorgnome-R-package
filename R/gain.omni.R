#' Gain of an omnidirectional antenna at remote points.
#'
#' Given the position and orientation of an omnidirectional antenna,
#' calculate the one-way power gain at remote points.
#' 
#' @param antPos 3-element antenna position vector (x, y, z)
#' 
#' @param antAxis 3-element antenna orientation axis (x, y, z). The
#' conducting element of the antenna is parallel to this vector.
#'
#' @param remPos 3-element vector (x, y, z) or n x 3 matrix of remote locations
#' at which gain is to be calculated.
#'
#' @return gain factor, in linear units, with maximum possible value of 1.0
#'
#' @note This gain factor is purely due to far-field antenna pattern, and does not
#' consider distance to the remote point(s) or relative polarization of
#' remote antenna.
#'
#' @references \url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

gain.omni = function(antPos, antAxis, remPos) {
    ## we use the closed-form formula from the reference.  The only
    ## parameter is \theta, the angle between antAxis and displacement
    ## to the remote position.

    ## get 3 x n matrix of displacements
    disp = t(remPos) - antPos

    ## make sure antenna axis is unit
    antAxis = antAxis / mag(antAxis)

    ## magnitude of displacement vectors
    dispMag = mag(disp)

    ## cosine of theta is dot product over magnitude
    dotProd = dot(antAxis, disp)
    cosTheta = dotProd / dispMag
    sinTheta = sqrt(1 - cosTheta ^2)
    
    ## E-field gain is cos (pi / 2 * cos(theta)) / sin(theta)
    ## square this for power.  Deal correctly with theta ~ 0.

    ifelse(abs(sinTheta) >= .Machine$double.eps, cos(pi / 2 * cosTheta) / sinTheta, 0)^2
}

    
    
