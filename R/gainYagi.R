#' Gain of yagi antenna in a particular direction.
#'
#' Calculate the one-way far-field power gain
#' for a yagi antenna of given number pattern and orientation.
#'
#' @param pattern the gain pattern for the antenna. This is a 181 x 91 element matrix
#' giving the gain in each direction specified by row (azimuth, in integer degrees
#' clockwise from North) and column (elevation angle, in integer degrees above the
#' horizontal).  The pattern corresponds to the antenna in standard position;
#' i.e. main axis pointing in positive y direction; cross-pieces pointing along
#' the x axis.
#' 
#' @param disp unit 3-vector or n x 3 matrix of unit vector rows in
#' direction(s) of interest.  This must already have been rotated to
#' correspond to the antenna in standard position.
#' 
#' @return one-way far-field power gain factor, in linear units,
#' relative to the isotropic pattern (i.e. a truly isotropic antenna
#' would give 1 in every direction), for each row of \code{disp}
#'
#' @note This gain factor is purely due to far-field antenna pattern,
#' and does not consider distance to the remote point(s) or relative
#' polarization of remote antenna.
#'
#' @references \url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

gainYagi <- function(pattern, disp) {

    ## the antenna pattern matrix specifies gain in directions given
    ## by azimuth angle (0..180 degrees measured clockwise from the y
    ## axis, and elevation angle (0..90 measured up from the
    ## horizontal plane), foor the situation where the antenna is
    ## oriented parallel to the ground, with major axis pointing at
    ## [0, 1, 0] and conducting elements pointing along [1, 0, 0]

    ## get the azimuth and elevation angles, in degrees, forcing
    ## the point to have x >= 0
    
    theta = 90 - deg(atan2(disp[,2], abs(disp[,1])))
    phi = deg(acos(pmin(1, sqrt(disp[,1]^2+disp[,2]^2))))

    ## fixme: interpolate between grid points
    return(pattern[cbind(1+round(theta), 1+round(phi))])
}
