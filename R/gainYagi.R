#' Gain of yagi antenna in a particular direction.
#'
#' Calculate the one-way far-field power gain
#' for a yagi antenna of given number pattern and orientation.
#'
#' @param pattern the gain pattern for the antenna. This is a 181 x 91 element matrix
#' giving the gain in each direction specified by row (azimuth, in integer degrees
#' clockwise from North) and column (elevation angle, in integer degrees above the
#' horizontal).
#' 
#' @param axes 2x3 matrix of unit axis orientations.  The first row gives the
#' direction in which the conducting elements (cross-pieces) point.  Either of the
#' two directions (i.e. toward one end or the other of a given cross piece) can be
#' specified as the Yagi gain pattern has left-right symmetry.
#' The second row gives the direction in which the main antenna axis points.  This must
#' be specified in the back to front direction, where back is the end with the longest
#' cross-piece, and front is the end with the shortest.
#'
#' @param disp unit 3-vector in direction of interest.
#' 
#' @return one-way far-field power gain factor, in linear units,
#' relative to the isotropic pattern (i.e. a truly isotropic antenna
#' would give 1 in every direction).
#'
#' @note This gain factor is purely due to far-field antenna pattern,
#' and does not consider distance to the remote point(s) or relative
#' polarization of remote antenna.
#'
#' @references \url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

gainYagi <- function(pattern, axes, disp) {
    ap = get(sprintf("yagi%dpattern", n))

    ## the antenna pattern matrix specifies gain in directions given
    ## by azimuth angle (0..180 degrees measured clockwise from the y
    ## axis, and elevation angle (0..90 measured up from the
    ## horizontal plane), foor the situation where the antenna is
    ## oriented parallel to the ground, with major axis pointing at
    ## [0, 1, 0] and conducting elements pointing along [1, 0, 0]

    ## first, rotate the displacement vector 
    
  pat$D * pat$g[round((theta %% (2*pi)) / pat$th.step.rad) + 1, round(pmin(abs(phi), pi/2) / pat$phi.step.rad) + 1]
}
