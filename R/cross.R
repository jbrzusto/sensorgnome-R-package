#' Cross product of vectors.
#'
#' Computer the Euclidean cross product of two 3-vectors.
#'
#' @param a numeric 3-vector
#' 
#' @param b numeric 3-vector
#'
#' @param unit logical; return a unit vector?
#'
#' @return 3-vector giving the standard vector cross product, or a unit
#' vector in the same direction, if \code{unit} is TRUE.
#' The cross product has magnitude \code{|a||b|sin(\theta)} where \code{theta}
#' is the (smaller) angle between the two vectors.  The direction is perpendicular
#' to both vectors, with sign chosen by the right hand rule.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#' 
## cross product of two 3-vectors
cross = function(a, b, unit = FALSE) {
    cp = c(a[2]*b[3]-a[3]*b[2], a[3]*b[1]-b[3]*a[1], a[1]*b[2]-a[2]*b[1])
    if (!unit)
        return (cp)
    return(cp / sqrt(sum(cp^2)))
}
