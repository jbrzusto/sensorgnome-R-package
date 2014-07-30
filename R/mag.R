#' Magnitudes of vectors.
#'
#' Get the Euclidean norm of a single 3-vector or each row of an nx3 matrix.
#'
#' @param x 3-element vector or \code{n x 3} matrix
#'
#' @return the norm of x, or an n-vector giving the norm of each row of x (for a matrix)
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

mag = function(x) {
    if (length(x) == 3)
        sqrt(sum(x^2))
    else
        apply(x, 1, function(x) sqrt(sum(x^2)))
}
