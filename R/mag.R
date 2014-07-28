#' Magnitudes of vectors.
#'
#' Get the Euclidean norm of a single 3-vector or each row of an nx3 matrix.
#'
#' @param x 3-element vector or n x 3 matrix
#'
#' @return a scalar or n-vector of norms of x
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

mag = function(x) {
    if (length(x) == 3)
        sqrt(sum(x^2))
    else
        apply(x, 1, function(x) sqrt(sum(x^2)))
}
