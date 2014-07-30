#' Dot product of vectors.
#'
#' Computer the (real) Euclidean dot product of 3-vectors.
#'
#' @param x1 left vector, either 3-vector or \code{n x 3} matrix
#' 
#' @param x2 right vector, either 3-vector or \code{n x 3} matrix
#'
#' @return vector or matrix of dot products.  If both \code{x1} and \code{x2}
#' are 3-vectors, then the result is a scalar.  If \code{x1} is a 3-vector
#' but \code{x2} is an \code{n x 3} matrix, the results is a vector of length
#' \code{n} giving the dot product of x1 with each row of x2.  Similarly
#' if \code{x1} is an \code{n x 3} matrix and \code{x2} is a 3-vector.
#' Finally, if \code{x1} is \code{m x 3} and \code{x2} is  \code{m x 3},
#' returns an \code{m x n} matrix with entry \code{[i, j]} giving the
#' dot product of the \code{i}-th row of \code{x1} with the \code{j}-th row
#' of \code{x2}.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

dot = function(x1, x2) {
    if (length(x1) == 3)
        apply(x1 * t(x2), 2, sum)
    else if (length(x2) == 3)
        apply(t(x1) * x2, 2, sum)
    else
        outer(x1[,1], x2[,1], `*`) + outer(x1[,2], x2[,2], `*`) + outer(x1[,3], x2[,3], `*`)
}

