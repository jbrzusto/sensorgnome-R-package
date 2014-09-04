#' Apply a function to one variable on each sliding window of fixed width
#' in another variable.
#'
#' Given a set of (x,y) pairs with x in non-decreasing order, apply a
#' function to each maximal set of consecutive pairs where the range
#' of x values is at most a fixed value w, the window size.  For
#' example, if x is time, y is temperature, w is 30 and the function
#' is \code{mean}, calculate the mean temperature over a sliding
#' window of width 30 seconds.
#'
#' @param x - vector of values in non-decreasing order
#'
#' @param y - vector of arbitrary values, with
#' \code{length(x)==length(y)}
#' 
#' @param w - width of sliding window in same units as \code{x}
#'
#' @param f - function of one or two variables to apply to each
#' sliding window of values.  \code{f} is called on each window of data,
#' with windowed y values if \code{f} accepts one argument and windowed
#' x and y values if \code{f} takes two arguments.
#'
#' @return result of applying \code{f} to a sliding window of y
#' values; this has the same length as \code{x} and \code{y}.  The
#' \code{i}th element is obtained by applying \code{f} to
#' \code{y[i:j]} where \code{j} is the largest index such that \code{j
#' <= length(x)} and {x[j]-x[i] < w}
#'
#' @note The number of elements of y used to compute each element in
#' the result will not in general be constant.  It will be constant if
#' the values of x are equally spaced, except that the tail end of the
#' result will be computed with fewer elements.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

windowApply = function(x, y, w, f) {
    if (length(x) != length(y))
        stop("x and y must have the same length")
    if (any(diff(x) < 0))
        stop("x must be in non-decreasing order")
    if (w < 0)
        stop("w must be non-negative")
    if (!is.function(f) || !(length(formals(f)) %in% 1:2))
        stop("f must be a function of a single variable")
    if (length(formals(f)) == 1)
        usefun = function(x, y) f(y)
    else
        usefun = f
    i = 1L
    j = 1L
    n = length(x)
    rv = rep(NA, n)
    while (j <= n) {
        if (x[j] - x[i] <= w) {
            j <- j + 1L
        } else {
            iwin = i:(j - 1L)
            rv[i] = usefun(x[iwin], y[iwin])
            i <- i + 1L
        }
    }
    if (i <= n) {
        iwin = i:n
        rv[n] = usefun(x[iwin], y[iwin])
    }
    return(rv)
}

