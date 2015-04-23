#' Quit script with warning message but zero exit code.
#'
#' Soft error from script which allows the calling script to continue
#' but still reports a warning message.  Equivalent to
#' \code{warning(...); q(save="no")}
#'
#' @param ... arguments as accepted by \code{warning()}
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
qWarn = function(...) {
    warning(...)
    q(save="no")
}

