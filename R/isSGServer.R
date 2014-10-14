#' Is the current machine a sensorgnome server?
#'
#' A few machines are sensorgnome servers: users upload raw data to them for
#' processing and distribution.  Such a machine will have a file called
#' 
#'    /SG/THIS_HOST_IS_AN_SG_SERVER
#'
#' @return TRUE if the current machine is a sensorgnome server; FALSE otherwise.
#'
#' @note To avoid checking for the existence of a file on each call to
#' the function, we cache the return value of this function in the
#' global variable \code{.isSGServer}.  This means that if you make
#' your machine an SG server by creating the file
#' /SG/THIS_HOST_IS_AN_SG_SERVER, you must reload the sensorgnome
#' package to recognize this.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

isSGServer = function() {
    if (exists(".isSGServer"))
        return(.isSGServer)
    .isSGServer <<- FALSE
    try ({
        .isSGServer <<- isTRUE(file.exists("/SG/THIS_HOST_IS_AN_SG_SERVER"))
    }, silent=TRUE)
    return(.isSGServer)
}
