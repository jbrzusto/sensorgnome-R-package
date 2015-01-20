#' Get names and codes of projects.
#' 
#' Get the list of project directory names and associated project codes
#' for a given year.
#' 
#' @param year integer, registration year for projects.  If blank, uses the
#' year for the most recent field year (i.e. the year of 3 months ago.)
#'
#' @return a character vector of project codes.  These are the names
#' used in both the tag characteristics and tag detection databases.
#' The names of the elements are the project directories (under
#' /SG/contrib/YEAR/ on the server).
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'
## getProjects.R: return a list of project codes and directory names for a given
## deployment year.

getProjects = function(year = lubridate::year(Sys.time() - 3*30*24*3600)) {
    top = file.path("/", "SG", "contrib", year)
    maybe = dir(top, pattern="^[-_a-z0-9]+", full.names=TRUE)
    projcodefiles = file.path(maybe, "PROJCODE.TXT")
    keep = file.exists(projcodefiles)
    return (structure(sapply(projcodefiles[keep], function(f) readLines(f)), names=basename(maybe)[keep]))
}
