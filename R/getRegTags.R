#' Get the tag finder database of registered tags for a year.
#'
#' Given a year (or the current field season if not specified), return a data.frame
#' of all tags and their parameters registered for that year.
#'
#' @param year integer, year of project registration; defaults to current field season
#' (current year minus 3 months)
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'

getRegTags = function(year = lubridate::year(Sys.time() - 3*30*24*3600)) {
    ## FIXME: once we have these stored in SQLITE, fix the following
    
    x = read.csv(sprintf("/SG/%4d_tags.csv", year), as.is=TRUE)
    return(x)
}
