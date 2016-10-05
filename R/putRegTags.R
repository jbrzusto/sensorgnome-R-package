#' Put the set of registered tags for a year to the tag registartion database.
#'
#' Given a data.frame of registered tags and their parameters,
#' store it in the standard place for the given year's tag registrations.
#'
#' @param rtags, data.frame of registered tags.  Must have at least these columns:
#' \enumerate{
#' \item proj character tag project code
#' \item id integer manufacturer's tag ID
#' \item tagFreq real tag broadcast frequency, MHz
#' \item bi real burst interval, seconds
#' \item g1 gap between pulses 1, 2 for Lotek coded tags, milliseconds
#' \item g2 gap between pulses 2, 3 for Lotek coded tags, milliseconds
#' \item g3 gap between pulses 3, 4 for Lotek coded tags, milliseconds
#' }
#' 
#' @param year integer, year of project registration; defaults to current field year
#' (current year minus 3 months)
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'

putRegTags = function(x, year = lubridate::year(Sys.time() - 3*30*24*3600)) {
    ## FIXME: once we have these stored in SQLITE, fix the following
    
    write.csv(x, sprintf("/SG/%4d_tags.csv", year), row.names=FALSE)
}
