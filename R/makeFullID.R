#' Create a tag fullID field from its components.
#'
#' Given a dataframe with id, proj, bi, and tagFreq columns return,
#' a vector of fullIDs, one per row.
#'
#' @param tags data.frame with at least these columns:
#' \enumerate{
#' \item proj character project code
#' \item id integer manufacturer tag ID
#' \item tagFreq tag broadcast frequency in MHz
#' \item bi tag burst interval, in seconds
#' }

makeFullID = function(tags) {
    sprintf("%s#%d@%g:%g", tags$proj, tags$id, round(tags$tagFreq, 3), round(tags$bi, 1))
}

