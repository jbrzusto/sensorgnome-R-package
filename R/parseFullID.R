#' Split a fullID into parts.
#'
#' For a vector of tag fullIDs, return a data.frame with one row per tag
#' and one colum per component of fullID.
#'
#' @param fid character vector of fullIDs
#'
#' @return data.frame with these columns:
#' \enumerate{
#' \item proj character project code
#' \item id integer manufacturer tag ID
#' \item freq tag broadcast frequency in MHz
#' \item bi tag burst interval, in seconds
#' }
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

parseFullID = function(fid) {
    return (do.call('rbind', strsplit(fid, "[#@:]", perl=TRUE)))
}
