#' Generate a plotting label for a set of tags.
#'
#' For a set of full IDs from a given deployment year, generate
#' labels suitable for plotting.  The deployment database is
#' searched for a species code.
#' 
#' @param fullID character or factor, vector of fullIDs
#' 
#' @param year integer scalar, registration year for the tags (e.g. 2014)
#'
#' @param tagDepFile if not NULL, use this as the tag deployment database file,
#' rather than looking in the standard place for a file appropriate to \code{year}
#' This file must have at least the columns \code{fullID} and \code{sp}.
#' 
#' @return a list with two elements:
#' \enumerate{
#'  \item "sp" - a character vector of species codes, of the same length as \code{fullID}
#'  \item "label" - a character vector with the same length as \code{fullID}, giving
#' a label suitable for use in plots
#' }
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'
#' @examples  getLabel("Taylr#335@@166.380:6.1", 2014)
#'            ## returns list(sp="BLPW", label="Taylr 335:6.1 @@ 166.380 BLPW")

getLabel = function(fullID, year, tagDepFile=NULL) {
    if (is.null(tagDepFile)) {
        tagDepFile = sprintf("/SG/%d_tag_deployments.csv", year)
    }
    fid = as.character(fullID)
    if (file.exists(tagDepFile)) {
        tagDeps = read.csv(tagDepFile, as.is=TRUE)
        tagDeps = subset(tagDeps, ! duplicated(tagDeps$fullID))
        rownames(tagDeps) = tagDeps$fullID
        sp = tagDeps[fid, "sp"]
    } else {
        tagDeps = NULL
        sp = ""
    }
    parts = do.call('rbind', strsplit(fid, "[#@:]", perl=TRUE))
    return (list(sp=sp, label=sprintf("%s %5g:%s @ %s %s", parts[,1], as.numeric(parts[,2]), parts[,4], parts[,3], sp)))
}
