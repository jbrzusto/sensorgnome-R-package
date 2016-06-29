#' List files available for download on a sensorgnome.org wiki page
#'
#' @param page character; path to a single user page at sensorgnome.org
#' e.g. \code{User:adam/CedarIsland}  This must start with "User:" if
#' the file is on a user's private page, and is typically followed by
#' the user's name and the site name.
#'
#' @param userpw character; username and password for sensorgnome.org, separated
#' by a colon; e.g. \code{"john:NotMyRealPassword"}
#'
#' @return a data.frame of files attached to the specified page, with these columns:
#' \itemize{
#' \item name: full name of the file
#' \item id: internal ID number of the file, used for download by \link{\code{sgGetFile}}
#' \item revision: revision of file; how many versions exist?
#' \item size: size of file, in bytes
#' }
#'
#' @examples
#'
#' ## Usage:
#'
#' ## l = sgListFiles("User:adam/CedarIsland", "adam:XXXXX")  ## replace XXXXX with your password
#' ## l
#' ## sgGetFile(l, 3, "adam:XXXXX") ## download and save the 3rd file from the list

#' @seealso \link{\code{sgGetFile}}
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'
#' @export


sgListFiles = function(page, userpw) {
    URL = sprintf("https://sensorgnome.org/@api/deki/pages/=%s/files",
                  URLencode(URLencode(page, TRUE), TRUE))
    x = getURL(URL, userpw=userpw)
    fileInfo = data.frame(name=character(0), id=integer(0), revision=integer(0), size=numeric(0), stringsAsFactors = FALSE)
    xmlParse(x, handlers=list(
                    file = function(n) {
                        fileInfo <<- rbind(fileInfo,
                                           data.frame(
                                               name = xmlValue(xmlChildren(n)[["filename"]]),
                                               id = as.integer(xmlAttrs(n)[["id"]]),
                                               revision = as.integer(xmlAttrs(n)[["revision"]]),
                                               size = as.numeric(xmlAttrs(xmlChildren(n)[["contents"]])[["size"]]),
                                               stringsAsFactors = FALSE
                                           )
                                           )
                    })
             )
    return(fileInfo)
}
