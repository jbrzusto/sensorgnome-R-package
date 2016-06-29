#' Download and save a file from the sensorgnome.org wiki
#'
#' Once you have called \link{\code{sgListFiles(...)}}, you can download any
#' file it returns using a call to this function.
#'
#' @param fileList; a data.frame returned by \link{\code{sgListFiles()}}
#'
#' @param i; integer scalar; which file in  \code{fileList} to download,
#' starting at 1.  The function will download the file described by the \code{i} th
#' row of \code{fileList}
#' 
#' @param userpw character; username and password for sensorgnome.org, separated
#' by a colon; e.g. \code{"john:NotMyRealPassword"}
#'
#' @param dir; character; folder into which to download the file; defaults to
#' the current directory, as returned by \link{\code{getpwd()}}
#' 
#' @examples
#'
#' ## Usage:
#'
#' ## l = sgListFiles("User:adam/CedarIsland", "adam:XXXXX")  ## replace XXXXX with your password
#' ## l
#' ## sgGetFile(l, 3, "adam:XXXXX") ## download and save the 3rd file from the list

#' @seealso \link{\code{sgListFiles}}
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'
#' @export

sgGetFile = function(fileList, i, userpw, dir=".") {
    URL = sprintf("https://sensorgnome.org/@api/deki/files/%d", fileList$id[i])
    writeBin(getBinaryURL(URL, userpw=userpw), file.path(dir, fileList$name[i]))
}
