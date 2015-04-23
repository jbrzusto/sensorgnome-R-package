#' Return tag detections from an sqlite detections database.
#'
#' Tag detections are provided as an sqlite database file.  This lets you load
#' subsets of data without having to read the entire file into memory.
#'
#' @param where if specified, an SQL query specifying a subset of data to be read.
#' If not specified, read all tag detections, subject to the \code{filter} parameter
#' below.  See below for a few examples of SQL syntax.  If only a single
#' unnamed parameter is specified, and if it is the path to an \code{.sqlite} file, then
#' it is treated as the \code{dbFile} parameter, and \code{where} is set
#' to \code{""}.
#' 
#' @param dbFile the name of the .sqlite database file to read from.
#' This is typically named \code{YEAR_PROJECT_SITE_alltags.sqlite} or
#' \code{YEAR_PROJECT_globaltags.sqlite} where \code{YEAR},
#' \code{PROJECT}, and \code{SITE} are the 4-digit year, project name,
#' and site name.  The \code{*_globaltags.sqlite} file holds all
#' detections of your tags from all sites.  Site-specific
#' \code{*_alltags.sqlite} files hold all detections of any tags from
#' one of your sites.  If not specified, then the most recently used
#' dbFile argument is re-used.  If this is the first time \code{tags()} has
#' been called in this R session, then the first file named
#' \code{*_alltags.sqlite} or \code{*_globaltags.sqlite} in the
#' current working folder will be used, with a warning.
#'
#' @param filter by default, only return detections with small
#' variance in offset frequency of pulses, and run length of at least
#' 3 detections.  You can specify \code{filter=""} to disable
#' filtering and return all detections in the database. This will
#' likely return large numbers of false positives from sites with significant
#' radio noise.
#'
#' @param year only used when running on a sensorgnome server, specify the year of
#' the database from which to return detections.  Defaults to the 'latest
#' field season', which is the year of today's date minus 3 months.
#' 
#' @return a data.frame of tag detections, sorted by increasing time.
#'
#' @note
#' As of October 2014, the .sqlite format will be the preferred format for
#' distribution of detection data by the sensorgnome.org project
#'
#' @examples
#'\dontrun{
#' ## Return all filtered detections from a database file.
#'
#'   t = tags("path/to/my/data/2014_peterson_monhegan_alltags.sqlite")
#'
#' ## Return all detections of Taylor project tags from 2013 with IDs in the range 100..120
#' 
#'   t = tags("2013_taylor_globaltags.sqlite", "id >= 100 and id <= 120")
#'
#' ## Return all *unfiltered* detections from the Taylor project's 2014 Seal Island East
#' ## site of tags from the Lorng or Welch projects.
#' 
#'   t = tags("2014_taylor_seal_island_east_alltags.sqlite", "tagProj in ('Lorng', 'Welch')", filter="")
#'
#' ## (Running on a Sensorgnome Server only) return all detections in the current year
#' ## of tag 480 from the Taylor project
#'
#'   t = tags("id=480 and tagProj='Taylr'")
#'
#'}
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

tags = function(where="1=1", dbFile = NULL, filter = "and ((freqsd is null or freqsd < 0.1) and runLen > 2)", year) {
  if (missing(where) && isSGServer())
    stop("On an SG Server, you need to specify a subset to select, e.g. \"proj='Taylr' and sp='IPSP'\"")
  require(RSQLite)
  if (nargs() == 1 && grepl("\\.sqlite$", where)) {
      dbFile = where
      where = "1=1"
  }
  if (is.null(dbFile)) {
      if (isSGServer()) {
          if (missing(year)) {
              year = lubridate::year(Sys.time() - 120 * 24 * 3600)
              warning("No year specified; defaulting to 'recent' field year ", year)
          }
          dbFile = sprintf("/SG/%d_alltags.sqlite", year)
      } else if (exists(".lastSGDataFile")) {
          dbFile = .lastSGDataFile
      } else {
          dbFile = dir(".", pattern=".*(alltags.sqlite|globaltags.sqlite)")[1]
          if (is.na(dbFile))
              stop("No dbFile specified, no previous dbFile in this R session, and no *alltags.sqlite or *globaltags.sqlite file found in current working folder")
          else
              warning("No dbFile specified; using file ", dbFile, " found in current working folder")
      }
  }
  if (!file.exists(dbFile))
      stop("non-existent tag detections database file: ", dbFile)
  .lastSGDataFile <<- dbFile
  con = dbConnect(RSQLite::SQLite(), dbFile)
  x = dbGetQuery(con, sprintf("select * from tags where %s %s", where, filter))
  x = x[order(x$ts),]
  class(x$ts) = c("POSIXt", "POSIXct")
  x$label = as.factor(x$label)
  dbDisconnect(con)
  return(x)
}
