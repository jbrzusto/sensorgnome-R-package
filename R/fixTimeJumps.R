#' Fix timejumps in a series of timestamps from a sensorgnome.
#'
#' \code{fixTimeJumps} corrects jumps in a series of timestamps.  These jumps are
#' presumably due to transmission errors in the data from the USB gps, perhaps in
#' a noisy environment over a USB extension cable.
#' The gpsd program, which updates the sensorgnome clock from GPS data, is smart
#' enough not to set the clock backward, so only bit errors causing large forward
#' jumps will propagate through to the system clock, and hence sensorgnome data files.
#' 
#' Time jumps are detected in raw data streams by the sgdb.R script,
#' using a threshold (in a noisy data stream, very long intervals
#' between consecutive pulse detections are almost certainly due to
#' time jumps), and recorded to the YEAR_PROJECT_SITE.sqlite database
#' 
#' @param recvDB character scalar giving full path to the sqlite database for the receiver
#' @param ts numeric vector of raw timestamps
#' @param bootnum numeric vector of bootnumbers, same length as \code{ts}
#' @param depID numeric vector of deployment IDs, same length as \code{ts}
#' @return a numeric vector of timestamps corrected for timejumps.
#'

fixTimeJumps = function (recvDB, ts, bootnum, depID) {
  if (!file.exists(recvDB))
    stop("The file ", recvDB, " does not exist")
  con = dbConnect("SQLite", recvDB)
  on.exit(dbDisconnect(con))

  if (! "timeJumps" %in% dbListTables(con)) {
    warning("No timeJumps table found in ", recvDB)
    return (ts)
  }

  alltj = sql(con, "select * from timeJumps order by timeJumps.tsBefore")
  if (nrow(alltj) == 0)
    return (ts)

  oneFix = function(i) {
    ## fix timeJumps for timestamps in one bootnum / depID pair
    ## in case we didn't detect the time jump soon enough, back date it by
    ## 30 minutes (= 1800 seconds)
    
    tj = alltj[i,]
    tj = tj[order(tj$tsBefore),]
    tj$tsAfter = tj$tsAfter - 1800
    fromTimes = c(tj$tsAfter, max(tj$tsAfter) + 1e8) - 1800
    tjmap = approxfun(fromTimes, fromTimes - cumsum(c(tj$tsAfter - tj$tsBefore, 0)))
    jumps = which(ts >= tj$tsAfter[1] & bootnum == alltj$bootnum[i[1]] & depID == alltj$depID[i[1]])
    ts[jumps] <<- tjmap(ts[jumps])
  }

  invisible(tapply(1:nrow(alltj), list(alltj$depID, alltj$bootnum), oneFix))
  return(ts)
}


