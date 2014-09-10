#' Get receiver operating status via hourly file presence.
#'
#' Counts the number of files written by a receiver during each hour for
#' which data are available.
#'
#' @param site site name
#' 
#' @param proj project name
#'
#' @param year deployment year (defaults to current year)
#' 
#' @return a data frame with columns \code{ts}, giving the top-of-the-hour timestamp
#' for each hour of each day in the full operating range, and \code{n}, the count of files
#' written within that hour.
#'
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

getOperatingStatus = function(site, proj, year = NULL) {
    if (is.null(year))
        year = year(Sys.time())
    f = siteFile(".sqlite", proj, site, year)
    if (length(f) == 0)
        stop("invalid combination of site, project, year")

    con = dbConnect("SQLite", f)
    fts = dbGetQuery(con, "select ts, bootnum from files order by ts")
    dbDisconnect(con)

    class(fts$ts) = c("POSIXt", "POSIXct")

    fts = subset(fts, ts >= ymd("2008-01-01"))
    fts$hc = as.POSIXct(round(fts$ts,"hour"))
    fts$hcf = as.factor(as.character(fts$hc))

    res = tapply(fts$ts, fts$hcf, length)
    bcr = tapply(fts$bootnum, fts$hcf, mean)

    rv = data.frame(ts=seq(from=fts$hc[1]-3600, to=fts$hc[nrow(fts)]+3600, by=3600))
    rownames(rv) = as.character(rv$ts)
    rv$count = 0
    rv$bootnum = 0
    rv[names(res), "count"] = c(res)
    rv[names(res), "bootnum"] = c(bcr)
    return(rv)
}
