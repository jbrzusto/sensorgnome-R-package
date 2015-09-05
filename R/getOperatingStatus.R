#' Get receiver operating status.
#'
#' Counts the number of files written by a receiver during each hour for
#' which data are available.  Examines GPS records to see when GPS was
#' functioning (vs. stuck on a fix). If the site database has it,
#' also returns a list of counts of each pulse type (i.e. USB port code)
#' per hour.
#'
#' @param site site name
#' 
#' @param proj project name
#'
#' @param year deployment year (defaults to current year)
#' 
#' @return a list with these items:\cr
#' \describe{
#' \item{files.per.hour}{data frame with at most one record per hour, giving number of raw data files recorded and bootcount}
#' \item{gps.fix}{data frame with at most hourly true GPS fix (i.e. repeated 'stuck' fixes are not reported)}
#' \item{undated.period}{the number of days of data recorded with dates beginning with 1 Jan 2000; these are periods when the GPS had not set the clock}
#' \item{num.boots}{the number of times the system was restarted during the operating period}
#' \item{pulse.counts}{data frame with columns pcode, ts, count giving count of pulses from each port per hour}
#'}
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

getOperatingStatus = function(site, proj, year = lubridate::year(Sys.time())) {
    f = siteFile(".sqlite", proj, site, year)
    if (length(f) == 0)
        stop("invalid combination of site, project, year")

    con = dbConnect(RSQLite::SQLite(), f)
    fts = dbGetQuery(con, "select bootnum, min(ts) as ts, count(ts) as num from files group by bootnum, round(ts/3600 - 0.5)*3600 order by ts")
    gps = dbGetQuery(con, "select distinct min(ts) as ts, lat, lon from gps group by round(ts/3600-0.5)*3600 order by ts")

    if (dbExistsTable(con, "pulseCounts")) {
        pulseCounts = dbGetQuery(con, "select pcode, hourBin as ts, sum(count) as count from pulseCounts group by pcode, hourBin order by hourBin");
        pulseCounts$ts = structure(pulseCounts$ts * 3600, class=class(Sys.time()))
    } else {
        pulseCounts = NULL
    }
    dbDisconnect(con)

    class(fts$ts) = class(gps$ts) = c("POSIXt", "POSIXct")

    ## duration of unpinned data (no GPS timefix)
    undated.period = diff(range(fts$ts[fts$ts <= ymd("2002-01-01")]))

    return (list(files.per.hour = fts, gps.fix = gps, undated.period = undated.period, num.boots = diff(range(fts$bootnum)), pulse.counts = pulseCounts))
}
