##
## timePins: return the table of time pins for a site.  This allows
## correcting raw file timestamps in the same way the findtags.R
## algorithm does (i.e. dates within a single power cycle before
## a GPS fix was obtained are back-corrected, provided a GPS fix
## was eventually obtained during that power cycle).

timePins = function(proj, site, year = lubridate::year(Sys.time())) {
    rv = siteSQL("select * from timePins", proj, site, year)
    rv$bootstart = rv$gpsts-(rv$systs - as.numeric(ymd("2000-01-01")))
    class(rv$bootstart) = class(rv$systs) = class(rv$gpsts) = c("POSIXt", "POSIXct")
    return(rv)
}
