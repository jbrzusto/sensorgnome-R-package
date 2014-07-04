## siteGPS: get the table of recorded GPS fixes for a site
##

siteGPS = function(proj, site, year = lubridate::year(Sys.time())) {
    rv = siteSQL("select * from gps", proj, site, year)
    class(rv$ts) = c("POSIXt", "POSIXct")
    return(rv)
}
    
