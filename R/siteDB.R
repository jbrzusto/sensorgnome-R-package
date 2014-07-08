## siteDB: return the full path to a site's .sqlite file
## which holds metadata about gps, param settings, and raw files
##

siteDB = function(proj, site, year = lubridate::year(Sys.time())) {
    rv = siteFile(".sqlite", proj, site, year)
    if (length(rv) == 0)
        stop("No database for specified year, project, site")
    return(rv)
}
