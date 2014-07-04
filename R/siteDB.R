## siteDB: return the full path to a site's .sqlite file
## which holds metadata about gps, param settings, and raw files
##

siteDB = function(proj, site, year = lubridate::year(Sys.time())) {
    siteFile(".sqlite", proj, site, year)
}
