##
## path to the top of a project in a given year
##
sitePath = function(proj, site, year = lubridate::year(Sys.time())) {
    rv = SGPath("contrib", year, proj, site)
    return (rv[file.exists(rv) & file.info(rv)$isdir])
}
