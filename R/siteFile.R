## siteFile: return the full path to a file for a given year, project, site
##
siteFile = function(file, proj, site, year = lubridate::year(Sys.time())) {
    rv = file.path(sitePath(proj, site, year), sprintf("%4d_%s_%s%s", as.integer(year), proj, site, file))
    return (rv[file.exists(rv)])
}
