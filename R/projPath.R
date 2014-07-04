##
## path to the top of a project in a given year
##
projPath = function(proj, year = lubridate::year(Sys.time())) {
    SGPath("contrib", year, proj)
}
