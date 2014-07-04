## getProjects.R: return a list of project names for a given deployment year
##

getProjects = function(year = lubridate::year(Sys.time())) {
    top = file.path("/", "SG", "contrib", year)
    maybe = dir(top, pattern="^[-_a-z0-9]+", full.names=TRUE)
    projcodefiles = file.path(maybe, "PROJCODE.TXT")
    return(basename(maybe)[file.exists(projcodefiles)])
}
