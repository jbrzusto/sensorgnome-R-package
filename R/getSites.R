## getSites.R: return a list of sites for a given year (and set of projects, if specified)
##

getSites = function(proj = NULL, year = lubridate::year(Sys.time())) {
    if (is.null(proj))
        proj = getProjects(year)

    rv = NULL
    for (p in proj) {
        top = projPath(p, year)
        maybe = dir(top, pattern="^[-_a-z0-9]+", full.names=TRUE)
        sitecodefiles = file.path(maybe, "SITECODE.TXT")
        goodsites = basename(maybe)[file.exists(sitecodefiles)]
        if (length(proj) > 1)
            rv = c(rv, file.path(p, goodsites))
        else
            rv = goodsites
    }
    return(rv)
}
