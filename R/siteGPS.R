## siteGPS: get the table of recorded GPS fixes for a site
##

siteGPS = function(proj, site, year = lubridate::year(Sys.time())) {
    library(RSQLite)
    db = siteDB(proj, site, year)
    if (length(db) == 0)
        stop("Error: no database for specified project, site, and year")
    con = dbConnect("SQLite", siteDB(proj, site, year))
    rv = dbGetQuery(con, "select * from gps")
    dbDisconnect(con)
    class(rv$ts) = c("POSIXt", "POSIXct")
    return(rv)
}
    
