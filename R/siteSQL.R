## siteSQL: do a query on a site's sqlite meta database
##

siteSQL = function(query, proj, site, year = lubridate::year(Sys.time())) {
    db = siteDB(proj, site, year)
    con = dbConnect(RSQLite::SQLite(), db)
    rv = dbGetQuery(con, query)
    dbDisconnect(con)
    return(rv)
}
