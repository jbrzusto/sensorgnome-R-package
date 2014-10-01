tags = function(where, year = year(Sys.date() - 120 * 24 * 3600), filter = "and ((freqsd is null or freqsd < 0.1) and runLen > 2)", dbFile=NULL) {
  if (missing(where))
    stop("Need to specify a subset to select, e.g. \"proj='Taylr' and sp='IPSP'\"")
  require(RSQLite)
  if (is.null(dbFile))
      dbFile = sprintf("/SG/%d_alltags.sqlite", year)
  if (!file.exists(dbFile))
      stop("non-existent tag detections database file: ", dbFile)
  con = dbConnect("SQLite", dbFile)
  x = dbGetQuery(con, sprintf("select * from tags where %s %s", where, filter))
  class(x$ts) = c("POSIXt", "POSIXct")
  x$label = as.factor(x$label)
  x = x[order(x$ts),]
  dbDisconnect(con)
  return(x)
}
  
  
    
