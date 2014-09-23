tags = function(where, year = 2013, filter = "and ((freqsd is null or freqsd < 0.1) and runLen > 2)") {
  if (missing(where))
    stop("Need to specify a subset to select, e.g. proj=='Taylr' and sp=='IPSP'")
  require(RSQLite)
  con = dbConnect("SQLite", sprintf("/SG/%d_alltags.sqlite", year))
  x = dbGetQuery(con, sprintf("select * from tags where %s %s", where, filter))
  class(x$ts) = c("POSIXt", "POSIXct")
  x$label = as.factor(x$label)
  x = x[order(x$ts),]
  dbDisconnect(con)
  return(x)
}
  
  
    
