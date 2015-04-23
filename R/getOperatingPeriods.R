## get the operating periods of a sensorgnome.  Any jump in GPS timestamp
## larger than jumpsize is treated as a new 'operating period'.
##
## Args
##  - f: path to a site database e.g. "/SG/contrib/2013/taylor/ryan_leet/2013_taylor_ryan_leet.sqlite"
##  - jumpsize: minimum number of seconds between consecutive GPS fixes which is treated as a break
##    between periods.  Note that typically, GPS fixes were reported every 5 minutes, with a bit of
##    variation around that, so that the default of 10 minutes represents a completely missed GPS fix.
##
## Returns a data.frame with these columns:
## - from: first GPS timestamp of period (GMT)
## - to:   last GPS timestamp of period (GMT)
## - meanLat: mean latitude during period (deg. N)
## - meanLon: mean longitude during period (deg. E)

getOperatingPeriods = function(f, jumpsize=600) {

  con = dbConnect(RSQLite::SQLite(), f)
  gps = dbGetQuery(con, "select * from gps order by ts")
  dbDisconnect(con)
  
  class(gps$ts) = c("POSIXt", "POSIXct")
  
  ## find breaks in timestamp longer than jumpsize seconds (default 10
  ## minutes) to indicate periods during which the SG was running

  jumps = diff(gps$ts) > jumpsize
  seg = cumsum(c(1, jumps))
  breaks = which(jumps)
  
  return(data.frame(from = gps$ts[c(1, breaks + 1)], to = gps$ts[c(breaks, nrow(gps))], meanLat=tapply(gps$lat,seg, mean), meanLon=tapply(gps$lon, seg, mean)))
}
