## read a WPX file and return waypoints as a data frame with these columns
##
## ts: timestamp  (GMT)
## lat: latitude  (degrees N)
## lon: longitude (degrees E)
## ele: elevation (metres ASL)

readWPX = function(file) {
  root = xmlRoot(xmlTreeParse(file))
  n = sum(names(root) == "wpt")
  rv = data.frame(ts = numeric(n), lat = numeric(n), lon = numeric(n), ele = numeric(n))
  i = 1
  xmlApply(root,
           function(n) {
             if (xmlName(n) == "wpt") {
               rv$ts[i] <<- as.POSIXct(xmlValue(xmlChildren(n)[["time"]]), format="%Y-%m-%dT%H:%M:%S")
               attr = as.numeric(xmlAttrs(n))
               rv$lat[i] <<- attr[1]
               rv$lon[i] <<- attr[2]
               rv$ele[i] <<- as.numeric(xmlValue(xmlChildren(n)[["ele"]]))
               i <<- i + 1
             }
           })
  class(rv$ts) = c("POSIXt", "POSIXct")
  return(rv)
}
