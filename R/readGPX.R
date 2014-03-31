## read a GPX file and return trackpoints and waypoints as a data
## frame with these columns
##
## ts: timestamp  (GMT)
## lat: latitude  (degrees N)
## lon: longitude (degrees E)
## ele: elevation (metres ASL)
## lab: character: 'wpt' or the name of the track to which the point belongs

readGPX = function(file) {
  ns = c(d="http://www.topografix.com/GPX/1/1")
  doc = xmlParse(file)

  ## count waypoints and trackpoints
  n = length(getNodeSet(doc, "//d:trkpt|//d:wpt", namespaces = ns))

  ## create return dataframe
  rv = data.frame(ts = numeric(n), lat = numeric(n), lon = numeric(n), ele = numeric(n), lab=I(character(n)))
  
  lab = "" ## filled in by <trk> element
  i = 1
  
  xpathApply(doc,
             "//d:trkpt|//d:wpt|//d:trk",
             
           function(n) {
             nm = xmlName(n)
             if (nm == "trk") {
               lab <<- xmlValue(xmlChildren(n)[["name"]])
             } else if (nm == "wpt" || nm == "trkpt") {
               rv$ts[i] <<- as.POSIXct(xmlValue(xmlChildren(n)[["time"]]), format="%Y-%m-%dT%H:%M:%S")
               attr = as.numeric(xmlAttrs(n))
               rv$lat[i] <<- attr[1]
               rv$lon[i] <<- attr[2]
               rv$ele[i] <<- as.numeric(xmlValue(xmlChildren(n)[["ele"]]))
               rv$lab[i] <<- if (nm == "wpt") "wpt" else lab
               i <<- i + 1
             }
           }
             , namespaces = ns
           )
  class(rv$ts) = c("POSIXt", "POSIXct")
  return(rv)
}
