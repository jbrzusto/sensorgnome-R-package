#' Convert a data.frame of Lotek tag data to a SensorGnome-compatible data.frame
#'
#' The output of filterDTA() is augmented with additional columns required by the SensorGnome
#' database format.
#'
#' @param tags a data.frame of Lotek tag data, as produced by readDTA() or filterDTA()
#'
#' @param year the deployment year.
#' 
#' @param recv the receiver model / serial number (character scalar)
#' 
#' @return A data.frame with the same number of rows, but in SensorGnome format
#' 
#' @author John Brzustowski \email{jbrzusto is at fastmail.fm}

SGify = function(tags, year, proj, site, recv) {
  ## parse components of the tag ID: project, id, frequency, and burst interval
  parts = regexpr(pattern="(?:(?<proj>[^#]+))(?:#)(?:(?<id>[^@]+))(?:@)(?:(?<freq>[^:]+))(?::)(?:(?<bi>.+))$", tags$id, perl=TRUE)

  tmp = attr(parts, "capture.start")[,"id"]
  id = substr(tags$id, tmp, attr(parts, "capture.length")[,"id"] + tmp - 1)
  
  tmp = attr(parts, "capture.start")[,"proj"]
  tagProj = substr(tags$id, tmp, attr(parts, "capture.length")[,"proj"] + tmp - 1)

  ## generate the SG-compatible dataframe:

  ## tags has these columns:
  ## [1] "ts"        "ant"       "id"        "runID"     "posInRun"  "sig"       "burstSlop" "DTAline"   "lat"       "lon"      
  ## [12] "antFreq"   "gain"     "runLen"
  ## where
  ## id looks like: Lorng#69@166.380:4.9
  ## i.e. PROJ#TAGID@FREQ:BI
  
  ## and we need these:

  ##  [1] "ant"       "ts"        "fullID"    "freq"      "freqsd"    "sig"      
  ##  [7] "sigsd"     "noise"     "runID"     "posInRun"  "slop"      "burstSlop"
  ## [13] "antFreq"   "tsOrig"    "bootnum"   "runLen"    "id"        "tagProj"  
  ## [19] "nomFreq"   "lat"       "lon"       "alt"       "depYear"   "proj"     
  ## [25] "site"      "recv"      "sp"        "label"
  
  ## where fullID is as id above

  rv = data.frame(
    ant = tags$ant,
    ts = tags$ts,
    fullID = tags$id,
    freq = NA,
    freqsd = NA,
    sig = tags$sig,
    sigsd = NA,
    noise = NA,
    runID = tags$runID,
    posInRun = tags$posInRun,
    slop = NA,
    burstSlop = tags$burstSlop,
    antFreq = tags$antFreq,
    tsOrig = tags$ts,
    bootnum = NA,
    runLen = tags$runLen,
    id = id,
    tagProj = tagProj,
    nomFreq = tags$antFreq,
    lat = tags$lat,
    lon = tags$lon,
    alt = NA,
    depYear = year,
    proj = proj,
    site = site,
    recv = recv,
    gain = tags$gain,
    dbm = lotekPowerTodBm(tags$sig, tags$gain)
    )

  class(rv$ts) = c("POSIXt", "POSIXct")
  rv$fullID = as.factor(rv$fullID)
  rv$ant = as.factor(rv$ant)

  return(rv)
}
