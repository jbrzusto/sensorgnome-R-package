## dta2sg.R: read, filter, and export a lotek .DTA file to a tags
## file compatible with sensorgnome data.

dta2sg = function(
  DTAfile,                                                              ## path to input file
  tagDB,                                                                ## path to public tag database file
  proj = readLines(file.path(dirname(DTAfile), "..", "PROJCODE.TXT")),  ## project code
  site = readLines(file.path(dirname(DTAfile), "SITECODE.TXT")),        ## site code
  outfile = sub("\\.dta$", "_filtered.rds", DTAfile, ignore.case=TRUE), ## output file
  confirm = 3,                                                          ## minimum bursts to confirm a run
  maxMiss = 20,                                                         ## maximum consecutive missing bursts in a run
  slop = 4,                                                             ## slop between measured and registered burst intervals, in milliseconds
  slopExpand = 0                                                        ## amount slop increases for each missed burst, in milliseconds
  ) {
  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)
  out = filterTags(dtaout, tagDB, confirm, maxMiss, slop, slopExpand)

  ## generate the SG-compatible dataframe:

  ## out$tags has these columns:
  ##  [1] "ts"        "ant"       "id"        "tagProj"   "runID"     "posInRun"  "sig"       "burstSlop" "DTAline"   "lat"       "lon"      
  ## [12] "antFreq"   "runLen"

  ## and we need these:

  ##  [1] "ant"       "ts"        "tagProj"   "tagID"     "freq"      "freqsd"   
  ##  [7] "sig"       "sigsd"     "noise"     "runID"     "posInRun"  "slop"     
  ## [13] "burstSlop" "hitRate"   "antFreq"   "tsOrig"    "bootnum"   "runLen"   
  ## [19] "id"        "lat"       "lon"       "alt"       "depYear"   "proj"     
  ## [25] "site"      "recv"      "fullID"   

  rv = data.frame(
    ant = out$tags$ant,
    ts = out$tags$ts,
    tagProj = out$tags$tagProj,
    tagID = out$tags$id,
    freq = NA,
    freqsd = NA,
    sig = out$tags$sig,
    sigsd = NA,
    noise = NA,
    runID = out$tags$runID,
    posInRun = out$tags$posInRun,
    slop = NA,
    burstSlop = out$tags$burstSlop,
    hitRate = NA,
    antFreq = out$tags$antFreq,
    tsOrig = out$tags$ts,
    bootnum = NA,
    runLen = out$tags$runLen,
    id = out$tags$id %% 1000,
    lat = out$tags$lat,
    lon = out$tags$lon,
    depYear = as.numeric(strftime(structure(min(out$tags$ts), class="POSIXct"), "%Y")),
    proj = proj,
    site = site,
    recv = out$recv,
    fullID = sprintf("%s#%3d@%6.3f", out$tags$tagProj, out$tags$id %% 1000, out$tags$antFreq)
    )

  saveRDS(rv, outfile)
  return(invisible(rv))
}

    
	
