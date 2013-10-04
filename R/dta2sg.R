## dta2sg.R: read, filter, and export a lotek .DTA file to a tags
## file compatible with sensorgnome data.

dta2sg = function(
  DTAfile = chooseDTAFile(), ## path to input file
  tagDB = chooseDBFile(),    ## path to public tag database file
  myproj = chooseProject(),  ## project code
  site = NULL,               ## site code
  confirm = 3,               ## minimum bursts to confirm a run
  maxMiss = 20,              ## maximum consecutive missing bursts in a run
  slop = 4,                  ## slop between measured and registered burst intervals, in milliseconds
  slopExpand = 0,            ## amount slop increases for each missed burst, in milliseconds
  split = TRUE               ## if TRUE, write separate files for tags from this and from all other projects
  ) {

  force(DTAfile)
  force(tagDB)
  force(myproj)
  
  if (is.null(site)) {
    cat("Please enter the site code for file ", basename(DTAfile), ":\n")
    site = readLines(n=1)
    if (length(site) == 0)
      stop("Cancelled")
  }

  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)
  out = filterTags(dtaout, tagDB, confirm, maxMiss, slop, slopExpand)

  ## generate the SG-compatible dataframe:

  ## out$tags has these columns:
  ##  [1] "ts"        "ant"       "id"        "tagProj"   "runID"     "posInRun"  "sig"       "burstSlop" "dtaline"   "lat"       "lon"      
  ## [12] "antFreq"   "gain"      "runLen"

  ## and we need these:

  ##  [1] "ant"       "ts"        "tagProj"   "tagID"     "freq"      "freqsd"   
  ##  [7] "sig"       "sigsd"     "noise"     "runID"     "posInRun"  "slop"     
  ## [13] "burstSlop" "hitRate"   "antFreq"   "tsOrig"    "bootnum"   "runLen"   
  ## [19] "id"        "lat"       "lon"       "alt"       "depYear"   "proj"     
  ## [25] "site"      "recv"      "fullID"   "gain"

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
    proj = myproj,
    site = site,
    recv = out$recv,
    fullID = sprintf("%s#%3d@%6.3f", out$tags$tagProj, out$tags$id %% 1000, out$tags$antFreq),
    gain = out$tags$gain
    )

  class(rv$ts) = c("POSIXt", "POSIXct")
  rv$fullID = as.factor(rv$fullID)
  rv$ant = as.factor(rv$ant)
  ## split output datasets between my and others' projects, or put all in the same file
  
  if (split) {
    outfile = sub("\\.dta$", "_filtered_my_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(subset(rv, tagProj == myproj), outfile)
    outfile2 = sub("\\.dta$", "_filtered_their_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(subset(rv, tagProj != myproj), outfile2)
    cat(sprintf("Wrote your tags to file:\n   %s\nand tags from other projects to file:\n   %s\n", outfile, outfile2))
  } else {
    outfile = sub("\\.dta$", "_filtered_all_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(rv, outfile)
    cat(sprintf("Wrote all tags (yours and other projets') to file:\n   %s\n", outfile))
  }

  return(invisible(rv))
}
