## dta2sg.R: read, filter, and export a lotek .DTA file to a tags
## file compatible with sensorgnome data.

dta2sg = function(
  DTAfile = NULL,   ## path to input file
  tagDB = NULL,     ## path to public tag database file
  proj = NULL,      ## project code
  site = NULL,      ## site code
  confirm = 3,      ## minimum bursts to confirm a run
  maxMiss = 20,     ## maximum consecutive missing bursts in a run
  slop = 4,         ## slop between measured and registered burst intervals, in milliseconds
  slopExpand = 0,   ## amount slop increases for each missed burst, in milliseconds
  split = TRUE      ## if TRUE, write separate files for tags from this and from all other projects
  ) {

  if (is.null(DTAfile)) {
    DTAfile = tclvalue(tkgetOpenFile(title="Choose a Lotek .DTA file for input",
       filetypes="{{Lotek DTA files} {.DTA}} {{All files} {.*}}"))
    if (length(DTAfile) == 0 || nchar(DTAfile)[1] == 0)
      return()
  }
  
  if (is.null(tagDB)) {
    tagDB = tclvalue(tkgetOpenFile(title="Choose the public tag database file for input",
       filetypes="{{CSV files} {.csv}} {{All files} {.*}}"))
    if (length(tagDB) == 0 || nchar(tagDB)[1] == 0)
      return()
  }
  
  if (is.null(proj)) {
    projects = unique(read.csv(tagDB, as.is=TRUE)$proj)
    proj = select.list(title="Which project is yours?", sprintf("%-30s", projects))
    if (length(proj) == 0)
      return()
  }
  
  if (is.null(site)) {
    cat("Please enter the site code for file ", basename(DTAfile), ":\n")
    site = readLines(n=1)
    if (length(site) == 0)
      return()
  }

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

  ## split output datasets between my and others' projects, or put all in the same file
  
  if (split) {
    outfile = sub("\\.dta$", "_filtered_my_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(subset(rv, tagProj == myProj), outfile)
    outfile2 = sub("\\.dta$", "_filtered_their_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(subset(rv, tagProj != myProj), outfile2)
    cat(sprintf("Wrote your tags to file:\n   %s\nand tags from other projects to file:\n%s\n", outfile, outfile2))
  } else {
    outfile = sub("\\.dta$", "_filtered_all_tags.rds", DTAfile, ignore.case=TRUE) ## output file
    saveRDS(rv, outfile)
    cat(sprintf("Wrote all tags (yours and other projets') to file:\n   %s\n", outfile))
  }

  return(invisible(rv))
}
