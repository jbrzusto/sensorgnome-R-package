## dta2sg.R: read, filter, and export a lotek .DTA file to a tags
## file compatible with sensorgnome data.

dta2sg = function(
  DTAfile = NULL, ## path to input file
  tagDB = NULL,   ## path to public tag database file
  myproj = NULL,  ## project code
  site = NULL,    ## site code
  confirm = 3,    ## minimum bursts to confirm a run
  maxMiss = 20,   ## maximum consecutive missing bursts in a run
  slop = 4,       ## slop between measured and registered burst intervals, in milliseconds
  slopExpand = 0, ## amount slop increases for each missed burst, in milliseconds
  split = TRUE    ## if TRUE, write separate files for tags from this and from all other projects
  ) {

  DTAfile = chooseDTAFile(DTAfile)
  tagDB = chooseDBFile(tagDB)
  myproj = chooseProject(myproj)
  
  if (is.null(site)) {
    cat("Please enter the site code for file ", basename(DTAfile), ":\n")
    site = readLines(n=1)
    if (length(site) == 0)
      stop("Cancelled")
  }
  
  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)
  out = filterTags(dtaout, tagDB, confirm, maxMiss, slop, slopExpand)
  if (is.null(out$tags))
      return(invisible(structure(list(), recv=dtaout$recv, tsRange=range(dtaout$tags$ts, na.rm=TRUE))))

  ## use the year of the first detection as the deployment year.
  rv = SGify(out$tags, as.numeric(strftime(structure(min(out$tags$ts), class="POSIXct"), "%Y")), myproj, site, dtaout$recv)
  
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
    cat(sprintf("Wrote all tags (yours and other projects') to file:\n   %s\n", outfile))
  }

  return(invisible(structure(rv, recv=dtaout$recv, tsRange=range(dtaout$tags$ts, na.rm=TRUE))))
}
