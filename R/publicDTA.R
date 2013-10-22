## extract the public tag records from a DTA file
publicDTA = function(
  DTAfile = NULL, ## path to input file
  tagDB = NULL,   ## path to public tag database file
  myproj = NULL,  ## project code
  site = NULL     ## site code
) {

  DTAfile = chooseDTAFile(DTAfile)
  tagDB = chooseDBFile(tagDB)
  depYear = substr(tagDB, 1, 4)
  myproj = chooseProject(myproj)
  
  if (is.null(site)) {
    cat("Please enter the site code for file ", basename(DTAfile), ":\n")
    site = readLines(n=1)
    if (length(site) == 0)
      stop("Cancelled")
  }
  
  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)

  pubtags = read.csv(tagDB, as.is=TRUE)

  outfile = sub("\\.dta$", "_raw_public_tags.rds", DTAfile, ignore.case=TRUE) ## output file
  
  rv = list(recv=dtaout$recv, proj=myproj, site=site, tags=subset(dtaout$tags, id %in% pubtags$id))
  saveRDS(rv, outfile)

  cat(sprintf("Wrote public tag records to file %s\n", outfile))

  return(invisible(rv))
}
  
  
  
