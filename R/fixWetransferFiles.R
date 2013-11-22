## if sensorgnome data files are saved in the root directory of a FAT drive, the long
## filenames sometimes get mangled into 8+3 names.  This function restores original
## names, provided there are at least some original names in the folder.

fixMangledFilenames = function(dir = ".", dryRun = FALSE) {
  ##
  ## read in funny names
  ##
  mang = dir(path = dir, pattern = "^.{6}~[0-9].GZ$", full.names=TRUE)

  ##
  ## hopefully, find at least one valid name
  ##
  okay = dir(path = dir, pattern = "^[A-Za-z0-9]+-[0-9A-F]+-[0-9]{6}-")

  if (length(okay) == 0)
    stop("Can't fix mangled names - there don't  appear to be any valid ones in the same directory")

  parts = strsplit(basname(okay[1]), "-", fixed=TRUE)

  newName = function(oldName) {
    ts = structure(read.csv(textConnection(readLines(f)[1]), header=FALSE)[1,2], class=c("POSIXt", "POSIXct"))
    file.path(dirname(okay[1]), sprintf("%s-%s-%s-%s-%s", parts[1], parts[2], parts[3], strftime (ts, "%Y-%m-%dT%H-%M-%OS4Z"), paste(parts[-(1:8)], collapse="-")))
  }

  f = if (dryRun) print else system
  
  for (m in mang)
    f(sprintf("mv '%s' '%s'", m, newName(m)))
}
