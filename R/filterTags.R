## filter tags using the filter_tags program
## returns a list with these items:
##   recv - receiver model and serial number
##   tags - filtered dataframe of tag detections

filterTags = function(x, tagDB,  confirm = 2, maxMiss = 20, slop = 20, slopExpand = 0) {
 ## tagDB must be a "public" version of the tag database, having only proj, id, freq, bi columns
  tmp = tempfile("dtaout")
  write.table(x$tags[!is.na(x$tags[,1]),], tmp, row.names=FALSE, col.names=FALSE, sep=",")
  exefile = file.path(path.package("sensorgnome"), "bin", "filter_tags")
  if (.Platform$OS.type != "unix")
    exefile = paste(exefile, ".exe", sep="")  
  p = pipe(sprintf('%s -c %d -b %d -B %d -S %d "%s" "%s"',
    exefile, confirm, slop, slopExpand, maxMiss, tagDB, tmp))
  out = readLines(p)
  close(p)

  file.remove(tmp)

  out = read.csv(textConnection(out), as.is=TRUE)

  if (nrow(out) == 0) {
    stop(sprintf(
"There were no runs of tags found in this file under the current filtering criteria:
   confirm = %d
   maxMiss = %d
   slop = %.2f ms
   slopExpand = %.2f ms
   database = %s
",
      confirm, maxMiss, slop, slopExpand, basename(tagDB)))
  }
  
  ## re-number runs in order from 1
  out$runID = as.numeric(as.factor(out$runID))

  ## sort in increasing time
  out = out[order(out$ts),]
  
  ## add a run length column
  runLen = rep(0, nrow(out))
  invisible(tapply(1:nrow(out), out$runID, function(i) runLen[i] <<- length(i)))
  out$runLen = runLen

  return(list(recv=x$recv, tags=out))
}

