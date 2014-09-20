## filter tags using the filter_tags program
## returns a list with these items:
##   recv - receiver model and serial number
##   tags - filtered dataframe of tag detections

filterTags = function(x, tagDB,  confirm = 2, maxMiss = 20, slop = 20, slopExpand = 0) {
 ## tagDB must be a "public" version of the tag database, having only proj, id, freq, bi columns
  tmp = tempfile("dtaout")
  write.table(x$tags[!is.na(x$tags[,1]),], tmp, row.names=FALSE, col.names=FALSE, sep=",")
  exefile = file.path(path.package("sensorgnome"), "bin", "filter_tags")
  tmp2 = tempfile("filterout")
  if (.Platform$OS.type != "unix")
    exefile = paste(exefile, ".exe", sep="")  
  system(sprintf('%s -c %d -b %d -B %d -S %d "%s" "%s" > %s',
    exefile, confirm, slop, slopExpand, maxMiss, tagDB, tmp, tmp2))

  file.remove(tmp)

  fail = FALSE
  if (! file.exists(tmp2)) {
      fail = TRUE
  } else {
      out = read.csv(tmp2, as.is=TRUE)
      if (nrow(out) == 0)
          fail = TRUE
  }

  if (fail) {
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
  file.remove(tmp2)
  
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

