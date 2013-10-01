## filter tags using the filter_tags program
## returns a list with these items:
##   recv - receiver model and serial number
##   tags - filtered dataframe of tag detections

filterTags = function(x, tagDB,  confirm = 2, maxSkip = 20, slop = 20, slopExpand = 0) {
 ## tagDB must be a "public" version of the tag database, having only proj, id, freq, bi columns
  tmp = tempfile("dtaout")
  write.table(x$tags, tmp, row.names=FALSE, col.names=FALSE, sep=",")
  p = pipe(sprintf('./filter_tags -c %d -b %d -B %d -S %d "%s" "%s"',
    confirm, slop, slopExpand, maxSkip, tagDB, tmp))
  out = readLines(p)
  close(p)

  file.remove(tmp)

  out = read.csv(textConnection(out), as.is=TRUE)

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

