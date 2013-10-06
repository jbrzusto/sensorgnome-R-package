## look for records of unregistered tags in a Lotek .DTA file
## - compute a millisecond-resolution table of observed burst intervals
##   for each tag ID.
## - remove entries which are close to multiples of the registered burst
##   interval, if any
## - among remaining table entries, if any seem large, report them

findUnregTags = function(
  DTAfile = chooseDTAFile(), ## path to input file
  tagDB = chooseDBFile(),    ## path to public tag database file
  slop = 4                   ## slop for comparison to tag database BI
  ) {
  force(DTAfile)
  force(tagDB)

  x = subset(readDTA(DTAfile)$tags, id != 999)

  tt = tapply(seq(along=x$ts), list(x$antfreq, x$id),
    function(i) {
      t = table(round(diff(as.numeric(x$ts[i])), 3))
      return(cbind(x$id[i[1]], x$antfreq[i[1]], as.numeric(names(t)), as.numeric(t)))
    },
    simplify = FALSE
    )

  tt = tt[sapply(tt, ncol) == 4]
  tt = as.data.frame(do.call(rbind, tt))
  names(tt) = c("id", "antfreq", "dt", "count")

  ## reduce to sets of likely interest: tags with more than 2 gaps
  ## that are identical to the nearest millisecond, and for which
  ## those gaps are less than 30 seconds
  
  tt = subset(tt, dt < 30 & count > 2)

  ## remove gaps compatible with the database

  tdb = read.csv(tagDB, as.is=TRUE)

  keep = rep(TRUE, length(tt$id))
  for (i in seq(along=tt$id)) {
    ## known burst intervals for this tag ID
    bis = tdb$bi[tdb$id %% 1000 == tt$id[i] & tdb$tagFreq == tt$antfreq[i]]
    ebi = tt$dt[i]
    for (bi in bis) {
      if (abs(ebi - bi * round(ebi/bi)) < slop / 1000) {
        keep[i] = FALSE
        break
      }
    }
  }
  tt = tt[keep,]

  ## for remaining IDs, get mean, sd of apparent burst interval,
  ## assuming smallest value is the BI

  est = tapply (seq(along=tt$id), tt$id,
    function(i) {
      bn = round(tt$dt[i] / min(tt$dt[i]))
      vals = rep(tt$dt[i] / bn, tt$count[i])
      return (c(tt$antfreq[i[1]], mean(vals), sd(vals), sum(tt$count[i])))
    })

  ## drop ones for which BI sd is too high, or can't be estimated
  est = sapply(est,
    function(stats) {
      if (is.finite(stats[3]) && stats[3] < slop/1000)
        return(stats)
      else
        return(NULL)
    })
  
  est = est[sapply(est, function(x) ! is.null(x))]

  if (length(est) > 0) {
    edf = as.data.frame(cbind(as.numeric(names(est)), do.call(rbind, est)))
    names(edf) = c("id", "antfreq", "bi", "bisd", "count")
    choices = sprintf("Tag # %d @ %.3f MHz detected %d times; mean burst interval %.3f s +/- %.1f ms", edf$id, edf$antfreq, edf$count, edf$bi, edf$bisd * 1000)
    addToDB = match(select.list(title="Which tags would you like to add to the DB?", choices, multiple=TRUE), choices)
    if (length(addToDB) > 0) {
      sensorgnome.dbfile <<- tagDB
      proj = chooseProject()
      f = file(tagDB, "a")
      edf = edf[addToDB,]
      records = paste(sprintf('"%s",%d,%.3f,%f\n', proj, edf$id, edf$antfreq, edf$bi), collapse="")
      cat(records, file=f)
      close(f)
      cat("Appended records:\n",records,"\nto file ", tagDB, "\n")
      return(edf)
    } else {
      cat("Cancelled.\n")
      return()
    }
  }
  cat("No likely candidates for valid but unregistered tags found.\n")
}
  
