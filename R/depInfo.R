## depInfo: find the deployment information associated with a tag at a given time
##

depInfo = function(proj, id, freq, ts, year = 2013, info="spcd") {
  ## e.g.  depInfo("Taylr", c(481, 481, 518, 518), 166.38, ymd(c("2013-06-01", "2013-08-17", "2013-04-16", "2013-08-19")), info=c("spcd", "age"))

  ## proj, id, freq are vectors identifying the tag; recycled as necessary

  ## ts is a vector of timestamps, of the same length as nrows(cbind(proj,id,freq))

  ## year is the year for deployment, indicating which database to use for lookup

  ## info is a character vector of column names desired from the deployment,
  ## chosen from:

  ##       proj tagFreq tagLife tagID gpsId pttId spcd bandNumber
  ##       startSite startLat startLong bandColour bandId wingTag age
  ##       sex mass billDepth culmen head tarsus wing tail startTS
  ##       stopTS
  
  ## returns a dataframe with the requested columns, and one row per input,
  ## except that the output row for any input tag/ts combination outside of
  ## known deployments is returned as all NA

  validCols = c("proj","tagFreq","tagLife","tagID","gpsId","pttId","spcd","bandNumber","startSite","startLat","startLong","bandColour","bandId","wingTag","age","sex","mass","billDepth","culmen","head","tarsus","wing","tail","startTS","stopTS","projIDFreq")

  if (any(is.na(match(info, validCols))))
    stop(paste("Invalid info columns selected; must be chosen from ", sprintf(validCols, collapse=", ")))
  
  library(RSQLite)
  con = dbConnect(RSQLite::SQLite(), sprintf("/SG/%d_tag_deployments.sqlite", year))

  ## create a temporary in-memory database to hold the temporary table for the join
  sql(con, "attach ':memory:' as mem")

  ## write the query labels and timestamps to a temporary db table for doing a join

  key = paste(proj, '#', id, '@', freq, sep="")
  dbWriteTable(con, "mem.temp", data.frame(key=I(key), ts=ts), row.names=FALSE, overwrite=TRUE)

  ## encode requested columns into the query

  outCols = paste(sprintf("tagDeps.%s as %s", info, info), collapse=",")
  
  ## do the join
  res = sql(con, "select mem.temp.rowID as i, %s from mem.temp left join tagDeps on mem.temp.key = tagDeps.projIdFreq where mem.temp.ts >= tagDeps.startTS and (mem.temp.ts <= tagDeps.stopTS or tagDeps.stopTS is null)", outCols);

  ## if the result has fewer rows than the input, we need to pad with NA rows

  if (nrow(res) == length(key)) {
    tmp = res[, -1] ## drop index column
  } else if (nrow(res) < length(key)) {
    ## get a single row from the tagDeps table, to act as a template
    tmp = sql(con, "select %s from tagDeps limit 1", outCols)
    ## make it NA
    tmp[1,] = NA

    ## replicate to the whole dataframe
    tmp = tmp[rep(1, length(key)),, drop=FALSE]

    ## fill in those rows for which dep info was found
    tmp[res$i,] = res[, -1]
  } else {
    ## hmmm. problem
    stop("More than one deployment record was found for some rows - problem in code or database?")
  }
  dbDisconnect(con)
  rownames(tmp) = NULL

  return (tmp)
}  
