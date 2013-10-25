## lookupSpecies: find the species code associated with a tag deployment
##
##

lookupSpecies = function(proj, id, freq, ts, year = 2013, ignoreStop = TRUE) {
  ## proj, id, freq are vectors identifying the tagLabel is like the "label" field in the master tag database
  ## e.g. "Dosmn#36@166.38:7.6"  i.e. PROJ#ID@FREQ:BI, with BI rounded to 0.1 seconds
  ## and ID not zero-padded, and FREQ in MHz

  ## tagLabel and ts should have the same length; ts is numeric timestamp

  ## additionally: if ignoreStop is TRUE, ignore stop times; just return the species
  ## code associated with the last startTime at or before the given ts

  ## year is the tag deployment year (indicates which tag_deployment database to use)
  
  ## returns a character vector:

  ##  - non-empty string: the species code for that tag at that time, if tag was active
  ##  - NA: the tag was not deployed on an organism at that time
  ##  - empty string: the tag was deployed, but species code has not been registered


  library(RSQLite)
  con = dbConnect("SQLite", sprintf("/SG/%d_tag_deployments.sqlite", year))

  ## write the query labels and timestamps to a temporary db table for doing a join

  key = paste(proj, '#', id, '@', freq, sep="")
  dbWriteTable(con, "temp", data.frame(key=I(key), ts=ts), row.names=FALSE, overwrite=TRUE)

  ## do the join
  res = dbGetQuery(con, "select temp.rowID as i, tagDeps.spcd as sp from temp left join tagDeps on temp.key = tagDeps.projIdFreq where temp.ts >= tagDeps.startTS and (temp.ts <= tagDeps.stopTS or tagDeps.stopTS is null)");

  ## drop the temporary table
  dbGetQuery(con, "drop table temp")

  ## res has two columns i, sp
  ## i is an index into the input vectors

  sp = rep(NA, length(key))

  ## assign those species codes found
  sp[res$i] = res$sp

  dbDisconnect(con)

  return (sp)
}  
