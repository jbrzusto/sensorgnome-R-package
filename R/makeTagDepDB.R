##
## makeTagDepDB.R : create an sqlite table with appropriate indices etc
##                  from the .csv file of tag deployments
##  e.g. /SG/2013_tag_deployments.csv --> /SG/2013_tag_deployments.sqlite

makeTagDepDB = function(year=2013) {
  library(lubridate)
  
  csv = sprintf("/SG/%d_tag_deployments.csv", year)
  db =  sprintf("/SG/%d_tag_deployments.sqlite", year)

  x = read.csv(csv, as.is=TRUE)

  ## convert date-by-parts into numeric timestamp

  mkts = function(prefix) {
    cols = paste(prefix, c("yr", "mo", "day", "hr", "min"), sep=".")
    for (c in cols)
      x[[c]][is.na(x[[c]])] = 0
    x[[paste(prefix, "TS", sep="")]] <<- ymd_hms(do.call(sprintf, c(list("%d-%02d-%02d %0d:%02d:00"), x[, cols ])))
    for (c in cols)
      x[[c]] <<- NULL
  }

  mkts("start")
  mkts("stop")

  ## convert "abc.def" to "abcDef"
  
  names(x) = sapply(names(x), function(n) {
    parts = strsplit(n, ".", fixed=TRUE)
    if (length(parts[[1]]) == 1)
      return(n)
    return(paste(parts[[1]][1], toupper(substr(parts[[1]][2], 1, 1)), substring(parts[[1]][2], 2), sep=""))
  })

  x$projIDFreq = paste(x$proj, '#', x$tagID, '@', x$tagFreq, sep="")
  
  ## write out the converted table to an sqlite database
  library(RSQLite)
  con = dbConnect("SQLite", db)
  dbWriteTable(con, "tagDeps", x, row.names=FALSE, overwrite=TRUE)

  ## add an index for fast lookup by the tagID, project, tagFreq triplet
  
  dbGetQuery(con, "create index tagDeps_projIDFreq on tagDeps (projIDFreq)")

  dbDisconnect(con)
}
