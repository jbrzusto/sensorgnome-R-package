##
## makeRecvStatusDB.R
##
## - for every .sqlite database of raw data from every receiver and every project,
##   generate a status record for each file written by the receiver, at a granularity
##   of 10 minutes or larger.
##
## The database is called /SG/YEAR/YEAR_receiver_status.sqlite
##
## proj - name of project
## site - name of site (within project)
## recv - receiver serial number
## ts - timestamp


makeRecvStatusDB = function(year, quiet=TRUE) {
  library(RSQLite)
  library(lubridate)
  files = getAllSiteFiles(year, ".sqlite")

  db = sprintf("/SG/%d_receiver_status.sqlite", year)
  con = dbConnect("SQLite", db)
  dbGetQuery(con, "drop table if exists status")
  dbGetQuery(con, "
create table status (
   proj text,
   site text,
   recv text,
   ts  double
);
")
  for (f in files) {
    if (grepl("_lotek", f)) {
      if (!quiet)
        cat("Skipping lotek file ", f, "\n")
      next
    }
    if (!quiet)
      cat("Processing file ", f, "\n")
    
    bn = basename(f)
    parts = strsplit(bn, "[_.]")[[1]]
    proj = parts[2] ## get project
    site = paste(parts[c(-1, -2, -length(parts))], collapse="_")  ## drop year, proj, 'sqlite'
    sql(con, "attach database '%s' as sitedb", f)
    ff = sql(con, "select '%s' as proj, '%s' as site, deployments.recv as recv, files.ts as ts from sitedb.files join sitedb.deployments on sitedb.files.depID=sitedb.files.depID where files.tscode = 'Z'", proj, site)
    sql(con, "detach database sitedb")
    if (nrow(ff) == 0)
      next
    ff = ff[! duplicated(round(ff$ts/600)),]
    dbWriteTable(con, "status", ff, append=TRUE, row.names=FALSE)
  }
  dbDisconnect(con)
}
