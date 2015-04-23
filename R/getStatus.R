##
## getStatus.R : return all status records for a specified receiver
##

getStatus = function(year, proj, site) {
  con = dbConnect(RSQLite::SQLite(), sprintf("/SG/%d_receiver_status.sqlite", year))
  rv = sql(con, "select * from status where site='%s' and proj='%s' order by ts", site, proj)
  if (nrow(rv) == 0) {
    if (sql(con, "select count(ts) from status where proj = '%s' limit 1", proj)[1,1] == 0) {
      vals = sql(con, "select distinct proj from status")$proj
      dbDisconnect(con)
      stop(paste("Not a valid project.  Must be one of:\n", paste(vals, collapse="\n")))
    }
    vals = sql(con, "select distinct site from status where proj='%s'", proj)$site
    dbDisconnect(con)
    stop(paste("Not a valid site for this project.  Must be one of:\n", paste(vals, collapse="\n")))
  }
  dbDisconnect(con)
  return(rv)
}
