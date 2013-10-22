createRawDepDB = function(con, depName) {
  
  ## Create tables for raw data for a receiver deployment.  Raw data
  ## consists of the contents of all files recorded during the
  ## deployment.  These are kept in a separate sqlite database from
  ## 'cooked' data, such as extracted GPS settings and tag detections,
  ## which will usually be much smaller.  The raw database should be
  ## archived after processing, but is not used again unless the user
  ## tries to re-extract tags with different parameters.

  ## A "deployment" is sequence of placements of an active
  ## receiver.  At any given time, the deployment consists of (at
  ## most) a single physical receiver, but the physical hardware in
  ## use might change from one placement to another, or even within a
  ## placement.  What matters is that all placements are related in
  ## some conceptual way, and that no more than a single receiver is
  ## active at any one time.
  ##
  ## e.g.:
  ##     - a single receiver set up at a single fixed site for a season
  ##     - a single receiver taken to various sites at various times, but
  ##       with a unifying purpose, e.g. "monitoring PIPL at southern NJ beaches"
  ##     - a single receiver installed on a moving platform
  ##     - a set of different receivers taken on visits to a single site,
  ##       with only one receiver per visit
  ##     
  
  ## allow con to be a filename
  if (is.character(con)) {
    con = dbConnect("SQLite", con)
    on.exit(dbDisconnect(con))
  }

  sql = function(...) dbGetQuery(con, sprintf(...))

  ## store the deployment name in a single celled table
  
  sql("
create table deployment (
  depName text -- name of this deployment
)
")

  sql("insert into deployment values ('%s')", depName);
    
  sql("
create table files (  
  fileID   integer not null primary key,             -- file ID - matches file_contents
  recv     text,                                     -- receiver name
  name     text unique,                              -- name of file (basename only; no path)
  size     integer,                                  -- size of file when read, in bytes
  bootnum  integer,                                  -- boot number: number of times receiver was booted before this file was recorded
  ts       double,                                   -- timestamp from filename (time at which file was created)
  tscode   character(1),                             -- timestamp code: 'P'=prior to GPS fix; 'Z' = after GPS fix
  tsDB     double,                                   -- timestamp when file was read into this database
  isDone   integer                                   -- if non-zero, this is a finished .gz file
)
")
  
  sql("create index files_name on files ( name ) -- fast lookup by filename")
  sql("create index files_depID_bootnum on files ( recv, bootnum ) -- fast lookup by receiver and bootnum")

sql("
create table file_contents (  
  fileID   integer not null primary key,             -- file ID - used in most data tables
  contents BLOB                                      -- contents of file
)"
      );
}
