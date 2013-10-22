createCookedDepDB = function(con, depName) {
  
  ## Create tables for processed data for a receiver deployment.

  ## This database contains data extracted from the raw receiver files,
  ## so will typically be smaller than the raw database.
  
  ## allow con to be a filename
  if (is.character(con)) {
    con = dbConnect("SQLite", con)
    on.exit(dbDisconnect(con))
  }

  sql = function(...) dbGetQuery(con, sprintf(...))

  sql("
create table deployment (
  depName text -- name of this deployment
)
")

  sql("insert into deployment values ('%s')", depName);
   
  sql("
create table receivers (
  recvID  integer not null primary key, -- receiver ID - used in other tables
  recv text                             -- receiver model / serial number
)
")
  
## a database of timepins mapping GPS timestamps to system timestamps, for periods when
## chrony has not updated the system clock (or when this fails entirely)
## this table should contain only one record per (depID, bootnum) pair

  sql("
create table timepins (  
  recvID    integer references receivers ( recvID ), -- receiver ID - NONUNIQUE FOREIGN KEY
  bootnum  integer,                                  -- boot number: number of times SG was booted before this file was recorded
  systs    double,                                   -- system timestamp for GPS fix
  gpsts    double                                    -- GPS timestamp from GPS fix
)");
  
  ##  If this turns out to be an issue for more receivers than just the one on the Ryan Leet, implement this
  
  ## A table of points when the time unexplainedly jumps forward to a ridiculous value (years in the future)
  ## This has been on the unit deployed on the ship Ryan Leet, for example.
  ## It seems rare, but we don't assume it only happens once per boot, so we try to detect
  ## any time the system clock jumps forward more than 1 day within a single file
  
  sql("
create   table timeJumps (  
  depID    integer references deployments ( depID ), -- deployment ID - NONUNIQUE FOREIGN KEY
  bootnum  integer,                                  -- boot number: number of times SG was booted before this file was recorded
  tsBefore double,                                   -- system timestamp before a big jump
  tsAfter  double                                    -- system timestamp after a big jump
)");
  
  sql("create unique index timeJumps_all on timeJumps ( depID, bootnum, tsBefore )")

  sql("
create table file_contents (  
  fileID   integer not null primary key,             -- file ID - used in most data tables
  contents BLOB                                      -- contents of file
)");
  
  sql("
create table gps (
  recvID   integer references receivers ( recvID ), -- ID of receiver
  ts      double,                              -- receiver timestamp for this record
  gpsts   double,                              -- gps timestamp for this record
  lat     double,                              -- latitude, decimal degrees
  lon     double,                              -- longitude, decimal degrees
  alt     double                               -- altitude, metres
)");
  
  sql("create index gps_depID on gps ( recvID )")
  sql("create index gps_ts on gps ( ts )")
  sql("create unique index gps_all on gps ( recvID, ts)")

  sql("
create table params (
  recvID   integer references receivers ( recvID ), -- ID of receiver
  ts      double,                              -- timestamp for this record
  tscode  character(1),                        -- timestamp code: 'P'=prior to GPS fix; 'Z' = after GPS fix
  port    integer,                             -- hub port -- for which device setting applies
  param   text,                                -- parameter name
  val     double,                              -- parameter setting
  error   integer,                             -- 0 if parameter setting succeeded; error code otherwise
  errinfo character                            -- non-empty if error code non-zero
)");

  sql("create index params_recvID on params ( recvID )")
  sql("create index params_ts on params ( ts )")
  sql("create unique index params_all on params ( depID, ts, tscode, port)")
  
  sql("
create table taghits (  
  port       integer,                                  -- hub port -- which device did this tag get identified on?
  ts         double,                                   -- timestamp of hit
  id        integer,                                   -- tag id (Lotek or other mfg)
  freq       double,                                   -- apparent tag frequency offset in kHz
  freqsd     double,                                   -- standard deviation of frequency offset for pulses in burst
  sig        double,                                   -- signal level in dBmax
  sigsd      double,                                   -- signal level SD, in percent
  noise      double,                                   -- noise level in db
  runID      integer,                                  -- id uniting hits in a run (presumably from same tag)
  posInRun   integer,                                  -- position of this tag hit in the run (perhaps omit; can be calculated by ranking timestamps)
  slop       double,                                   -- mean absolute difference of gap size between hit and database
  burstSlop  double,                                   -- mean absolute difference between most recent burst interval and database
  recvID     integer references receivers ( recvID ),  -- deployment ID - NONUNIQUE FOREIGN KEY
  tscode     character(1),                             -- timestamp code: 'P'=prior to GPS fix; 'Z' = after GPS fix
  bootnum    integer                                   -- which boot number is this tag hit from? (each power cycle increases it by 1)
)");

  sql("create index taghits_ts on taghits ( ts )")
  sql("create index taghits_recvID on taghits ( recvID )")
  sql("create index taghits_id on taghits ( id )")
  sql("create unique index taghits_all on taghits ( ts, port, id )")

}
