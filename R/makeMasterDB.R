## makeMasterDB.R
##
## create or append to or replace records in table "hits" of /SG/master.sqlite
##
## from all files in /SG/contrib/YEAR/PROJ/SITE/YEAR_PROJ_SITE_alltags.rds
##
## These are records of tag detections.
##


## fields called "key" or ending in "_key" are not meant to be
## user-visible; they serve only to link records among tables. Foreign
## key fields are given the singular name of the table followed by "_key"
##
## e.g. in table "tags", field "project_key" refers to field "key" in table "projects".

## table tags: one record per physical tag
## create table tags (
##   key          integer not null primary key,             -- unique key
##   year         integer,                                  -- year of purchase
##   project_key  integer references projects (key),        -- project deploying tag
##   mfg          character,                                -- manufacturer
##   model        character,                                -- model
##   id           character,                                -- manufacturer ID code
##   freq         double,                                   -- nominal frequency tag broadcasts on, in MHz
##   bi           double,                                   -- burst interval (or pattern repeat interval), in seconds
##   dfreq        double,                                   -- offset of base frequency from nominal frequency, in kHz
##   lifetime     integer                                   -- nominal tag lifetime, in days
##   );

## create table projects (
##   key        integer not null primary key,             -- unique key
##   code       character,                                -- short code name
##   desc       character,                                -- human readable description 
##   contact    character,                                -- name of primary contact person 
##   email      character                                 -- email address for primary contact
##   );

## create table receivers (
##   key          integer not null primary key,             -- unique key for single physical receiver (Lotek) or sensorgnome computer
##   mfg          character,                                -- manufacturer
##   model        character,                                -- model
##   serial       character,                                -- serial number/string
##   project_key  integer references projects (key),        -- project who owns receiver
##   year         integer                                   -- year receiver first went into service
## );

## create table radios (
##   key          integer not null primary key              -- unique key for single physical radio (e.g. funcubedongle, DVB stick, etc.)
##   mfg          character,                                -- manufacturer
##   model        character,                                -- model
##   serial       character,                                -- serial number/string
##   project_key  integer references projects (key),        -- project who owns receiver
##   year         integer                                   -- year receiver first went into service
##   );

## create table 
##   create table taghits (  
##   ant       character,                                  -- antenna number hub port -- which device did this tag get identified on?
##   ts         double,                                   -- timestamp of hit
##   id        integer,                                   -- tag id (Lotek or other mfg)
##   freq       integer,                                  -- apparent tag frequency offset in kHz
##   freqsd     integer,                                  -- standard deviation of frequency offset for pulses in burst
##   sig        double,                                   -- signal level in native units for receiver (dBmax for SG; 0..255 for Lotek)
##   sigsd      double,                                   -- signal level SD, in percent
##   noise      double,                                   -- noise level in db
##   runID      integer,                                  -- id uniting hits in a run (presumably from same tag)
##   posInRun   integer,                                  -- position of this tag hit in the run (perhaps omit; can be calculated by ranking timestamps)
##   slop       double,                                   -- mean absolute difference of gap size between hit and database
##   burstSlop  double,                                   -- mean absolute difference between most recent burst interval and database
##   recvID     integer references receivers ( recvID ),  -- deployment ID - NONUNIQUE FOREIGN KEY
##   tscode     character(1),                             -- timestamp code: 'P'=prior to GPS fix; 'Z' = after GPS fix
##   bootnum    integer                                   -- which boot number is this tag hit from? (each power cycle increases it by 1)
