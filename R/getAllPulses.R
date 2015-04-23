##
##
## FIXME: modify the logic from findtags.R
##
## getAllPulses.R - from a datafile summary database for a receiver,
## create a dataframe of all detected pulses,
## with the following fields:
##   proj,ant,ts,dfreq,sig,noise,lat,lon,alt,freq
##   
##   where ant is 1..7
##   ts is timestamp in seconds, GMT
##   freq is in MHz, and is the sum of tuner frequency and offset frequency
##   sig and noise are in db

getAllPulses = function(dbfile) {
  options(digits=14)
  
  if (! grepl(".sqlite$", dbfile, ignore.case=TRUE))
    dbfile = paste(dbfile, ".sqlite", sep="");
  
  con = dbConnect(RSQLite::SQLite(), dbfile)
    
  tables = dbListTables(con)

  if (! all (c("deployments", "files", "gps", "params") %in% tables))
    stop("missing one or more required tables - run sgdb.R to create tables and import data")

  deps = sql(con, "select * from deployments")

  sql(con, "drop table if exists pulses")
  sql(con,
      "CREATE TABLE pulses (
recv TEXT,
proj TEXT,
ant TEXT,
ts REAL,
dfreq REAL,
sig REAL
noise REAL,
lat REAL,
lon REAL,
freq REAL
);")
  
  for (d in seq(along=deps$depID)) {
    ## dataframe of files for this deployment
    files = sql(con, "select fileID,name from files where files.depID = %d order by files.ts", deps$depID[d])
    ## gps hits
    gps.hits = NULL
    
    ## freq settings
    freq.set = NULL

    n = nrow(files)
    i = 1
    while (i <= n) {
      if (i < n && files$name[i+1] == paste(files$name[i], ".gz", sep="")) {
        ## have both .txt and .txt.gz
        gzLines = character(0)
        try( {
          ## protect against partial file
          fcon = sql(con, "select contents from file_contents where fileID=%d", files$fileID[i+1])
          rc = rawConnection(fcon$contents[[1]])
          gzc = gzcon(rc)  
          gzLines = readLines(gzc)
          close(gzc)
          
        }, silent=TRUE)
        if (length(gzLines) > 0) {
          ## use the gz version
          txtLines = gzLines
        } else {
          fcon = sql(con, "select contents from file_contents where fileID=%d", files$fileID[i])
          tc = textConnection(rawToChar(fcon$contents[[1]]))
          txtLines = readLines(tc)
        }          
        i = i + 2
      } else {
        if (grepl(".txt$", files$name[i])) {
          fcon = sql(con, "select contents from file_contents where fileID=%d", files$fileID[i])
          tc = textConnection(rawToChar(fcon$contents[[1]]))
          txtLines = readLines(tc)
          close(tc)
        } else {
          fcon = sql(con, "select contents from file_contents where fileID=%d", files$fileID[i])
          rc = rawConnection(fcon$contents[[1]])
          gzc = gzcon(rc)  
          txtLines = readLines(gzc)
          close(gzc)
        }
        i = i + 1
      }
      bad <<- FALSE
      ## get single letter code for line type
      tryCatch({
        code = substr(txtLines, 1, 1)
      }, error=function(e) {
        bad <<- TRUE
      })
      if (bad)
        next
      if (any(code == 'p')) {
        tryCatch({
          pulses = read.csv(textConnection(paste(substring(txtLines[code=='p'], 2), collapse="\n")), as.is=TRUE, header=FALSE)
        }, error = function(e) {
          bad <<- TRUE
        })
        if (bad || ncol(pulses) != 5)
          next
        names(pulses) = c("ant", "ts", "dfreq", "sig", "noise")
      } else {
        pulses = NULL
      }
      if (any(code == 'G')) {
        gps = read.csv(textConnection(paste(substring(txtLines[code=='G'], 3), collapse="\n")),  as.is=TRUE, header=FALSE)
        names(gps) = c("ts", "lat", "lon", "alt")
        gps.hits = rbind(gps.hits, gps)
      }
      gps.hits = gps.hits[is.finite(gps.hits$ts),]
      if (! is.null(pulses)) {
        if (! is.null(gps.hits) && nrow(gps.hits) > 0) {
          pulses$lat = spline(gps.hits$ts, gps.hits$lat, method="natural", xout=pulses$ts)$y
          pulses$lon = spline(gps.hits$ts, gps.hits$lon, method="natural", xout=pulses$ts)$y
          pulses$alt = spline(gps.hits$ts, gps.hits$alt, method="natural", xout=pulses$ts)$y
        } else {
          pulses$lat = NA
          pulses$lon = NA
          pulses$alt = NA
        }
      }

      if (any(code == 'S')) {
        settings = read.csv(textConnection(paste(substring(txtLines[code=='S'], 3), collapse="\n")),  as.is=TRUE, header=FALSE)
        names(settings) = c("ts", "ant", "param", "value", "rc", "error")
        freq.set = rbind(freq.set, settings)
      }
      if (! is.null(pulses)) {
        if (! is.null(freq.set) > 0) {
          freq = list()
          for (a in unique(freq.set$ant)) {
            ff = subset(freq.set, ant==a)
            freq[[a]] = approxfun(ff$ts, ff$value, method="constant", f=0, rule=1:2)
          }
          pulses$freq = 0
          for (a in unique(pulses$ant)) {
            use = which(pulses$ant==a)
            pulses$freq[use] = freq[[a]](pulses$ts[use]) + pulses$dfreq[use]/1000
          }
        } else {
          pulses$freq = NA
        }
      }     
      if (!is.null(pulses)) {
        pulses = cbind(bone=deps$recv[d],proj=deps$proj[d], pulses)
        dbWriteTable(con, "pulses", pulses, row.names=FALSE, append=TRUE)
      }
    }
  }
  dbDisconnect(con)
}
