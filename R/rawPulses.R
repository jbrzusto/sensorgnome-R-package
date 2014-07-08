##
## rawPulses.R: retrieve raw pulse records for a given time interval at a site
## Pulses might be from different nominal antenna frequencies, if frequency
## switching is enabled.  TODO:  This information must be obtained separately
## from the params table in the .sqlite meta database for the site.

rawPulses = function(range, proj, site, year = lubridate::year(Sys.time())) {
    range = sort(as.numeric(range))

    tp = timePins(proj, site, year)
    tp = tp[order(tp$bootstart),]
    class(tp$systs) = class(tp$gpsts) = class(tp$bootstart) = "numeric"

    begin = tail(subset(tp, bootstart <= range[1]), 1)  ## timepin record for start of range
    end = tail(subset(tp, bootstart <= range[2]), 1)  ## timepin record for end of range

    ## the endpoints of the range are legitimate timestamps which will
    ## be either before or after the GPS time pin.  If before, we
    ## back-calculate the 2000-01-01-based date, which is what the raw
    ## file will use.  Otherwise, we use the date as-is, since raw
    ## file timestamps will have the correct date.
    
    if (range[1] < begin$gpsts)
        begin$filets = as.numeric(ymd("2000-01-01")) + range[1] - begin$bootstart
    else
        begin$filets = range[1]

    if (range[2] < end$gpsts)
        end$filets = as.numeric(ymd("2000-01-01")) + range[2] - end$bootstart
    else
        end$filets = range[2]

    ## for each endpoint of the range, find the latest dated raw file which is before the endpoint

    beginfile = siteSQL(sprintf("select * from files where bootnum=%d and ts <= %f order by ts desc limit 1", begin$bootnum, begin$filets), proj, site, year)
    endfile = siteSQL(sprintf("select * from files where bootnum=%d and ts <= %f order by ts desc limit 1", end$bootnum, end$filets), proj, site, year)

    files = siteSQL(sprintf("select * from files where fileID >= %d and fileID <= %d", beginfile$fileID, endfile$fileID), proj, site, year)
    class(files$ts)=c("POSIXt", "POSIXct")
    
    ## read pulse records from each file, correcting timestamps as we go

    rv = NULL
    for (i in 1:nrow(files)) {
        ## get file path
        f = file.path(sitePath(proj, site, year), sprintf("filed/%4d-%02d-%02d/%s", year(files$ts[i]), month(files$ts[i]), day(files$ts[i]), files$name[i]))
        ## read appropriate file type
        if (grepl("gz$", f))
            dat = readLines(gzfile(f))
        else
            dat = readLines(f)
        ## select only pulse records
        dat = grep("^p", dat, value=TRUE, perl=TRUE)
        dat = read.csv(textConnection(dat), header=FALSE, as.is=TRUE)

        ## do we need to back-correct times?  Not if the dates are > 2010 (the SG boots up at 2000)
        if (files$ts[i] < as.numeric(ymd("2010-01-01"))) {
            ## find the timePin for this bootnumber
            mytp = which(tp$bootnum == files$bootnum[i])

            if (length(mytp) != 1) {
                warning("Multiple or no timepins for bootnum=", files$bootnum[i], "; can't correct timestamps")
            } else {
                ## corect the times using the timePin for this boot cycle
                dat[2] = dat[2] + (tp$gpsts[mytp] - tp$systs[mytp])
            }            
        }
            
        rv = rbind(rv, dat)
    }
    names(rv) = c("ant", "ts", "freq", "sig", "noise")
    class(rv$ts) = c("POSIXt", "POSIXct")

    ## filter out extra pulses from start of first file and end of last
    
    rv = subset(rv, ts >= range[1] & ts <= range[2])
    
    return(rv)
}
