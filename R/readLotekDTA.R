
for (infilename in infilenames) {
  ## place where the file should go
  new.infilename = PATH$field_data(basename(infilename))

  if (!file.exists(new.infilename)) {
    if (!file.exists(infilename))
      stop("Lotek .DTA file does not exist: ", ARGS[4], "\nand could not be found in:", PATH$field_data(""))
    report("Copying data to project hierarchy:\nFrom: ", infilename, "\nTo: ", new.infilename, "\n")
    file.copy(infilename, new.infilename)
    binfilename = sub(".DTA$", ".bin", infilename)
    if (file.exists(binfilename)) {
      report("Copying binary data to project hierarchy:\nFrom: ", infilename, "\nTo: ", new.infilename, "\n")
      file.copy(binfilename, sub(".DTA$", ".bin", new.infilename))
    }
  }

  ## frequency and gain tables
  gain.tab = numeric()
  freq.tab = numeric()
  
  tagfile = PATH$registered_tags()

  report("Reading tags from ", tagfile, "\n")

  tags.data = read.csv(tagfile, as.is=TRUE)

  ## make ID-indexed vectors of tag gaps (in seconds) from the tags database
  g1 = g2 = g3 = rep(NA, max(tags.data$id))
  g1[tags.data$id] = tags.data$g1 / 1000
  g2[tags.data$id] = tags.data$g2 / 1000
  g3[tags.data$id] = tags.data$g3 / 1000

  basename = sub(".DTA$", "", new.infilename)

  report("Processing ", new.infilename, "\n")

  ## read the whole file as a string
  txt = readChar(new.infilename, nchar=file.info(new.infilename)$size, useBytes=TRUE)

  ## match against a regular expression to find tables
  res = gregexpr(paste(readLines("/BD/code/Lotek_DTA_regex.txt"), collapse="\r\n"), txt, perl=TRUE)[[1]]
  clen = t(attr(res, "capture.length"))
  cstart = t(attr(res, "capture.start"))
  parts = clen != 0
  pieces = substring(txt, cstart[parts], cstart[parts] + clen[parts] - 1)
  names(pieces) = rownames(clen)[1 + ((which(parts)-1) %% nrow(clen))]

  ## each element of the character vector pieces is the body of a table
  ## Interpret these now.

  noise.blank.level = 0

  for (ip in seq(along=pieces)) {
    piece.name = names(pieces)[ip] ## can be repeated

    report("Processing ", piece.name, " table with ")
    tab = read.table(textConnection(pieces[ip]), as.is=TRUE)
    report(nrow(tab), " records...\n")
    if (piece.name == "noise_blank_level") {
      noise.blank.level = tab[1,1]
    } else if (piece.name == "active_scan") {
      ## make a lookup table for frequency by channel
      freq.tab[as.character(tab[,1])] = tab[,2]
    } else if (piece.name == "antenna_gain") {
      ## make a lookup table for gain by antenna
      ## We use this to adjust observed power i.e. we reduce
      ## power by a factor of gain.  A brief lab trial
      ## showed an approximate increase of recorded signal
      ## strength of 40 units for each 10 unit increase in gain,
      ## so we reduce signal strength by 4 * gain before converting
      ## to dB way below.
      gain.tab[Lotekify(tab[,1])] = tab[,2]
    } else {
      ## this is a table of tag hits, either ID only or ID + GPS
      tab[1] = as.numeric(as.POSIXct(strptime(paste(tab[[1]], tab[[2]]), date.format, tz="GMT")))
      tab = tab[-2]
      if (piece.name == "id_only") {
        names(tab) = c("ts", "chan", "id", "ant", "sig")
        tab$lat = NA
        tab$long = NA
      } else if (piece.name == "id_gps") {
        names(tab) = c("ts", "chan", "id", "ant", "sig", "lat", "long")
        if (is.null(gps.outfile)) {
          gps.outfile = file(gps.outfilename, "w")
        }
        writeLines(sprintf(gps.file.format, tab$ts, tab$lat, tab$long), con=gps.outfile)
      }
      tab$ant = Lotekify(tab$ant)
      ants = unique(tab$ant)
      bad.ants = is.na(gain.tab[ants])
      if (any(bad.ants)) {
        ## the table refers to antenna for which we haven't seen a gain settings
        ## It appears that on the SRX-DL, if there are two antennas,
        ## and one of them is AH0, that antenna appears as "2" in tag hit records
        ## "AH0" has already been converted to 10, so check for this situation:
        if (sum(bad.ants) == 1 && !is.na(gain.tab[10]) && all (tab$ant != 10)) {
          ## use the gain for AH0, as this is presumably the same antenna
          gain.tab[ants[bad.ants]] = gain.tab[10]
        } else {
          ## use the gain from the first antenna with known gain, otherwise, use
          ## 80 as this was common
          if (all(is.na(gain.tab))) {
            gain.to.use = 80
            gain.source = " but NO GAIN VALUES WERE SPECIFIED FOR ANY ANTENNA!"
          } else {
            gain.to.use = gain.tab[which(!is.na(gain.tab))[1]]
            gain.source = paste(" specified for antenna '", names(gain.tab)[which(!is.na(gain.tab))[1]], "'", sep="")
          }
          gain.tab[ants[bad.ants]] = gain.to.use
          report("Warning: No gain setting found for antenna number(s): ", paste(deLotekify(ants[bad.ants]), collapse=", "),
                 " specified in ", piece.name, " table.\nUsing gain value of ", gain.to.use, gain.source, "\n")
        }
      }
        
      tab$ant.freq = freq.tab[as.character(tab$chan)]
      tab$sig = lotek.power.to.dB(tab$sig, gain.tab[tab$ant])
      ## drop 999 tags
      bad = tab$id == 999
      if (sum(bad) > 0) {
        report(sprintf("Dropping %d records with ID 999\n", 
                       sum(bad)))
        tab = tab[!bad,]
      }

      ## drop unregistered tags
      unreg = is.na(g1[tab$id])
      num.unreg = sum(unreg)
      if (num.unreg > 0) {
        report(sprintf("Warning: dropping %d records with these unregistered tag IDs: %s\n",
                       num.unreg,
                       paste(sort(unique(tab$id[unreg])), collapse=", ")))
        tab = tab[!unreg, ]
      }

      ## if user has specified timestamp fixes, apply these
      if (exists("file.ts.fixes")) {
        for (i in seq(along=file.ts.fixes$name.match)) {
          if (grepl(file.ts.fixes$name.match[i], infilename)) {
            dt = diff(as.numeric(as.POSIXct(
              strptime(c(file.ts.fixes$bad.date.pin[i], file.ts.fixes$good.date.pin[i]),
                       "%Y-%b-%d %H:%M:%OS", "UTC"))))
            tab$ts = tab$ts + dt
            break
          }
        }
      }

      ## for each tag burst, create timestamps for pulses 2, 3, and 4
      tab$ts2 = tab$ts  + g1[tab$id]
      tab$ts3 = tab$ts2 + g2[tab$id]
      tab$ts4 = tab$ts3 + g3[tab$id]

      ## add a noise column
      tab$noise = lotek.power.to.dB(noise.blank.level,  gain.tab[tab$ant])
      
      ## get unique antennas with data in this block
      ants = unique(tab$ant)

      all.antnos = unique(c(all.antnos, ants))
      
      ## for each antenna in the block, process its data separately
      for (ant in ants) {
        report("Working on data from antenna ", ant, "...")
        if (is.na(outfilenames[ant])) {
          LANE = paste("antenna", ant, "data", sep="_")
          outfilenames[ant] = PATH$antenna_pulses(LANE)
          outfiles[[ant]] = file(outfilenames[ant], "w")
          freq.outfilenames[ant] = PATH$antenna_freq(LANE)
          freq.outfiles[[ant]] = file(freq.outfilenames[ant], "w")
        }
        atab = tab[tab$ant==ant,]
        i.freqchange = c(1, 1 + which(diff(atab$ant.freq) != 0))
        writeLines(sprintf(freq.file.format, atab$ts[i.freqchange], atab$ant.freq[i.freqchange]), con=freq.outfiles[[ant]])

        ## for all tag hits, generate pulses
        writeLines(sprintf(pulse.file.format, c(t(atab[c("ts", "ts2", "ts3", "ts4")])), rep(atab$sig, each=4), rep(atab$noise, each=4)), con=outfiles[[ant]])
        report(nrow(atab), " hits.\n")
      }
    }
  }
}
