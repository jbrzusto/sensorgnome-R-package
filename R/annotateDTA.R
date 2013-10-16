## annotate a DTA file with information from the tag filtering process
## write the output to the same file with "_annotated.txt" appended,
## and optionally to an html file with a bit of navigation chrome.

annotateDTA = function(
  DTAfile,
  tagDB,
  show=TRUE,
  html=FALSE,
  confirm = 3,
  maxMiss = 20,
  slop = 4,
  slopExpand = 0) {

  DTAfile = chooseDTAFile(DTAfile)
  tagDB = choose(tagDB)
  
  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)
  out = filterTags(dtaout, tagDB, confirm, maxMiss, slop, slopExpand)
  
  extra = rep("", length(dtalines))
  extra[out$tags$DTAline] = sprintf(" Run %6d: %6d/%6d  Proj: %s", out$tags$runID, out$tags$posInRun, out$tags$runLen, out$tags$tagProj)
  txt = file(paste(DTAfile, "_annotated.txt", sep=""), "w")
  if (html) {
    htmlname = paste(DTAfile, "_annotated.html", sep="")
    htmlf = file(htmlname, "w")
    cat(sprintf('
<html>
  <head>
    <title>%s - filtered and annotated
    </title>
    <style>
.DTA_annotation { color: rgb(0, 200, 0); }
.line0 { background-color: rgb(225, 255, 200);}
.line1 { background-color: rgb(200, 255, 225);}
    </style>
  </head>
  <body>
    <pre>
', basename(DTAfile)), file=htmlf)
  }
  
  info = sprintf('
  This annotated Lotek .DTA file was created by annotateDTA() in R package sensorgnome
  from http://sensorgnome.org.

  Filtering Parameters
  ====================

  tag database =  %s

       confirm = %4d  : minimum number of bursts required in a run; need not be consecutive

       maxMiss = %4d  : maximum number of consecutive missed bursts; detected bursts
                         separated by more than this number of missing bursts are placed in separate runs.

          slop = %6.1f: maximum deviation from registered burst interval between consecutive bursts,
                         in milliseconds
    slopExpand = %4d  : amount permitted deviation increases for each missed burst, in milliseconds

  Filtering is performed by looking for maximal runs of each tag ID in the database.
  The bursts in a run must satisfy the filtering parameters.  In particular, the time gap
  between them must be reasonably compatible with the burst interval in the database.

  In the ID only and ID + GPS sections below, detections surviving the filtering are grouped
  into runs, which are indicated to the right of the original table.  The annotation
  looks like this:

      Slop:  -1.6 ms  Run      1:      3/   263  Proj: LHSP2013

  where:

     - Slop is the difference between the observed burst interval and the one registered
            in the database.

     - Run is the run identifier; detections with the same run ID are linked together
            by compatible time gaps.

     - i/N  the index and total number of detections in this run; here, the 
            detection is the third out of 263 for run #1.

     - Proj is the inferred project for this tag, based on antenna frequency and
            burst interval.  Tags having the same ID but different burst interval
            can be distinguished, and are placed in different runs.  The project code
            is from the master database.
',
    basename(tagDB),
    confirm,
    maxMiss,
    slop,
    slopExpand)

  cat(info, file=txt)
  if (html) {
    cat (info, file=htmlf)
  }

  annotation = sprintf(' Slop: %5.1f ms  Run %6d: %6d/%6d  Master ID: %4d  Proj: %s',
    out$tags$burstSlop * 1000,
    out$tags$runID,
    out$tags$posInRun,
    out$tags$runLen,
    out$tags$id,
    out$tags$tagProj)
  extra[out$tags$DTAline] = paste('<span class="DTA_annotation">', annotation, '</span>', sep="")

  if (html) {
    prefix = sprintf('<span class="line%d">', seq(along=dtalines) %% 2)
    suffix = '</span>'

    ## class names of file pieces
    pn = names(dtaout$pieces)
    
    ## numbered class names of file pieces
    pn2 = rep("", length(pn))
    tapply(seq(along=pn), pn, function(i) pn2[i] <<- sprintf("%s_%d", pn[i], 1:length(i)))
    
    ## a back to TOC link for each table piece
    
    prefix[dtaout$piece.lines.before] = paste(prefix[dtaout$piece.lines.before],
            sprintf('<a name="%s"></a><br><a href="#TOC">Back to TOC</a><br>', pn2), sep="")

    cat('
</pre><h3>
<a name="TOC">Table of Contents</a>
</h3>\n', file=htmlf)

    
    tapply(seq(along=pn), pn,
           function(i) {
             cat(paste(sprintf("<li>%s: %s</li>", pn[i][1],
                               paste(sprintf('<a href="#%s">%d</a>', pn2[i], seq(along=i)), collapse=", ")
                               ), collapse="\n"),
                 file=htmlf)})
    cat("</ul><hr><br><pre>\n", file=htmlf)
    cat(paste(prefix, dtalines, extra, suffix, sep="", collapse="\r\n"), file=htmlf)
    cat("</pre></body></html>\r\n", file=htmlf)
    close(htmlf)
  }

  ## bare text version
  
  dtalines[out$tags$DTAline] = paste(dtalines[out$tags$DTAline], annotation)
  cat(paste(dtalines, collapse="\r\n"), file=txt)
  close(txt)

  ## browse to the annotated HTML version
  if (html && show) {
    if (.Platform$OStype == "unix" && substr(htmlname, 1, 1) != "/")
      htmlname = file.path(getwd(), htmlname)
    browseURL(paste("file:///", htmlname, sep=""))
  }
}
