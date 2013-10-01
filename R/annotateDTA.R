## annotate a DTA file with information from the tag filtering process
## write the output to the same file with a ".txt" afterward

annotateDTA = function(DTAfile, tagDB, show=TRUE, confirm = 2, maxSkip = 20, slop = 20, slopExpand = 0) {
  dtalines = readLines(DTAfile)
  dtaout = readDTA(lines=dtalines)
  out = filterTags(dtaout, tagDB, confirm, maxSkip, slop, slopExpand)
  prefix = sprintf('<span class="line%d">', seq(along=dtalines) %% 2)
  suffix = '</span>'
  extra = rep("", length(dtalines))
  extra[out$tags$DTAline] = sprintf(" Run %6d: %6d/%6d  Proj: %s", out$tags$runID, out$tags$posInRun, out$tags$runLen, out$tags$tagProj)
##  cat(paste(dtalines, extra, sep="", collapse="\r\n"), file=paste(DTAfile, ".lines", sep=""))
  htmlname = paste(DTAfile, ".html", sep="")
  html = file(htmlname, "w")
  cat(sprintf('
<html>
  <head>
    <title>%s filtered by code from sensorgnome.org
    </title>
    <style>
.DTA_annotation { color: rgb(0, 200, 0); }
.line0 { background-color: rgb(225, 255, 200);}
.line1 { background-color: rgb(200, 255, 225);}
    </style>
  </head>
  <body>
  This annotated Lotek .DTA file was created by annotateDTA() in R package SG,<br>
  from the <a href="http://sensorgnome.org">sensorgnome.org</a> project.<br>
  <h3>Filtering Parameters</h3>
  <pre>
  tag database =  %s
       confirm =  %4d  : minimum number of bursts required in a run; need not be consecutive
       maxSkip =  %4d  : maximum number of consecutive skipped bursts
          slop =  %6.1f: maximum deviation from registered burst interval between consecutive bursts, in milliseconds
    slopExpand =  %4d  : amount permitted deviation increases for each skipped burst, in milliseconds
  </pre>
  Filtering is performed by looking for maximal runs of each tag ID in the database.
  The bursts in a run must satisfy the filtering parameters.  In particular, the time gap
  between them must be reasonably compatible with the burst interval in the database.
',
              basename(DTAfile),
              basename(tagDB),
              confirm,
              maxSkip,
              slop,
              slopExpand)
      , file=html)
  
  extra[out$tags$DTAline] = sprintf(' <span class="DTA_annotation">Slop: %5.1f ms  Run %6d: %6d/%6d  Proj: %s</span>',
         out$tags$burstSlop * 1000,
         out$tags$runID,
         out$tags$posInRun,
         out$tags$runLen,
         out$tags$tagProj)

  ## class names of file pieces
  pn = names(dtaout$pieces)

  ## numbered class names of file pieces
  pn2 = rep("", length(pn))
  tapply(seq(along=pn), pn, function(i) pn2[i] <<- sprintf("%s_%d", pn[i], 1:length(i)))

  ## a back arrow link for each table piece
  
  prefix[dtaout$piece.lines.before] = paste(prefix[dtaout$piece.lines.before],
          sprintf('<a name="%s"></a><br><a href="#TOC">Back to TOC</a><br>', pn2), sep="")
          
  cat('
<h3>
<a name="TOC">Table of Contents</a>
</h3>
<ul>\n', file=html)
          
  tapply(seq(along=pn), pn,
         function(i) {
           cat(paste(sprintf("<li>%s: %s</li>", pn[i][1],
                             paste(sprintf('<a href="#%s">%d</a>', pn2[i], seq(along=i)), collapse=", ")
                             ), collapse="\n"),
               file=html)})
  cat("</ul><hr><br><pre>\n", file=html)
  cat(paste(prefix, dtalines, extra, suffix, sep="", collapse="\r\n"), file=html)
  cat("</pre></body></html>\r\n", file=html)
  close(html)
  if (show) {
    if (substr(htmlname, 1, 1) != "/")
      htmlname = file.path(getwd(), htmlname)
    browseURL(paste("file://", htmlname, sep=""))
  }
}
