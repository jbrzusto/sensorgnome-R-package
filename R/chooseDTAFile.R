chooseDTAFile = function(f) {
  if (is.null(f) || !file.exists(f)) {
    f = tclvalue(tkgetOpenFile(title="Choose a Lotek .DTA file for input",
      initialdir = ifelse(exists("sensorgnome.dtadir"), sensorgnome.dtadir,"."),
      filetypes="{{Lotek DTA files} {.DTA}} {{All files} {.*}}"))
    if (length(f) == 0 || nchar(f)[1] == 0)
      stop("Cancelled")
  }
  sensorgnome.dtadir <<- dirname(f)
  return(f)
}

