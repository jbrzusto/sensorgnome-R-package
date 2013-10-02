chooseDBFile = function() {
  if (! exists("sensorgnome.dbfile")) {
    f = tclvalue(tkgetOpenFile(title="Choose the public tag database file for input",
      initialdir = ifelse(exists("sensorgnome.dtadir"), sensorgnome.dtadir, "."),
      filetypes="{{CSV files} {.csv}} {{All files} {.*}}"))
    if (length(f) == 0 || nchar(f)[1] == 0)
      stop("Cancelled")
  } else {
    f = sensorgnome.dbfile
  }
  sensorgnome.dbfile <<- f
  return(f)
}

