chooseProject = function() {
  if (!exists("sensorgnome.project")) {
    projects = unique(read.csv(sensorgnome.dbfile, as.is=TRUE)$proj)
    p = select.list(title="Which project is yours?", projects)
    if (length(p) == 0 || nchar(p[1]) == 0)
      stop("Cancelled")
  } else {
    p = myProject
  }
  sensorgnome.project <<- p
  return(p)
}
  
  
