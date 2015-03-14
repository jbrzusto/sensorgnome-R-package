chooseProject = function(p = NULL) {
    if (is.null(p)) {
        if (!exists("sensorgnome.project")) {
            projects = unique(read.csv(sensorgnome.dbfile, as.is=TRUE)$proj)
            library(tcltk)
            p = select.list(title="Which project is yours?", projects)
            if (length(p) == 0 || nchar(p[1]) == 0)
                stop("Cancelled")
        } else {
            p = sensorgnome.project
        }
    }
    sensorgnome.project <<- p
    return(p)
}
