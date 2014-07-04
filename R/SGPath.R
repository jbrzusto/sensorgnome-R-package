##
## SGPath: path to a component of the SG file hierarchy
##
SGPath = function(dirs = NULL, ...) {
    return (do.call(file.path, c(list("/", "SG"), dirs, ...)))
}
    
