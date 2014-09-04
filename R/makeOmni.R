#' Create an omnidirectional antenna object.
#'
#' Creates a 'proto' object of class 'antenna' which will store
#' position and orientation of an omnidirectional antenna.
#' 
#' @param pos 3-element antenna position vector (x, y, z)
#' 
#' @param axis 3-element antenna orientation axis (x, y, z). This
#' points along the antenna.
#'
#' @references \url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

makeOmni = function(pos, axis) {
    if (! exists(".omni.proto"))
        .omni.proto <<- proto(pos=NA, axis=NA, gain=function(.ignore, x) gainOmni(x),
                              setAxis = function(obj, axis) {
                                  ## store axis, ensuring it's a unit vector forming the first column of a 1 x 3 matrix
                                  x$axis = matrix(axis / mag(axis), nrow=1)
                              },
                              type="omnidirectional")
    x = proto(.omni.proto)
    ## store position
    x$pos = pos

    x$setAxis(axis)
    ## proto() doesn't copy extra class attributes from its proto argument, so set it here:
    class(x) = c(class(x), "antenna")
    return(x)
}




        
