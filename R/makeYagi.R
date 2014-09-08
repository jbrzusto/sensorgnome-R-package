#' Create a yagi antenna object.
#'
#' Creates a 'proto' object of class 'antenna' which will store
#' position and axis orientations of a Yagi-Uda antenna.
#'
#' @param n integer; number of elements (cross pieces) of the antenna.
#' So far, only antennas with 5 and 9 elements are supported.
#' 
#' @param pos 3-element antenna position vector (x, y, z)
#' 
#' @param axis1 orientation of main antenna axis.  This is one of the following:
#' \enumerate{
#'   \item "vertical"; this represents an atenna pointing straight up
#'   \item a single number; this represents the azimuth (degrees clockwise from North)
#' of an antenna oriented parallel to the ground
#'   \item a 2-vector; this represents azimuth and elevation angles (degrees clockwise
#' from North, degrees above the horizon) for an antenna pointing in an arbitrary direction
#'   \item a 3-vector; this is a vector giving the direction (x, y, z) in which the antenna
#' is pointing
#' }
#'
#' @param axis2 orientation of the antenna elements ('cross pieces') relative to the antenna axis.
#' Specified as one of the following:
#' 
#' \enumerate{
#' \item "horizontal": the elements are parallel to the ground
#' \item "vertical": the elements are vertical
#' \item a single number: the angle of rotation of the elements when viewed from the antenna
#' base (wider end) toward the antenna tip.  Angle is measured clockwise in degrees, starting
#' at 0 for horizontal.  So 0 is the same as "horizontal" and 90 is the same as "vertical".
#' \item a 2-vector: this represents azimuth and elevation angles (degrees clockwise
#' from North, degrees above the horizon) for the antenna elements, and must be perpendicular to
#' the \code{axis1} parameter
#' \item a 3-vector: this is a vector giving the direction (x, y, z) in which the antenna elements
#' are pointing.  It must be perpendicular to the \code{axis1} parameter.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

makeYagi = function(n, pos, axis1, axis2) {
    gainfun = paste("gainYagi", n, sep="")
    if (!exists(gainfun))
        stop("I don't know the antenna pattern for a Yagi antenna with", n, "elements.  Contact sensorgnome.org for help")
    gainfun = get(gainfun)
    if (! exists(".omni.yagi"))
        .omni.yagi <<- proto(pos=NA, axis=NA, gain=function(obj, x) gainfun(obj$axis, x),
                              setAxis = function(obj, axis1, axis2) {
                                  axis1 = makeAxis(axis1)
                                  axis2 = makeAxis(axis2, axis1)
                                  if (abs(dot(axis1, axis2) > 1e-10))
                                      stop("Specified antenna axes are not perpendicular")
                                  ## Note: we store the element axis first, to match the omni antennas
                                  obj$axis = matrix(c(axis2, axi1), nrow=2, byrow=TRUE)
                              },
                              type="yagi")
    x = proto(.omni.proto)
    ## store position
    x$pos = pos

    x$setAxis(axis1, axis2)
    ## proto() doesn't copy extra class attributes from its proto argument, so set it here:
    class(x) = c(class(x), "antenna")
    return(x)
}




        
