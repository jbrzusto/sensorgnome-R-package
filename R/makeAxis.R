#' Generate a 3-D unit axis vector.
#'
#' Creates a unit 3-vector indicating an axis direction.  Various ways of specifying
#' the direction in 3D are allowed.
#'
#' @param dir direction of antenna axis, which can be specified several ways:
#' 
#' \enumerate{
#' \item "horizontal": the elements are parallel to the ground and perpendicular to the
#' reference axis.
#' \item "vertical": the elements are vertical
#' 
#' \item a single number: if \code{ref} is specified, then \code{dir}
#' is the angle of rotation of the elements when viewed from the
#' antenna base (wider end) toward the antenna tip.  Angle is measured
#' clockwise in degrees, starting at 0 for horizontal.  So 0 is the
#' same as "horizontal" and 90 is the same as "vertical".
#' 
#' If \code{ref} is \code{NULL}, then \code{dir} is the azimuth angle
#' (degrees clockwise from North) of an axis parallel to the ground.
#' 
#' \item a 2-vector: this represents azimuth and elevation angles (degrees clockwise
#' from North, degrees above the horizon) of the axis.
#' \item a 3-vector: this is a vector giving the axis as a direction vector (x, y, z).
#'
#' @param ref: reference axis.  If not null, must be a unit 3-vector.
#' 
#' @return a unit 3-vector giving the actual direction of the axis
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

makeAxis = function(dir, ref = NULL) {
    if (!is.null(ref)) {
        if (length(ref) != 3)
            stop("reference axis, if specified, must be a 3-vector")
        ref = ref / mag(ref)
    }
    if (dir[1] == "horizontal") {
        if (is.null(ref))
            stop("cannot specify 'horizontal' without a reference axis")
        return (crossProd(ref, c(0, 0, 1), TRUE))
    }
    if (dir[1] == "vertical")
        return (c(0, 0, 1))
    if (length(dir) == 1) {
        if (is.null(ref)) {
            dir = rad(90 - dir)
            return (c(cos(dir), sin(dir), 0))
        } else {
            ## get a basis for the plane perpendicular to the reference axis
            basis = svd(matrix(c(ref, rep(0,6)), 3, 3))$u[,2:3]

            ## coordinates of a unit vector parallel to the ground in that basis
            ch = c(basis[3,2], -basis[3,1])
            ch = ch / mag(ch)

            ## coordinates of a unit vector perpendicular to the preceding
            cv = c(ch[2], -ch[1])

            ## ensure the coordinates are for a vector pointing up
            if ((basis %*% cv)[3] < 0)
                cv = -cv
            
            dir = rad(dir)

            ax = basis %*% (cos(dir)*ch + sin(dir)*cv)
            return(ax / mag(ax))
        }
    }
    if (length(dir) == 2) {
        dir = rad(c(90 - dir[1], dir[2]))
        return (c(cos(dir[2])*cos(dir[1]), cos(dir[2])*sin(dir[1]), sin(dir[2])))
    }
    return (dir / mag(dir))
}
