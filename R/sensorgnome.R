#' Tools for working with Sensorgnome field computers
#'
#' This is a mixed bag of utility functions to work with data obtained
#' from sensorgnomes, which are embedded linux computers recording raw
#' or semi-processed data from medium bandwidth sensors such as VHF
#' telemetry radio receivers and audio microphones.
#'
#' Here is a brief introduction to some telemetry functions related to localization:
#'
#' \code{a1 = makeOmni(pos=c(0, 0, 0), axis="vertical")}
#' 
#' \code{a1} represents an omnidirectional antenna situated at (x, y, z) = (0, 0, 0)
#' and oriented vertically
#'
#' \code{a2 = makeOmni(pos=c(500, 0, 10), axis=45)}
#' 
#' \code{a2} represents an omnidirectional antenna situated at 500 metres east
#' and 10 metres above ground.  It is oriented horizontally, and points due northeast (i.e. 45 degrees clockwise from North).
#'
#' \code{g = friis(a1, a2)}
#' 
#' \code{g} is the one-way free-space power gain between antennas a1 and a2, in linear units.  i.e. If antenna \code{a1} is transmitting with power \code{P}, then antenna \code{a2} will receive the signal with power \code{g*P}.  This takes into account the distance between antennas, the spatial gain patterns of both antennas (i.e. 'beam shapes'), and the relative polarization of the two antennas.  It does not include atmospheric effects, multipath transmission, or ground propagation, for example.
#'
#' \code{a3 = makeYagi(n=5, pos=c(0, 1000, 0), 180, "horizontal")}
#'
#' \code{a3} is a 5-element Yagi-Uda antenna located at 1000 metres North of the origin, at ground level.  Its main axis is pointing due South (i.e. 180 degrees clockwise from North), and its cross-pieces are horizontal (i.e. parallel to the ground).
#' Once an antenna has been created, you can change its position or orientation using prototype methods:\cr
#' \code{a3$setPos(c(-500, -500, 0))} \cr
#' will move the antenna to 500 metres West, 500 metres South of the origin.  Subsequent calculations with the \code{friis} function will use the new position.
#'
#' \code{a3$setAxis(c(270, 30), "horizontal")} \cr
#' will re-orient the antenna so that its main axis is pointing toward the West (i.e. 270 degrees clockwise from North), but tilted at 30 degrees above the horizon.  The cross-pieces are still parallel to the ground.
#'
#' The function defined by \cr
#' \code{dB = function(x) 10*log10(x)} \cr
#' is useful and will be in the next version of this package.  It converts
#' linear power units to dB.
#'
#' See documentation for \code{friis} for a more detailed example.
#' 
#' @docType package
#' @name sensorgnome
NULL
