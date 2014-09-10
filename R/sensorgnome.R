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
#' - \code{a1} represents an omnidirectional antenna situated at (x, y, z) = (0, 0, 0)
#' and oriented vertically
#'
#' \code{a2 = makeOmni(pos=c(500, 0, 10), axis=45)}
#' - \code{a2} represents an omnidirectional antenna situated at 500 metres east
#' and 10 metres above ground.  It is oriented horizontally, and points due northeast (i.e. 45 degrees clockwise from North).
#'
#' \code{g = friis(a1, a2)}
#' - \code{g} is the one-way free-space power gain between antennas a1 and a2, in linear units.  i.e. If antenna \code{a1} is transmitting with power \code{P}, then antenna \code{a2} will receive the signal with power \code{g*P}.  This takes into account the distance between antennas, the spatial gain patterns of both antennas (i.e. 'beam shapes'), and the relative polarization of the two antennas.  It does not include atmospheric effects, multipath transmission, or ground propagation, for example.
#'
#' \code{a3 = makeYagi(n=5, pos=c(0, 1000, 0), 180, "horizontal")
#'
#' @docType package
#' @name sensorgnome.
NULL
