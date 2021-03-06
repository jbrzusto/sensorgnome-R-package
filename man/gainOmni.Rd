% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gainOmni.R
\name{gainOmni}
\alias{gainOmni}
\title{Gain pattern of an omnidirectional antenna.}
\usage{
gainOmni(angle)
}
\arguments{
\item{angle}{(radians) vector or matrix of direction angles in space.  A
vector is treated as an n x 1 matrix.  The first column of this
parameter is taken as a set of angles between the antenna axis and
the desired direction. Any other columns are ignored, since this
antenna pattern is symmetric around the antenna axis.}
}
\value{
one-way far-field power gain factor, in linear units,
relative to the isotropic pattern.
}
\description{
Calculate the one-way far-field power gain (relative to isotropic)
for an omnidirectional (half-wave dipole) antenna, given the
displacement angle to points in space.
}
\note{
This gain factor is purely due to far-field antenna pattern,
and does not consider distance to the remote point(s) or relative
polarization of remote antenna.
}
\author{
John Brzustowski \email{jbrzusto@REMOVE_THIS_PART_fastmail.fm}
}
\references{
\url{http://en.wikipedia.org/wiki/Dipole_antenna#Half-wave_dipole}
}

