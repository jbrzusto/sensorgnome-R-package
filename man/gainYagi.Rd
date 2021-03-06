% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gainYagi.R
\name{gainYagi}
\alias{gainYagi}
\title{Gain of yagi antenna in a particular direction.}
\usage{
gainYagi(pattern, disp)
}
\arguments{
\item{pattern}{the gain pattern for the antenna. This is a 181 x 91 element matrix
giving the gain in each direction specified by row (azimuth, in integer degrees
clockwise from North) and column (elevation angle, in integer degrees above the
horizontal).  The pattern corresponds to the antenna in standard position;
i.e. main axis pointing in positive y direction; cross-pieces pointing along
the x axis.}

\item{disp}{unit 3-vector or n x 3 matrix of unit vector rows in
direction(s) of interest.  This must already have been rotated to
correspond to the antenna in standard position.}
}
\value{
one-way far-field power gain factor, in linear units,
relative to the isotropic pattern (i.e. a truly isotropic antenna
would give 1 in every direction), for each row of \code{disp}
}
\description{
Calculate the one-way far-field power gain
for a yagi antenna of given number pattern and orientation.
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

