% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixTimeJumps.R
\name{fixTimeJumps}
\alias{fixTimeJumps}
\title{Fix timejumps in a series of timestamps from a sensorgnome.}
\usage{
fixTimeJumps(recvDB, ts, bootnum, depID)
}
\arguments{
\item{recvDB}{character scalar giving full path to the sqlite database for the receiver}

\item{ts}{numeric vector of raw timestamps}

\item{bootnum}{numeric vector of bootnumbers, same length as \code{ts}}

\item{depID}{numeric vector of deployment IDs, same length as \code{ts}}
}
\value{
a numeric vector of timestamps corrected for timejumps.
}
\description{
\code{fixTimeJumps} corrects jumps in a series of timestamps.  These jumps are
presumably due to transmission errors in the data from the USB gps, perhaps in
a noisy environment over a USB extension cable.
The gpsd program, which updates the sensorgnome clock from GPS data, is smart
enough not to set the clock backward, so only bit errors causing large forward
jumps will propagate through to the system clock, and hence sensorgnome data files.
}
\details{
Time jumps are detected in raw data streams by the sgdb.R script,
using a threshold (in a noisy data stream, very long intervals
between consecutive pulse detections are almost certainly due to
time jumps), and recorded to the YEAR_PROJECT_SITE.sqlite database
}

