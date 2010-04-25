\name{aurelhy-utilities}
\Rdversion{1.1}
\alias{polarcoords}
\alias{match.coords}
\alias{coords}
\alias{resample}

\title{Various utilities functions for AURELHY}

\description{
  These functions manipulate geographical coordinates in various ways to optimize
  computation of the AURELHY method.
}

\usage{
polarcoords(geomat, x, y, maxdist)
match.coords(points, table, tol = 0.002)
coords(x, \dots)
resample(x, \dots)
}

\arguments{
  \item{geomat}{ A 'geomat' object }
  \item{x}{ X coordinate of the reference point }
  \item{y}{ Y coordinate of the reference point }
  \item{maxdist}{ Maximum distance to consider. All points whose distance from
    the reference point is larger are not considered in the calculation }
  \item{points}{ A list or data frame with 'x' and 'y' coordinates of the
    points to match to the reference points }
  \item{table}{ A similar list or data frame with 'x' and 'y' coordinates of the
    reference points to be matched }
  \item{tol}{ The maximum tolerance in 'x' and 'y' units to consider points are
    matching, that is, x +/- tol = x(ref) and y +/- tol = y(ref) }
  \item{\dots}{ Further arguments passed to the method }
}

\details{
  \code{polarcoords()} calculated polar coordinates of points.
  \code{match.coords()} select points with matching coordinates, given a
  tolerance distance between the reference points (usually, a DEM grid) and the
  points to match (stations).
  \code{coords()} is a generic function that extracts geographical coordinates
  from one object.
  \code{resample} is a generic function to resample a grid (geomat object).
}

\value{
  ... TODO
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{auremask}} }

\examples{
# TODO...
}

\keyword{utilities}
