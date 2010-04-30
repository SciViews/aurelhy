\name{aurelhy-utilities}
\Rdversion{1.1}
\alias{polar.coords}
\alias{match.coords}
\alias{coords}
\alias{resample}

\title{Various utilities functions for AURELHY}

\description{
  These functions manipulate geographical coordinates in various ways to optimize
  computation of the AURELHY method.
}

\usage{
polar.coords(geomat, x, y, maxdist)
match.coords(points, table, tol = 0.002)
coords(x, \dots)
resample(x, \dots)
}

\arguments{
  \item{geomat}{ a 'geomat' object }
  \item{x}{ X coordinate of the reference point }
  \item{y}{ Y coordinate of the reference point }
  \item{maxdist}{ maximum distance to consider in km. All points whose distance
    from the reference point is larger are not considered in the calculation }
  \item{points}{ a list or data frame with X and Y coordinates of the points to
    match to the reference points (in decimal degrees, for instance) }
  \item{table}{ a similar list or data frame with X(ref) and Y(ref) coordinates
    of the reference points to be matched (in the same units as for
    \code{points}) }
  \item{tol}{ the maximum tolerance in X and Y units to consider points are
    matching, that is, X +/- tol = X(ref) and Y +/- tol = Y(ref) }
  \item{\dots}{ further arguments passed to the method }
}

\details{
  \code{polar.coords()} calculates polar coordinates of points.
  \code{match.coords()} selects points with matching coordinates, given a
  tolerance distance between the reference points (i.e., from a geotm grid,
  using \code{coords(my_geotm, "xy")}) and the points to match (stations).
  \code{coords()} is a generic function that extracts geographical coordinates
  from one object in different fashions.
  \code{resample} is a generic function to resample a grid ('geomat' object).
}

\value{
  \code{polar.coords()} returns a data frame with 'angle' in rad and 'dist'(ance)
  in km for the reference point to each point in the grid, within 'maxdist'.
  There is also a 'geomat' attribute containing the window of the initial
  'geomat' object containing the considered points.
  
  \code{match.coords()} returns a vector of logical of the same length as the
  number of colunms in the points data frame (that must contain 'x' and 'y'
  columns with coordinates of points to be matched). 
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{geomat}}, \code{\link{auremask}} }

\keyword{utilities}
