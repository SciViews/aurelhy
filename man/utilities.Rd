\name{aurelhy-utilities}
\alias{deg.lat}
\alias{deg.lon}
\alias{polar.coords}
\alias{match.coords}
\alias{coords}
\alias{resample}
\alias{add.points}
\alias{add.points.geomask}
\alias{dist2sea}

\title{Various utilities functions for AURELHY}

\description{
  These functions manipulate geographical coordinates in various ways to optimize
  computation of the AURELHY method.
}

\usage{
deg.lat(latitude)
deg.lon(latitude)
polar.coords(geomat, x, y, maxdist)
match.coords(points, table, tol = 0.002)
coords(x, \dots)
resample(x, \dots)
add.points(x, \dots)
\method{add.points}{geomask}(x, geopoints, \dots)
dist2sea(geotm)
}

\arguments{
  \item{latitude}{ the latitude in decimal degrees }
  \item{geomat}{ a 'geomat' object }
  \item{x}{ X coordinate of the reference point for \code{polar.coords()}, or
    a correct object for the other functions }
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
  \item{geopoints}{ a geopoints object from which we want to add corresponding
    points in a geomask }
  \item{geotm}{ a geotm object }
}

\details{
  \code{deg.lat()} and \code{deg.lon()} provide the length of one degree in,
  respectively, latitude and longitude in km, given the corresponding latitude
  in decimal degrees. The ellipsoid defined in WGS84 model is used for these
  calculations.
  \code{polar.coords()} calculates polar coordinates of points.
  \code{match.coords()} selects points with matching coordinates, given a
  tolerance distance between the reference points (i.e., from a geotm grid,
  using \code{coords(my_geotm, "xy")}) and the points to match (stations).
  \code{coords()} is a generic function that extracts geographical coordinates
  from one object in different fashions.
  \code{resample} is a generic function to resample a grid ('geomat' object).
  \code{add.points} add points from a geopoints object in a geomask.
  \code{dist2sea()} calculate the distance of points in a geotm object to the
  sea.
}

\value{
  \code{deg.lat()} and \code{deg.lon()} return the length of one degree in km.
  \code{polar.coords()} returns a data frame with 'angle' in rad and 'dist'(ance)
  in km for the reference point to each point in the grid, within 'maxdist'.
  There is also a 'geomat' attribute containing the window of the initial
  'geomat' object containing the considered points.
  
  \code{match.coords()} returns a vector of logical of the same length as the
  number of colunms in the points data frame (that must contain 'x' and 'y'
  columns with coordinates of points to be matched). 
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>, and Francois Delobel for
  \code{dist2sea()}
}

\seealso{ \code{\link{geomat}}, \code{\link{auremask}} }

\examples{
# Size of one degree in latitude and longitude, given the latitude in decimal degrees
deg.lat(c(0, 15, 30, 45, 60, 75, 90))
# 110.574  110.649  110.852  111.132  111.412  111.618  111.694
deg.lon(c(0, 15, 30, 45, 60, 75, 90))
# 111.320  107.550  96.486  78.847  55.800  28.902  0.000
}

\keyword{utilities}
