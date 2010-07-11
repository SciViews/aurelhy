\name{auremask}
\Rdversion{1.1}
\alias{auremask}
\alias{print.auremask}
\alias{plot.auremask}

\title{Create and manipulate a mask for landscape descriptors used in AURELHY}

\description{
  An AURELHY mask ('auremask' object) specifies the regions relative to the point
  that define the various variables describing the landscape.
}

\usage{
auremask(type = "radial", dist = c(1, 6, 11, 16, 21, 26),
    angles = 0:7 * pi/4 + 0.01, n = 11, keep.origin = FALSE)

\method{print}{auremask}(x, geomat, \dots)
\method{plot}{auremask}(x, y, \dots)
}

\arguments{
  \item{type}{ the type of mask, either \code{"radial"} (by default), or
    \code{"rectangular"} as in the initial version of the AURELHY method. }
  \item{dist}{ A vector of distances (in km) to consider in the mask for the
    \code{"radial"} mask, or the distance to consider between two grid points
    for a \code{"rectangular"} mask (in this case, if you provide several
    distances, only the smallest one will be considered) }
  \item{angles}{ A vector of angles in radians to use to construct a
    \code{"radial"} mask. This argument is ignored for \code{"rectangular"}
    mask. Avoid to use angles parallels to the grid, like 0 or pi/4, because you
    will have points going into one or the other sector of your mask, depending
    on rounding of the numbers in the floating-point calculations! A slight
    shift angle (0.01, by default) avoids this unstability }
  \item{n}{ The number of grid points in latitude and longitude to use for a
    \code{"rectangular"} mask. For instance, if \code{n = 11}, the mask will be
    made of 11*11 = 121 points (minus one if \code{keep.origin is FALSE}). This
    argument is ignored for a \code{"radial"} mask. }
  \item{keep.origin}{ Is the origin where the mask is centered considered as
    one point of the grid, or not (by default, not, as in the original
    implementation of the AURELHY method) }
  \item{x}{ An 'auremask' object }
  \item{geomat}{ A reference grid, as a 'geomat' object against which the mask is
    tested (print or plot the number of points that are located in each sector
    of the mask)}
  \item{y}{ Same as \code{geomat} }
  \item{\dots}{ further arguments passed to the function }
}

\details{
  \code{auremask()} creates a new mask. The object has \code{print()} and
  \code{plot()} methods.
}

\value{
  An 'auremask' object with all information required to mask a 'geotm' object
  (terrain model) for creating landscape variables required by the AURELHY
  method.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{polar.coords}}, \code{\link{geomat}} }

\examples{
# Default mask
am <- auremask()
am
# Get an example terrain model and apply the mask on it
data(morocco)
plot(am, morocco)
# Further statistics are displayed with print() if a grid is provided too
print(am, morocco)
}

\keyword{utilities}
