\name{auremask}
\Rdversion{1.1}
\alias{auremask}
\alias{print.auremask}
\alias{plot.auremask}

\title{Create and manipulate a mask for terrain profile data used in AURELHY}

\description{
  An AURELHY mask (auremask object) specifies the regions relative to the point
  that define the various variables describing the landscape.
}

\usage{
auremask(type = "radial", dist = c(1, 6, 11, 16, 21, 26),
  angles = 0:7 * pi/4 + 0.01, n = 11, keep.origin = FALSE)

\method{print}{auremask}(x, grid, ...)
\method{plot}{auremask}(x, y, ...)
}

\arguments{
  \item{type}{ the type of mask, either \code{"radial"} (by default), or
    \code{"rectangular"} as the initial version of the AURELHY method. }
}

\details{
  \code{auremask()} creates a new mask. The object has \code{print()} and
  \code{plot()} methods.
}

\value{
  An 'auremask' object with all information required to mask a DEM for creating
  landscape variables.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{polarcoords}} }

\examples{
# Default mask
ag <- auremask()
ag
# Get an example terrain model and apply the mask on it
data(maroc.tm)
plot(ag, maroc.tm)
}

\keyword{utilities}
