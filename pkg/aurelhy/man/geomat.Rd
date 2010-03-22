\name{geomat}
\Rdversion{1.1}
\alias{read.dem}
\alias{print.geomat}
\alias{coords.geomat}
\alias{resample.geomat}
\alias{window.geomat}
\alias{image.geomat}

\title{A geomat and dem object for AURELHY}

\description{
  Geomat are matrices of geographically referenced data. Objects 'dem' are
  special 'geomat' matrices containing integers and representing digital
  elevation maps.
}

\usage{
read.dem(file, type = "ascii")

\method{print}{geomat}(x, \dots)
\method{coords}{geomat}(x, type = "par", \dots)
\method{resample}{geomat}(x, x0 = 1, y0 = 1, nx = 100, step = NULL, \dots)
\method{window}{geomat}(x, xlim, ylim)
\method{image}{geomat}(x, max.xgrid = 500, col = terrain.colors(100),
    add = FALSE, xlab = "Longitude", ylab = "Latitude", asp = 1, \dots)
}

\arguments{
  \item{file}{ ... }
}

\details{
  \code{read.dem()} ...
}

\value{
  A 'geomat' object ...
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{auremask}} }

\examples{
# TODO...
}

\keyword{utilities}
