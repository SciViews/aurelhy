\name{aurelhy-utilities}
\Rdversion{1.1}
\alias{polarcoords}
\alias{match.coords}

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
  \item{geomat}{ A geomat object}
  \item{x}{  }
  \item{y}{  }
  \item{maxdist}{  }
  \item{points}{  }
  \item{table}{  }
  \item{tol}{  }
  \item{\dots}{  }
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
  ...
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{auremask}} }

\examples{
# TODO...
}

\keyword{utilities}
