\name{geoshapes}
\Rdversion{1.1}
\alias{geoshapes}
\alias{read.geoshapes}
\alias{write.geoshapes}
\alias{print.geoshapes}
\alias{lines.geoshapes}
\alias{points.geoshapes}

\title{A geosshape object containing one or more georeferenced shapes}

\description{
  Geoshapes objects contain one or more shapes (that is, polygons, points, or
  polylines) defined by their longitude and latitude in decimal degrees. These
  objects can be read or write to ERSI shape files.
}

\usage{
geoshapes(x, name = "1")
read.geoshapes(shpFile)
write.geoshapes(x, file, type = c("polygon", "point", "polyLine"), \dots)

\method{print}{geoshapes}(x, \dots)
\method{lines}{geoshapes}(x, which = 1, \dots)
\method{points}{geoshapes}(x, which = 1, \dots)
}

\arguments{
  \item{x}{ A data frame with columns 'x' and 'y' for longitudes and latitudes of
    the points in decimal degrees, or a list of such data frames for
    \code{geoshapes()}; a 'geoshapes' object for the other functions }
  \item{name}{ The name to use for the shape in case a data frame is passed to
    \code{geoshapes()}. Ignored if a list is passed to the function }
  \item{shpFile}{ The path to a .shp file (ESRI shape file) to import }
  \item{file}{ The path to an ESRI file where to write data, without extension.
    Three files are created, with respective extensions .shp, .shx, and .dbf }
  \item{type}{ The type of shape to write in the ESRI shape file }
  \item{which}{ The index of the shape to use, or its name }
  \item{\dots}{ Further arguments passed to the functions (not used yet) }
}

\details{
  \code{geoshapes()} converts a data frame or a list into a 'geoshapes' object.
  \code{read.geoshapes()} and \code{write.geoshapes()} read and write shapes
  from or to ESRI shape files on disk.
  The 'geoshapes' objects have methods to print them (very concisely), and to
  add them to graphs, as polygons \code{lines()}, or as separate points
  \code{points()}.
}

\value{
  A 'geoshapes' object is returned from \code{geoshapes()} and
  \code{read.geoshapes()}. The other functions are used for their side-effect
  rather than for returning something useful.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{geomat}} }

\examples{
data(mbord) # Morocco borders
mbord

# Plot of Morocco terrain and add the borders in red
data(morocco)
image(morocco)
lines(mbord, col = 2)

# Simulate the creation of a geoshapes object with two shapes
geoshapes(list(a = mbord[[1]], b = mbord[[1]]))
}

\keyword{utilities}
