\name{geopoints}
\alias{geopoints}
\alias{read.geopoints}
\alias{write.geopoints}
\alias{print.geopoints}
\alias{points.geopoints}

\title{A 'geopoints' object containing one or more georeferenced data}

\description{
  Geospoints objects contain data for one or more points defined by their
  longitude and latitude in decimal degrees. These objects can be read or write
  to ERSI shape files, or DBF database.
}

\usage{
geopoints(x)
read.geopoints(File, format)
write.geopoints(x, file, arcgis = FALSE,\dots)

\method{print}{geopoints}(x, \dots)
\method{points}{geopoints}(x, \dots)
}

\arguments{
  \item{x}{ A 'geoshapes' object or a data frame with columns 'x' and 'y'
    for longitudes and latitudes of the points in decimal degrees. For
    \code{print()} and \code{points()} methods, it is a 'geopoints' object }
  \item{File}{ The path to a .shp (ESRI shape file) or .dbf (DBase) file to import }
  \item{format}{ Either \code{"shp"} or \code{"dbf"}. If you do not provide this
    argument, the format is guess from the \code{File} extension }
  \item{file}{ The path to an ESRI file where to write data, without extension.
    Three files are created, with respective extensions .shp, .shx, and .dbf }
  \item{arcgis}{ If \code{TRUE}, the header of the DBF table is made compatible
    with ArcGIS, that is, dot (.) is replaced by underline (_) }
  \item{\dots}{ Further arguments passed to the functions (not used yet) }
}

\details{
  \code{geopoints()} converts a 'geoshapes' object or a data frame into a
  'geopoints' object.
  \code{read.geoshapes()} and \code{write.geoshapes()} read and write shapes
  from or to ESRI shape files or DBase files on disk.
  The 'geoshapes' objects have methods to print them, and to
  add them to graphs (points at corresponding coordinates).
}

\value{
  A 'geopoints' object is returned from \code{geopoints()} and
  \code{read.geopoints()}. The other functions are used for their side-effect
  rather than for returning something useful.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{geomat}}, \code{\link{geoshapes}} }

\examples{
data(mpet)
mpet

# Plot of Morocco terrain and add the stations location in red
data(morocco)
image(morocco)
points(mpet, col = 2)
}

\keyword{utilities}
