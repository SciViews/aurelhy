\name{geomat}
\alias{as.geomat}
\alias{geomat}
\alias{geotm}
\alias{geomask}
\alias{read.geomat}
\alias{read.geotm}
\alias{read.geomask}
\alias{write.geomat}
\alias{write.geotm}
\alias{write.geomask}
\alias{print.geomat}
\alias{coords.geomat}
\alias{resample.geomat}
\alias{window.geomat}
\alias{plot.geomat}
\alias{image.geomat}
\alias{contour.geomat}
\alias{persp.geomat}

\title{A geomat, geotm or geomask object for AURELHY}

\description{
  Geomat are matrices of geographically referenced data. These are essentially
  georeferenced rectangular, regular grids of points. Data can be numeric
  (reals), integer, or logical (booleans). Objects 'geotm' are special 'geomat'
  matrices containing always integers and representing terrain models.
  Objects 'geomask' are also special 'geomat' that only contain logical values.
  They are mainly used to define a mask on top of a grid (which points to
  consider and which ones to eliminate from a calculation).
}

\usage{
geomat(x, size, xcenter, ycenter, coords = c(size = size, x = xcenter,
    y = ycenter), datatype = c("numeric", "integer", "logical"), nodata = NA)
geotm(x, size, xcenter, ycenter, coords = c(size = size, x = xcenter,
    y = ycenter))
geomask(x, size, xcenter, ycenter, coords = c(size = size, x = xcenter,
    y = ycenter))

read.geomat(file, type = "ascii", datatype = c("numeric", "integer", "logical"),
    \dots)
read.geotm(file, type = "ascii", \dots)
read.geomask(file, type = "ascii", threshold = 0, \dots)

write.geomat(x, file, type = "ascii", integers = FALSE, nodata = -9999, \dots)
write.geotm(x, file, type = "ascii", nodata = -9999, \dots)
write.geomask(x, file, type = "ascii", nodata = -9999, \dots)

as.geomat(x, \dots)

\method{print}{geomat}(x, \dots)
\method{coords}{geomat}(x, type = "par", \dots)
\method{resample}{geomat}(x, x0 = 1, y0 = 1, step = NULL, nx = 100, ny = nx,
    strict = FALSE, \dots)
\method{window}{geomat}(x, xlim, ylim, \dots)
\method{plot}{geomat}(x, y = NULL, max.xgrid = 100, nlevels = 50,
    color.palette = terrain.colors, xlab = "Longitude", ylab = "Latitude",
    asp = 1, \dots)
\method{image}{geomat}(x, max.xgrid = 500, col = terrain.colors(50),
    add = FALSE, xlab = if (add) "" else "Longitude",
    ylab = if (add) "" else "Latitude", asp = 1, \dots)
\method{contour}{geomat}(x, max.xgrid = 100, nlevels = 10, col = par("fg"),
    add = FALSE, xlab = if (add) "" else "Longitude",
    ylab = if (add) "" else "Latitude", asp = 1, \dots)
\method{persp}{geomat}(x, max.xgrid = 500, col = "green3",
    xlab = "Longitude", ylab = "Latitude", asp = 1, theta = 10, phi = 30,
    expand = 1, shade = 0.75, border = NA, box = TRUE, \dots)
}

\arguments{
  \item{x}{ An object (a matrix or data frame for \code{geomat()}, \code{geotm()},
    or \code{geomask()}, a 'predict.aurelhy' object for \code{as.geomat()}, or a 'geomat' object for the other functions) }
  \item{size}{ The size of a grid square (in decimal degrees) }
  \item{xcenter}{ The position of the center of the top-left square of the grid,
    that is, its longitude in decimal degrees }
  \item{ycenter}{ Idem, but latitude in decimal degrees }
  \item{coords}{ A named vector of three numbers: 'size', 'x' and 'y' as above }
  \item{datatype}{ The type of data to store in the grid, ort to read/write on
    the file. Can be 'numeric' (reals), 'integer', or 'logical' (booleans) }
  \item{nodata}{ The number to use to represent missing data in the grid (by
    default it is \code{NA}). For file operations, it is the numerical code used
    to represent missing or not applicable cell in the file. By default, it is
    -9999 in ASCII grid format }
  \item{file}{ The path to the file used for reading or writing data }
  \item{type}{ The type of data to read/write. Currently, only \"ascii\", which
    means ARC/INFO ASCII GRID format (.asc file). For \code{coords()}, it is
    the type of coordinates to ba calculated: \code{"par"} is the vector
    defining the coordinates as 'size' of the cell, 'x' and 'y' coordinates of
    the center of the top-left square in the grid and the 'x1', 'y1' coordinates
    of the top-left point and 'x2', 'y2' coordinates of the bottom-right points
    covered by the grid. If \code{"x"}, or \code{"y"}, \code{coords()} returns
    a vector of the coordinates of centers of the grid points. Finally if
    \code{"xy"}, then, \code{coords()} returns a data frame with 'x' and 'y'
    coordinates of all points in the grid (center of rectangles) }
  \item{threshold}{ Value (single integer) above which all data are converted to
    \code{TRUE}. The rest is converted to \code{FALSE}, except missing data that
    are encoded as \code{NA} during the conversion into logical values }
  \item{integers}{ Do we read/write integers (saves memory and disk space used
    to represent the grid) }
  \item{x0}{ The X origin of the new grid }
  \item{y0}{ The Y origin of the new grid }
  \item{step}{ The step to use for resampling (\code{step = 2} means we take one
    point every two original points in the grid). }
  \item{nx}{ The desired number of points in the X direction (longitude).
    \code{resample()} is a quick method that takes a point every n points in the
    grid without doing more calculation. The final number of points is an
    integer value of points that can be resampled without interpolation }
  \item{ny}{ idem than nx, but in the Y direction (latitude) }
  \item{strict}{ do we interpolated the grid so that we obtain exactly \code{nx}
    and \code{ny} point (when \code{strict = TRUE})? By default, not
    (\code{strict = FALSE}) and we span as far as possible to the right and to
    the bottom for the interpolated grid }
  \item{xlim}{ A vector of two numbers defining the limits to use in X direction
    (longitude) for the window }
  \item{ylim}{ A vector of two numbers defining the limits to use in Y direction
    (latitude) for the window }
  \item{y}{ Unused argument to match \code{plot()} method definition }
  \item{max.xgrid}{ The maximum number of points in x direction to use. If the
    grid that is plotted is denser, it is furst resampled to avoid drawing a
    graph with too much points }
  \item{nlevels}{ the number of contour levles to calculate }
  \item{color.palette}{ a color palette generation function }
  \item{col}{ A vector of colors to use for the plot }
  \item{xlab}{ The label of the X axis (\code{"Longitude"} by default) }
  \item{ylab}{ The label of the Y axis (\code{"Latitude"} by default) }
  \item{asp}{ The aspect ratio between 'x' and 'y'. The default value of
    \code{asp = 1} should usually not be changed. }
  \item{add}{ Do we add the graph to an existing graph device, or do we plot a
    fresh new graph? }
  \item{theta}{ angles defining the viewing direction. \code{theta} gives the
    azimuthal direction }
  \item{phi}{ \code{phi} is the colatitude angle of the viewing direction }
  \item{expand}{ the expansion level to use for the z-axis in the perspective }
  \item{shade}{ the shade at a surface facet is computed as \code{((1+d)/2)^shade},
    where \code{d} is the dot product of a unit vector normal to the facet and a
    unit vector in the direction of a light source. Values of shade close to one
    yield shading similar to a point light source model and values close to zero
    produce no shading. Values in the range 0.5 to 0.75 provide an approximation
    to daylight illumination. }
  \item{border}{ the color of the borders of facets. If \code{NA}, no border is
    drawn. This is usually a good value when shading is used }
  \item{box}{ If \code{TRUE}, a box, aznd axes are drawn around the perspective
    plot }
  \item{\dots}{ Further arguments passed to the functions (only used for the
    plotting method) }
}

\value{
  An object of class, respectively 'geomat', 'geotm' or 'geomask' inheriting from
  'matrix' is created. Methods either return an object of same class, or are used
  for their side effect of plotting a graph. Objects 'geotm' and 'geomask' also
  inherit from 'geomat'.
  
  A 'geomat' object. For the \code{print()} method, size of the grid is presented
  in km, with the corresponding conversion: one latitudinal degree is
  approximately 110.9km.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{aurelhy}}, \code{\link{auremask}} }

\examples{
# Create a simple geomat object containing random numbers
(gm <- geomat(matrix(rnorm(120), nrow = 10), 0.1, 10, 20))
# Get coordinates for this grid
coords(gm)
# Longitudes (x) and latitudes (y) for the center of all squares
coords(gm, type = "x")
coords(gm, type = "y")
# Coordinates of the center of all squares
coords(gm, type = "xy")

# Resample the grid to take one point every second points in the original grid
resample(gm, step = 2)

# Extract a window from the grid (keep only squares with centers in the window)
window(gm, xlim = c(9.5, 10.2), ylim = c(19.5, 20.6))

# Plot this grid in different ways
plot(gm)
image(gm)
contour(gm)
persp(gm, expand = 100)

# Now load real data (Morocco terrain model)
data(morocco)
morocco
image(morocco)
contour(morocco, add = TRUE)
grid()

# The mask of points inside Morocco territory was obtained like that:
#library(splancs)
#data(mbord)
#inm <- inout(coords(morocco, "xy"), mbord[[1]])
#mmask <- morocco
#mmask[inm] <- 1
#mmask[!inm] <- 0
#mmask[is.na(morocco)] <- NA
#mmask <- geomask(mmask, coords = coords(mmask))

data(mmask)
image(mmask)

# Get Morocco frontiers from a shapefile
# To read it from an ESRI shape
#mbord <- read.geoshapes("morocco_border.shp")

data(mbord)
lines(mbord, col = "red")
}

\keyword{utilities}
