\name{aurelhy}
\Rdversion{1.1}
\alias{aurelhy}
\alias{print.aurelhy}
\alias{summary.aurelhy}
\alias{plot.aurelhy}
\alias{points.aurelhy}
\alias{update.aurelhy}
\alias{predict.aurelhy}

\title{Create an 'aurelhy' object that contains required data to perform AURELHY interpolation }

\description{
  An 'aurelhy' object contains principal components calculated after the various variables describing the landscape,
  as well as other useful descriptors. Use the \code{predict()} method to interpolate some data with the AURELHY method.
}

\usage{
aurelhy(geotm, geomask, auremask = auremask(), x0 = 30, y0 = 30, step = 12,
    nbr.pc = 10, scale = FALSE, model = data ~ ., add.vars = NULL, var.name = NULL)

\method{print}{aurelhy}(x, \dots)
\method{plot}{aurelhy}(x, y, main = "PCA on land descriptors", \dots)
\method{points}{aurelhy}(x, pch = ".", \dots)
\method{summary}{aurelhy}(object, \dots)
\method{update}{aurelhy}(object, nbr.pc, scale, model,
    drop.vars, add.vars, var.name, \dots)
\method{predict}{aurelhy}(object, geopoints, \dots)
}

\arguments{
  \item{geotm}{ a terrain model ('geotm' object) with enough resolution to be
    able to calculate all landscape descriptors (use \code{print()} or
    \code{plot()} methods of an 'auremask' object used to calculate landscape
    descriptors to check your terrain model is dense enough). }
  \item{geomask}{ a 'geomask' object with same resolution and coverage of the
    'geotm' object, and indicating which points should be considered for the
    interpolation (note that your terrain model must be larger than the
    targetted area by, at least, maximum distance of the mask in all directions
    in order to be able to calculate landscape descriptors for all considered
    points). }
  \item{auremask}{ an 'auremask' object that defines the mask used around each
    point ot calculate its landscape descriptors. }
  \item{x0}{ shift in X direction (longitude) where to consider the first
    point of the interpolation grid (note that interpolation grid must be less
    dense or equal to the terrain model grid, depending on the mask used). }
  \item{y0}{ shift in Y direction (latitude) for the first point of the
    interpolation grid. }
  \item{step}{ resolution of the interpolation grid, i.e., we keep one point
    every \code{step} points from the original grid of the terrain model for
    constructing the interpolation grid. \code{step} must be a single integer
    larger or equal to one (may be equal to one only with rectangular 'auremask'
    objects). }
  \item{nbr.pc}{ number of PCA's principal components to keep in the
    interpolation. This is the initial value; the example show you how you can
    change this after the 'aurelhy' object is calculated. }
  \item{scale}{ should we scale the landscape descriptors (variance = 1) before
    performing the PCA? If \code{scale = FALSE} (by default), a PCA is run on
    the variance-covariance matrix (no scaling), otherwise, the PCA is run on
    the correlation matrix. }
  \item{model}{ a formula describing the model used to predict the data. The
    left-hand side of the formula must always be 'data' and the right-hand
    considers all predictors spearated by a plus sign. To use all predictors,
    specify \code{data ~ .} (by default). }
  \item{drop.vars}{ the name of the variables to drop from the object }
  \item{add.vars}{ additional variable(s) measured at the same points as the
    final interpolation grid. They will be used as additional predictors. The
    example show you how you can add or remove such variables after the
    'aurelhy' object is calculated. If \code{NULL} (by default), no additional
    variables will be used. }
  \item{var.name}{ if \code{add.vars} is a 'geomat' object, you can give the
    name you want to use for this predictor here. }
  \item{x}{ an 'aurelhy' object }
  \item{y}{ a 'geopoints' object to create a plot best depicting the
    interpolation process, or nothing to just plot the interpolation grid. }
  \item{main}{ the main title of the graph }
  \item{pch}{ the symbol to use for plotting points. The default value,
    \code{pch = "."} prints a small (usually one pixel size) square }
  \item{object}{ an 'aurelhy' object }
  \item{geopoints}{ a 'geopoints' object with data to be interpolated. }
  \item{\dots}{ further arguments passed to the function }
}

\details{
  \code{aurelhy()} creates a new 'aurelhy' object. The object has \code{print()}
  and \code{plot()} methods for further diagnostics. You should use the
  \code{predict()} method to perform the AURELHY interpolation on some data. The
  'aurelhy' object is also easy to save for further reuse (it is designed so
  that the most time-consumming operations are done during its creation; so, it
  is supposed to be generated only once and reused for different interpolations
  on the same terrain model).
}

\value{
  An 'aurelhy' object with all information required to perform an AURELHY
  interpolation with any 'geopoints' data.
}

\author{
  Philippe Grosjean <phgrosjean@sciviews.org>
}

\seealso{ \code{\link{geotm}}, \code{\link{auremask}} }

\examples{
# Create an aurelhy object for the Morocco terrain data
data(morocco)  # The terrain model with a grid of about 924x924m
data(mbord)    # A shape with Morocco's border
data(mmask)    # A 924x924m grid with a mask covering Morocco's territory
data(mseadist) # The distance to the sea for Morocco's territory

# Create a map with these data
image(morocco) # Plot the terrain model
grid()
lines(mbord, col = "red") # Add administrative borders in red

# Now, create an aurelhy object with landscape description, using the
# first five PCs, plus the distance to the sea (mseadist) for prediction
# Use a default radial mask of 26km as maximum distance
# and an interpolation grid of 0.1x0.1degrees (roughly 11x11km)
# Be patient... this takes a little time to calculate!
maurelhy <- aurelhy(morocco, mmask, auremask(), x0 = 30, y0 = 54, step = 12,
    scale = TRUE, nbr.pc = 5, add.vars = mseadist, var.name = "seadist")
maurelhy
points(maurelhy) # Add the interpolated points on the map

# Diagnostic of the PCA on land descriptors
summary(maurelhy)
plot(maurelhy)

# TODO: example of update() and predict()
}

\keyword{utilities}
