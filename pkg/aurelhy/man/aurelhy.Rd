\name{aurelhy}
\Rdversion{1.1}
\alias{aurelhy}
\alias{as.geomat.aurelhy}
\alias{print.aurelhy}
\alias{summary.aurelhy}
\alias{plot.aurelhy}
\alias{points.aurelhy}
\alias{update.aurelhy}
\alias{predict.aurelhy}
\alias{print.predict.aurelhy}
\alias{summary.predict.aurelhy}
\alias{plot.predict.aurelhy}
\alias{as.geomat.predict.aurelhy}

\title{Create an 'aurelhy' object that contains required data to perform AURELHY interpolation }

\description{
  An 'aurelhy' object contains principal components calculated after the various variables describing the landscape,
  as well as other useful descriptors. Use the \code{predict()} method to interpolate some data with the AURELHY method.
}

\usage{
aurelhy(geotm, geomask, auremask = auremask(), x0 = 30, y0 = 30, step = 12,
    nbr.pc = 10, scale = FALSE, model = data ~ ., vgm = vgm(1, "Sph", 10, 1),
    add.vars = NULL, var.name = NULL)

\method{print}{aurelhy}(x, \dots)
\method{plot}{aurelhy}(x, y, main = "PCA on land descriptors", \dots)
\method{points}{aurelhy}(x, pch = ".", \dots)
\method{summary}{aurelhy}(object, \dots)
\method{update}{aurelhy}(object, nbr.pc, scale, model, vgm, \dots)
\method{predict}{aurelhy}(object, geopoints, variable, v.fit = NULL, \dots)

\method{print}{predict.aurelhy}(x, \dots)
\method{summary}{predict.aurelhy}(object, \dots)
\method{plot}{predict.aurelhy}(x, y, which = 1, \dots)

\method{as.geomat}{aurelhy}(x, what = "PC1", nodata = NA, \dots) 
\method{as.geomat}{predict.aurelhy}(x,
    what = c("Interpolated", "Predicted", "KrigedResiduals", "KrigeVariance"),
	nodata = NA,\dots)
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
  \item{vgm}{ the variogram model to fit, as defined by the \code{vgm()}
    function of the gstat package }
  \item{add.vars}{ additional variable(s) measured at the same points as the
    final interpolation grid. They will be used as additional predictors. The
    example show you how you can add or remove such variables after the
    'aurelhy' object is calculated. If \code{NULL} (by default), no additional
    variables will be used. }
  \item{var.name}{ if \code{add.vars} is a 'geomat' object, you can give the
    name you want to use for this predictor here. }
  \item{x}{ an 'aurelhy' or 'predict.aurelhy' object, depending on the method invoked }
  \item{y}{ a 'geopoints' object to create a plot best depicting the
    interpolation process, or nothing to just plot the interpolation grid. }
  \item{main}{ the main title of the graph }
  \item{pch}{ the symbol to use for plotting points. The default value,
    \code{pch = "."} prints a small (usually one pixel size) square }
  \item{object}{ an 'aurelhy' object }
  \item{geopoints}{ a 'geopoints' object with data to be interpolated. }
  \item{variable}{ the name of the variable in the 'geopoints' object to interpolate }
  \item{v.fit}{ the fitted variogram model used to krige residuals. If \code{NULL}
    (by default), a fitted model for the variogram is calculated, starting from the
    model provided in the 'aurelhy' object, 'vgm' slot }
  \item{which}{ which graph to plot }
  \item{what}{ what is extracted as a 'geomat' object }
  \item{nodata}{ the code used to represent missing data in the 'geomat' object }
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

\source{
  Benichou P, Le Breton O (1987). Prise en compte de la topographie pour la
  cartographie des champs pluviometriques statistiques. La Meteorologie, 7:23-34.
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
data(mrain)    # Rain data measured at 43 stations to be interpolated

# Create a map with these data
image(morocco) # Plot the terrain model
grid()
lines(mbord, col = "red") # Add administrative borders in red

# Now, create an aurelhy object with landscape description, using the
# first ten PCs, plus the distance to the sea (mseadist) for prediction
# Use a default radial mask of 26km as maximum distance
# and an interpolation grid of 0.1x0.1degrees (roughly 11x11km)
# The variogram model is kept simple here, see ?vgm for other choices
# Be patient... this takes a little time to calculate!
maurelhy <- aurelhy(morocco, mmask, auremask(), x0 = 30, y0 = 54, step = 12,
    scale = TRUE, nbr.pc = 10, vgm = vgm(100, "Sph", 1, 1),
    add.vars = mseadist, var.name = "seadist")    
maurelhy
points(maurelhy) # Add the interpolated points on the map
points(mrain, col = "red") # Add location of weather stations in red

# Diagnostic of the PCA on land descriptors
summary(maurelhy)
plot(maurelhy)

# Interpolate 'rain' variable on the whole Morocco territory
# Since we do not want negative values for this variable and it is log-normally
# distributed, we will interpolate log10(rain) instead
mrain$logRain <- log10(mrain$rain)
pmrain <- predict(maurelhy, mrain, "logRain")
pmrain

# Diagnostic of regression model
summary(pmrain) # Significant predictors at alpha = 0.01 are x, y, PC3, PC6 and PC7
# one could simplify the model as data ~ x + y + PC3 + PC6 + PC7
# but it is faster to keep the full model for final interpolation
# when we are only interested by the final interpolation or when processing
# is automated...
# Any of the predictors can be extracted from maurelhy as a geomat object
# for further inspection. For instance, let's look at PC3, PC6 and PC7 components
persp(as.geomat(maurelhy, "PC3"), expand = 50)
persp(as.geomat(maurelhy, "PC6"), expand = 50)
persp(as.geomat(maurelhy, "PC7"), expand = 50)

plot(pmrain, which = 1) # Residuals versus fitted (how residuals spread?)
plot(pmrain, which = 2) # Normal Q-Q plot of residuals (residuals distribution)
plot(pmrain, which = 3) # Best graph to look at residuals homoscedasticity
plot(pmrain, which = 4) # Cook's distance of residuals versus observation
plot(pmrain, which = 5) # Residuals leverage: are there influencial points?
# Map of predicted values
filled.contour(as.geomat(pmrain, "Predicted"), asp = 1,
    color.palette = terrain.colors, main = "Values predicted by the linear model")

# Residuals kriging diagnostic
plot(pmrain, which = 6) # Semi-variogram and adjusted model
filled.contour(as.geomat(pmrain, "KrigedResiduals"), asp = 1,
    color.palette = terrain.colors, main = "Kriged residuals")
filled.contour(as.geomat(pmrain, "KrigeVariance"), asp = 1,
    color.palette = terrain.colors, main = "Kriged residuals variance")
# As we can expect, kriging variance is larger in the south/south-west part
# where density of stations is low

# AURELHY interpolation diagnostic plots
# Graph showing the importance of predicted versus kriged residuals for
# all observations
plot(pmrain, which = 7)  # Model prediction and kriged residuals

# Extract interpolated log(rain) and transform back into rain (mm)
geomrain <- as.geomat(pmrain)
geomrain <- 10^geomrain

# How is interpolated rain distributed?
range(geomrain, na.rm = TRUE)
range(mrain$rain)

# Ooops! We have some very high values! How many?
sum(geomrain > 1000, na.rm = TRUE)
# This is probably due to a lack of data at high altitudes
# Let's truncate them to 1000 for a better graph
geomrain[geomrain > 1000] <- 1000
# ... and plot the result
image(geomrain, col = topo.colors(12))
contour(geomrain, add = TRUE)
lines(mbord, col = "red")
points(mrain, col = "red")

# A better plot for these interpolated rain data
filled.contour(coords(geomrain, "x"), coords(geomrain, "y"), geomrain,
    asp = 1, xlab = "Longitude", ylab = "Latitude",
    main = "AURELHY interpolated rain data", key.title = title(sub = "Rain (mm)\n\n"), 
    color.palette = colorRampPalette(c("red", "gray", "blue"), bias = 2))


# One can experiment different interpolation parameters using update()
# Suppose we (1) don't want to scale PCs, (2) to keep only first 7 PCs,
# (3) we want an upgraded linear model like this:
# data <- a.x + b.y + c.z + d.PC3 + e.PC6 + f.PC7 + g.seadist + h.seadist^2 + i
# and (4) we want a Gaussian model for the semi-variogram
# (note that one can also regress against seadist2 <- seadist^2), just do:
# maurelhy2$seadist2 <- maurelhy$seadist^2
# Even with all these changes, you don't have to recompute maurelhy,
# just update() it and the costly steps of calculating landscape descriptors
# are reused (not the use of I() to protect calcs inside a formula)!
maurelhy2 <- update(maurelhy, scale = FALSE, nbr.pc = 7,
  model = data ~ x + y + z + PC3 + PC6 + PC7 + seadist + I(seadist^2),
  vgm = vgm(100, "Gau", 1, 1))
maurelhy2

# Diagnostic of the new PCA on land descriptors without scaling
summary(maurelhy2)
plot(maurelhy2)

# Interpolate with the new parameters
pmrain2 <- predict(maurelhy2, mrain, "logRain")
summary(pmrain2)

# A couple of graphs
plot(pmrain2, which = 1) # Residuals versus fitted (how residuals spread?)
plot(pmrain, which = 6) # Semi-variogram and adjusted model
plot(pmrain2, which = 7) # Model prediction and kriged residuals

#... Explore as much as you like until you find the set of parameters the suits you!
}

\keyword{utilities}
