# Given a point, calculate angle and distance from grid data (geomat object)
"polar.coords" <- function (geomat, x, y, maxdist)
{
	if (!inherits(geomat, "geomat"))
		stop("'geomat' must be a 'geomat' object")
	if (!missing(maxdist) && !is.null(maxdist)) {
		# maxdist is in km, but x and y are in decimal degrees
		maxdeg <- maxdist / 110.9 # For latitudes: 110.9km/degree
		
		# Filter out data contained in a square of maxdist * 1.05
		m <- maxdeg * 1.05
		xlim <- c(x - m, x + m)
		ylim <- c(y - m, y + m)
		# Take a window out of these data
		geomat <- window(geomat, xlim, ylim)
	}
	
	coords <- coords(geomat, "xy")
	# Recenter coordinates around x and y
	X <- coords$x - x
	Y <- coords$y - y
	# Note: distances are in km
	angles <- atan2(Y, X)	# Angles go from -pi to pi, and we want 0 to 2 * pi
	angles <- ifelse(angles > 0, angles, 2 * pi + angles)
	res <- data.frame(angle = angles, dist = sqrt(X^2 + Y^2) * 110.9)
	attr(res, "geomat") <- geomat
	return(res)
}

# match.coords matches numerical data with a tol value
# TODO: optimize this, considering we have a grid!
"match.coords" <- function (points, table, tol = 0.002)
{
	# Look if point is in the tol vicinity of one point in table
	match.one <- function (point, table, tol)
		any(point[1] - tol < table$x && point[1] + tol > table$x &&
			   point[2] - tol < table$y && point[2] + tol > table$y)
	res <- apply(points[, c("x", "y")], 1, match.one, table = table, tol = tol)
	return(res)
}

# A new coords method
"coords" <- function (x, ...)
	UseMethod("coords")

# New resample method
"resample" <- function (x, ...)
	UseMethod("resample")