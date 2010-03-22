# We define these objects:
# * geomat is a matrix which represents a regular grid with info about its
#   geographical localization (coordinate of top-left point and cell size)
# * dem is a geomat object that contains integers and is used for terrain
#   model data (digital elevation map)
# * aspect is a geomat object that contains aspect data (orientation of the
#   slope as N, NE, E, SE, S, SO, O or NO) (TODO)

# Read in the terrain model in ASCII format (.asc)
# into a dem object (digital elevation model)
read.dem <- function (file, type = "ascii") {
	if (!file.exists(file) || file.info(file)$isdir)
		stop("'file' is not found or is a directory")
	# Currently, support only "ascii" type
	if (type != "ascii")
		stop("type must be \"ascii\" only for the moment")
		
	# For type = "ascii", we consider the following header:
	#ncols         6000
	#nrows         6000
	#xllcorner     -10 (or xllcenter)
	#yllcorner     35 (or yllcenter)
	#cellsize      0.00083333333333333
	#NODATA_value  -9999 (optional)
	head <- readLines(file, n = 6)
	nr <- as.numeric(sub("^ncols *", "", head[1]))
	nc <- as.numeric(sub("^nrows *", "", head[2]))
	x <- as.numeric(sub("^xllcorner *", "", head[3]))
	y <- as.numeric(sub("^yllcorner *", "", head[4]))
	size <- as.numeric(sub("^cellsize *", "", head[5]))
	miss <- as.numeric(sub("^NODATA_value *", "", head[6]))
	coords <- c(size = size, x = x, y = y)
	
	# Read the data in
	res <- scan(file, integer(0), skip = 6)
	# Rewrite missing data
	res[res == miss] <- NA
	# Reshape into a matrix
	res <- matrix(res, ncol = nc, nrow = nr)
	attr(res, "coords") <- coords
	class(res) <- c("dem", "geomat", "matrix")
	return(res)
}

# Get coords data from a geomat object
coords.geomat <- function (x, type = "par", ...) {
	par <- attr(x, "coords")
	par["x2"] <- par["x"]  + (nrow(x) + 1) * par["size"]
	par["y2"] <- par["y"]  + (ncol(x) + 1) * par["size"]
	res <- switch(type,
			par = par,
			x = 0:(nrow(x)-1) * par["size"] + par["x"],
			y = 0:(ncol(x)-1) * par["size"] + par["y"],
			xy = data.frame(
				x = rep(0:(nrow(x)-1) * par["size"] + par["x"], ncol(x)),
				y = rep(0:(ncol(x)-1) * par["size"] + par["y"], each = nrow(x))
			),
		   stop("Unrecognized 'type' argument"))
	return(res)
}

# Print a geomat object
print.geomat <- function (x, ...) {
	coords <- coords(x)
	nc <- ncol(x)
	nr <- nrow(x)
	size <- coords["size"]
	cat("A geomat object with a grid of", nc, "x", nr, "\n")
	cat("The cell size is ", size, "°, that is approx. ",
		size * 111000, " m in lat.\n", sep = "")
	cat("The grids spans from ", coords["x"], "° to ", coords["x2"],
		"° long.\n", sep = "")
	cat("            and from ", coords["y"], "° to ", coords["y2"],
		"° lat.\n", sep = "")
	cat("Data are of type", typeof(x), "\n")
	return(invisible(x))
}

# Resample method for geomat objects
resample.geomat <- function (x, x0 = 1, y0 = 1, nx = 100, step = NULL, ...) {
	nr <- nrow(x)
	nc <- ncol(x)
	coords <- coords(x)
	x0 <- round(x0[1])
	y0 <- round(y0[1])
	if (x0 < 1 || x0 > nr)
		stop("'x0' cannot be < 1 or > nrow(x)")
	if (y0 < 1 || y0 > nc)
		stop("'y0' cannot be < 1 or > ncol(x)")
	
	# Do we resample by nx?
	if (is.null(step)) {
		# Try to guess a reasonable value for step from nx
		step <- (nr - x0 + 1) %/% (nx - 1)		
	}
	
	# Resample by step
	step <- round(step[1])
	if (step < 1)
		stop("'step' cannot be < 1")
	
	# Construct the resampling indexes
	rex <- seq(from = x0, to = nr, by = step)
	rey <- seq(from = y0, to = nc, by = step)
	size <- coords["size"] * step
	
	# Construct the new grid
	attrib <- attributes(x)
	res <- x[rex, rey]
	# Dims has changed
	attrib$dim <- dim(res)
	# Coords must be recalculated
	coords["x"] <- coords["x"] + (x0 - 1) * coords["size"]
	coords["y"] <- coords["y"] + (y0 - 1) * coords["size"]
	coords["size"] <- size
	attrib$coords <- coords[1:3]
	attributes(res) <- attrib
	return(res)
}

window.geomat <- function (x, xlim, ylim) {
	# Make sure xlim and ylim have two values and the first one is lowest
	xlim <- sort(as.numeric(xlim[1:2]))
	ylim <- sort(as.numeric(ylim[1:2]))
	
	# Extract a window from the original grid in x
	coords <- coords(x)
	X <- coords(x, 'x')
	Y <- coords(x, 'y')
	
	# Do we keep some coordinates
	keepX <- X >= xlim[1] & X <= xlim[2]
	keepY <- Y >= ylim[1] & Y <= ylim[2]
	
	# Resample the matrix
	attrib <- attributes(x)
	res <- x[keepX, keepY]
	# Dims has changed
	attrib$dim <- dim(res)
	
	# Recalculate coords
	lagX <- sum(X < xlim[1])
	lagY <- sum(Y < ylim[1])
	coords["x"] <- coords["x"] + lagX * coords["size"]
	coords["y"] <- coords["y"] + lagY * coords["size"]
	attrib$coords <- coords[1:3]
	
	attributes(res) <- attrib
	return(res)
}

image.geomat <- function (x, max.xgrid = 500, col = terrain.colors(100),
	add = FALSE, xlab = "Longitude", ylab = "Latitude", asp = 1, ...) {

	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid) {
		x <- resample(x, nx = max.xgrid)
	}
	
	# Make a map plot (image)
	image(x = coords(x, "x"), y = coords(x, "y"), z = x[, ncol(x):1],
		col = col, add = add, xlab = xlab, ylab = ylab, asp = asp, ...)
	
	# Draw the longitude and latitude axes at top and right too
	if (!add) {
		axis(3)
		axis(4)
	}
	
	# Possibly add a legend
	# TODO...
	
	return(invisible(x))	
}
