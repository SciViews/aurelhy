# We define these objects:
# * geomat is a matrix which represents a regular grid with info about its
#   geographical localization (coordinate of top-left point and cell size)
# * geotm is a geomat object that contains integers and is used for terrain
#   model data (digital elevation map)

# Create a geomat object from a matrix of numbers (reals, integers, or logicals)
"geomat" <- function (x, size, xcenter, ycenter,
coords = c(size = size, x = xcenter, y = ycenter),
datatype = c("numeric", "integer", "logical"), nodata = NA)
{
	# datatype can be "numeric", "integer" or "logical"
	datatype <- match.arg(datatype)
	nr <- nrow(x)
	nc <- ncol(x)
	if (is.null(nr) || is.null(nc))
		stop("'x' must be a bidimensional grid (a matrix or a data frame)")
	# Deal with nodata
	if (!is.na(nodata)) x[x == nodata] <- NA
	# Convert x according to datatype
	res <- switch(datatype,
		numeric = matrix(as.numeric(x), ncol = nc, nrow = nr),
		integer = matrix(as.integer(x), ncol = nc, nrow = nr),
		logical = matrix(as.logical(x), ncol = nc, nrow = nr),
		stop("Unrecognized 'datatype'"))
	# Add the coords attribute and change the class
	attr(res, "coords") <- coords
	class(res) <- c("geomat", "matrix")
	return(res)
}

# Create a geotm object, which is essentially inheriting from a 'geomat' object
# but contains integer values
"geotm" <- function (x, size, xcenter, ycenter,
coords = c(size = size, x = xcenter, y = ycenter))
{
	res <- geomat(x = x, coords = coords, datatype = "integer")
	class(res) <- c("geotm", class(res))
	return(res)
}

# Create a geomask object, which is a geomat containing boolean (logical) data
"geomask" <- function (x, size, xcenter, ycenter,
coords = c(size = size, x = xcenter, y = ycenter))
{
	res <- geomat(x = x, coords = coords, datatype = "logical")
	class(res) <- c("geomask", class(res))
	return(res)
}

# Read a geomat object from an ARC/INFO ASCII GRID format (.asc, or .E00)
"read.geomat" <- function (file, type = "ascii",
datatype = c("numeric", "integer", "logical"), ...)
{
	if (!file.exists(file) || file.info(file)$isdir)
		stop("'file' is not found or is a directory")
	# Currently, support only "ascii" type
	if (type != "ascii")
		stop("type can only be \"ascii\" for the moment")
	
	# datatype can be "numeric", "integer" or "logical"
	datatype <- match.arg(datatype)
		
	# For type = "ascii", we consider the following header:
	#ncols         6000					# Integer
	#nrows         6000					# Integer
	#xllcorner     -10 (or xllcenter)	# Western (left) corner, real
	#yllcorner     35 (or yllcenter)	# Southern (bottom) corner, real
	#cellsize      0.00083333333333333	# Real
	#NODATA_value  -9999 (optional)		# Code for missing data, real
	#List of the raster values, starting at upper-left corner
	#reals separated by a single space
	#Note: for reals, decimal separator is always point ".", and is optional
	#Note2: it is'nt clear if the format is case-sensitive, but it seems not!
	head <- tolower(readLines(file, n = 6))
	nr <- as.integer(sub("^ncols *", "", head[1]))
	nc <- as.integer(sub("^nrows *", "", head[2]))
	# Do we have xllcorner/yllcorner, or xllcenter/yllcenter (no mixing allowed)
	if (grepl("xllcorner", head[3])) {
		x <- as.numeric(sub("^xllcorner *", "", head[3]))
		y <- as.numeric(sub("^yllcorner *", "", head[4]))
		size <- as.numeric(sub("^cellsize *", "", head[5]))
		# We have to calculate centers of squares instead!
		x <- x + size/2
		y <- y + size/2
	} else {
		x <- as.numeric(sub("^xllcenter *", "", head[3]))
		y <- as.numeric(sub("^yllcenter *", "", head[4]))
		size <- as.numeric(sub("^cellsize *", "", head[5]))
	}
	# As the missing data is optional, check for its presence
	if (grepl("^nodata_value", head[6])) {
		nodata <- as.numeric(sub("^nodata_value *", "", head[6]))
		nskip <- 6
	} else {
		nodata <- NA
		nskip <- 5
	}
	# In our geomat object, we always refer to the center of each square!
	coords <- c(size = size, x = x, y = y)
	
	# Read the data in (possibly using integers to save memory space)
	res <- switch(datatype,
		numeric = scan(file, numeric(0), skip = nskip, quiet = TRUE),
		integer = scan(file, integer(0), skip = nskip, quiet = TRUE),
		logical = scan(file, integer(0), skip = nskip, quiet = TRUE), # Only numbers supported!
		stop("Unrecognized 'datatype'"))
	res <- matrix(res, ncol = nc, nrow = nr)
	res <- res[, ncol(res):1]
	# Create a geomat object with these data
	return(geomat(res, coords = coords, datatype = datatype, nodata = nodata))
}

# Read in the terrain model in ARC/INFO ASCII GRID format (.asc)
# into a geotm object (digital elevation model)
"read.geotm" <- function (file, type = "ascii", ...)
{
	# Delegate to read.geomat()
	res <- read.geomat(file = file, type = type, datatype = "integer", ...)
	# Only the class is different
	class(res) <- c("geotm", class(res))
	return(res)
}

# Read a geomask (essentially a geomat containing integers, and then, apply
# a threshold to indicate what is TRUE and what is FALSE)
"read.geomask" <- function (file, type = "ascii", threshold = 0, ...)
{
	# Delegate to read.geomat() to read integers
	# and then, apply the threshold to calculate TRUE/FALSE values
	res <- read.geomat(file = file, type = type, datatype = "integer", ...)
	res <- res > as.integer(threshold)[1]
	# The class is different
	class(res) <- c("geomask", class(res))
	return(res)
}

# Write a geomat object into a different format in a file
# Currently, only ARC/INFO ASCII GRID format (.asc, or .E00)
"write.geomat" <- function (x, file, type = "ascii", integers = FALSE,
nodata = -9999, ...)
{
	# Currently, support only "ascii" type
	if (type != "ascii")
		stop("type can only be \"ascii\" for the moment")
		
	nodata <- as.numeric(nodata)[1]
	if (isTRUE(integers)) nodata <- as.integer(nodata)
	
	# Write the "header" being:
	#ncols         6000					# Integer
	#nrows         6000					# Integer
	#xllcorner     -10 (or xllcenter)	# Western (left) corner, real
	#yllcorner     35 (or yllcenter)	# Southern (bottom) corner, real
	#cellsize      0.00083333333333333	# Real
	#NODATA_value  -9999 (optional)		# Code for missing data, real
	#List of the raster values, starting at upper-left corner
	#reals separated by a single space
	#Note: for reals, decimal separator is always point ".", and is optional
	#Note2: it is'nt clear if the format is case-sensitive, but it seems not!
	
	coords <- attr(x, "coords")
	# Check these coordinates
	if (is.null(coords))
		stop("'x' does not seems to be a valid object, or is corrupted: no 'coords' attribute found")
	if (!is.numeric(coords) || is.null(names(coords)) || !c("size", "x", "y") %in% names(coords))
		stop("'x' coords seems to be corrupted (must contain 'size', 'x', and 'y')")
		
	cat("ncols ", nrow(x), "\n", file = file)
	cat("nrows ", ncol(x), "\n", file = file, append = TRUE)
	cat("xllcorner ", coords["x"] - coords["size"]/2, "\n", file = file, append = TRUE)
	cat("yllcorner ", coords["y"] - coords["size"]/2, "\n", file = file, append = TRUE)
	cat("cellsize ", coords["size"], "\n", file = file, append = TRUE)
	cat("nodata_value ", nodata, "\n", file = file, append = TRUE)
	
	# Convert data into numeric or integer
	if (isTRUE(integers)) {
		x <- matrix(as.integer(x), ncol = ncol(x))
	} else {
		x <- matrix(as.numeric(x), ncol = ncol(x))
	}
	# Replace any missing value in the matrix with nodata
	x[is.na(x)] <- nodata
	x <- t(x[, ncol(x):1])
	# Write this matrix into the file
	apply(x, 1, function (x) cat(paste(x, collapse = " "), "\n", sep = "",
		file = file, append = TRUE))
	# Return TRUE invisibly
	return(invisible(TRUE))
}

# Write a geotm object into a different format in file
# This is essentially the same process as write.geomat()
"write.geotm" <- function (x, file, type = "ascii", nodata = -9999, ...)
	write.geomat(x = x, file = file, type = type, integers = TRUE, nodata = nodata, ...)

# Write a geomask object into a different format in file
# This is essentially the same process as write.geomat()
"write.geomask" <- function (x, file, type = "ascii", nodata = -9999, ...)
	write.geomat(x = x, file = file, type = type, integers = TRUE, nodata = nodata, ...)

# Get coords data from a geomat object
"coords.geomat" <- function (x, type = "par", ...)
{
	par <- attr(x, "coords")
	# This is the top-left and bottom-right points covered by the grid
	par["x1"] <- par["x"] - par["size"]/2
	par["y1"] <- par["y"] - par["size"]/2
	par["x2"] <- par["x1"]  + nrow(x) * par["size"]
	par["y2"] <- par["y1"]  + ncol(x) * par["size"]
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
"print.geomat" <- function (x, ...)
{
	coords <- coords(x)
	nc <- ncol(x)
	nr <- nrow(x)
	size <- coords["size"]
	cl <- class(x)[1]
	cat("A", cl, "object with a grid of", nr, "x", nc, "\n")
	cat("The cell size is ", size, "\u00b0, that is approx. ",
		round(size * 110900), " m in lat.\n", sep = "")
	cat("The grid spans from ", coords["x1"], "\u00b0 to ", coords["x2"],
		"\u00b0 long.\n", sep = "")
	cat("           and from ", coords["y1"], "\u00b0 to ", coords["y2"],
		"\u00b0 lat.\n", sep = "")
	cat("Data are of type", typeof(x), "\n")
	return(invisible(x))
}

# Resample method for geomat objects
"resample.geomat" <- function (x, x0 = 1, y0 = 1, step = NULL, nx = 100, ny = nx, strict = FALSE, ...)
{
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
	
	# If we decide to get strict, make sure to respect nx and ny!
	if (isTRUE(strict)) {
		if (length(rex) > nx) rex <- rex[1:nx]
		if (length(rey) > ny) rey <- rey[1:ny]
	}
	
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

"window.geomat" <- function (x, xlim, ylim, ...)
{
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

"plot.geomat" <- function (x, y = NULL, max.xgrid = 100, nlevels = 50,
color.palette = terrain.colors, xlab = "Longitude", ylab = "Latitude",
asp = 1, ...)
{
	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid)
		x <- resample(x, nx = max.xgrid)
	# Plot the data
	filled.contour(coords(x, "x"), coords(x, "y"), x, nlevels = nlevels,
		color.palette = color.palette, xlab = xlab, ylab = ylab, asp = asp, ...)
	return(invisible(x))	
}

"image.geomat" <- function (x, max.xgrid = 500, col = terrain.colors(50),
add = FALSE, xlab = if (add) "" else "Longitude",
ylab = if (add) "" else "Latitude", asp = 1, ...)
{
	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid)
		x <- resample(x, nx = max.xgrid)
	# Plot the data
	image(coords(x, "x"), coords(x, "y"), x, col = col, add = add, xlab = xlab, ylab = ylab,
		asp = asp, ...)
	
	# Draw the longitude and latitude axes at top and right too
	if (!isTRUE(add)) {
		axis(3)
		axis(4)
	}
	return(invisible(x))	
}

"contour.geomat" <- function (x, max.xgrid = 100, nlevels = 10, col = par("fg"),
add = FALSE, xlab = if (add) "" else "Longitude",
ylab = if (add) "" else "Latitude", asp = 1, ...)
{	
	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid)
		x <- resample(x, nx = max.xgrid)
	# Plot the data
	contour(coords(x, "x"), coords(x, "y"), x, nlevels = nlevels, col = col,
		add = add, xlab = xlab, ylab = ylab, asp = asp, ...)
	
	# Draw the longitude and latitude axes at top and right too
	if (!isTRUE(add)) {
		axis(3)
		axis(4)
	}
	return(invisible(x))	
}

"persp.geomat" <- function (x, max.xgrid = 500, col = "green3",
xlab = "Longitude", ylab = "Latitude", asp = 1, theta = 10, phi = 30,
expand = 1, shade = 0.75, border = NA, box = TRUE, ...)
{
	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid)
		x <- resample(x, nx = max.xgrid)
	# Plot the data
	persp(coords(x, "x"), coords(x, "y"), x/1000, theta = theta, phi = phi,
		scale = FALSE, expand = expand, shade = shade, border = border,
		box = box, col = col, xlab = xlab, ylab = ylab, ...)
	return(invisible(x))	
}

"image.geomask" <- function (x, max.xgrid = 500, col = c("#ffffff80", "#88888800"),
add = TRUE, xlab = if (add) "" else "Longitude",
ylab = if (add) "" else "Latitude", asp = 1, ...)
{
	# Avoid trying to plot too large data by resampling in the grid in this case
	if (nrow(x) > max.xgrid)
		x <- resample(x, nx = max.xgrid)
	# Plot the data
	image(coords(x, "x"), coords(x, "y"), x, col = col, add = add,
		xlab = xlab, ylab = ylab, asp = asp, ...)
	
	# Draw the longitude and latitude axes at top and right too
	if (!isTRUE(add)) {
		axis(3)
		axis(4)
	}
	return(invisible(x))	
}
