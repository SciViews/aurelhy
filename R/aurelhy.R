# This is the aurelhy object that performs the interpolation. It processes in
# two steps:
# 1) Giving a geotm object and an auremask, an aurelhy object is constructed that
#    contains the landscape variables, and a PCA is run to simplify landscape
#    definition. At this point, one can check PCA results and decide how many
#    principal components to keep. One can also add more descriptive variables.
#
# 2) The predict method applied to the aurelhy object, providing a geopoints
#    object with data to interpolate do the actual interpolation.
#    A predict.aurelhy object is created that one can inspect (regression,
#    kriging? ...). One can also convert the result into a geomat object, using
#    the as.geomat() function
"aurelhy" <- function (geotm, geomask, auremask = auremask(), x0 = 30, y0 = 30,
step = 12, nbr.pc = 10, add.vars = NULL, scale = FALSE)
{	
	call <- match.call()
	
	# Check that geotm and geomask are correcto objects and are on a grid of same size
	if (!inherits(geotm, "geotm"))
		stop("geotm must be a 'geotm' object")
	if (!inherits(geomask, "geomask"))
		stop("geomask must be a 'geomask' object")
	if (nrow(geotm) != nrow(geomask) || ncol(geotm) != ncol(geomask) ||
		!isTRUE(all.equal(coords(geotm), coords(geomask))))
		stop("geotm and geomask must cover exactly the same area on the same grid")
	# Check auremask
	if (!inherits(auremask, "auremask"))
		stop("auremask must be an 'auremask' object")
	# Check x0, y0, step and nbr.pc
	x0 <- as.integer(x0)[1]
	y0 <- as.integer(y0)[1]
	step <- as.integer(step)[1]
	if (step < 1) stop("step must be a positive integer")
	nbr.pc <- as.integer(nbr.pc)[1]
	if (nbr.pc < 1) stop("nbr.pc must be a positive integer")
	
	# Resample the geomask and determine which points to keep
	geomask[is.na(geomask)] <- FALSE
	mask <- resample(geomask, x0 = x0, y0 = y0, step = step)
	# Restrict the area covered by tm2 and mask according to selected points
	Xkeep <- apply(mask, 1, any)
	Ykeep <- apply(mask, 2, any)
	# Can we eliminate lines in the grid on the left, or on the right?
	dropX0 <- sum(cumsum(Xkeep) == 0)
	dropX1 <- sum(cumsum(rev(Xkeep)) == 0)
	# Recalculate x0 and calculate nx to best fit this zone
	x0 <- x0 + (dropX0 * step)
	nx <- length(Xkeep) - dropX0 - dropX1
	# Can we eliminate lines in the grid on the top, or on the bottom?
	dropY0 <- sum(cumsum(Ykeep) == 0)
	dropY1 <- sum(cumsum(rev(Ykeep)) == 0)
	# Recalculate y0 and calculate ny to best fit this zone
	y0 <- y0 + (dropY0 * step)
	ny <- length(Ykeep) - dropY0 - dropY1

	# Make sure x0 and y0 are large enough and nx and ny are small enough to leave space around
	maxdist <- max(attr(auremask, "dist"))
	maxdeg <- maxdist / 110.9
	coords <- coords(geotm)
	size <- coords["size"]
	band <- ceiling(maxdeg / size - 1)
	if (x0 < band)
		stop("'x0' must be larger to leave enough space at left to calculate landscape variables")
	if (y0 < band)
		stop("'x0' must be larger to leave enough space at left to calculate landscape variables")
	if (nrow(geotm) - x0 - step * (nx - 1) < band)
		stop("geotm has not enough data at right to calculate landscape variables")
	if (ncol(geotm) - y0 - step * (ny - 1) < band)
		stop("geotm has not enough data at the bottom to calculate landscape variables")
	
	# Resample geotm and mask using these new limits
	# and calculate coordinates of points where to define landscape
	mask <- resample(geomask, x0 = x0, y0 = y0, step = step, nx = nx, ny = ny, strict = TRUE)
	tm2 <- resample(geotm, x0 = x0, y0 = y0, step = step, nx = nx, ny = ny, strict = TRUE)
	res <- coords(tm2, "xy")
	Xsize <- nrow(tm2)
	Ysize <- ncol(tm2)
	# Add 'z', elevation at these points
	# TODO: shouldn't we average elevation around these points???
	res$z <- as.vector(tm2)
	
	# Replace NAs in geotm by 0's (supposed to be sea level)
	geotm[is.na(geotm)] <- 0
	
	# add '.mask.' to the table
	mask <- as.vector(mask)
	res$mask <- mask
	# res2 contains only points we keep
	res2 <- res[res$mask, ]

	# We choose a point in the middle of the original geotm object
	m <- band * size * 1.01
	xorig <- coords(geotm, "x")[nrow(geotm) %/% 2]
	yorig <- coords(geotm, "y")[ncol(geotm) %/% 2]
	xlim <- c(xorig - m, xorig + m)
	ylim <- c(yorig - m, yorig + m)
	# Take a window out of these data
	geotm2 <- window(geotm, xlim, ylim)
	pt <- coords(geotm2, "xy")
	# TODO: this is obviously only for radial mask!
	# Get the different groups of points for each sector
	pc <- polar.coords(geotm2, xorig, yorig, maxdist)
	# Make classes for angles and distances
	dists <- attr(auremask, "dist")
	angles <- attr(auremask, "angles")
	pc$dist <- cut(pc$dist, breaks = dists,  labels = 1:(length(dists) - 1))
	pc$angle <- cut(pc$angle, breaks = c(angles, 8),  labels = 1:length(angles))
	# Create a unique vector combining dist and angle to give a number to each sector
	pc$sector <- (as.numeric(pc$dist) - 1) * length(angles) + as.numeric(pc$angle)
	# Create a vector of lags in x and y directions
	cx <- nrow(geotm2) %/% 2
	cy <- ncol(geotm2) %/% 2
	lags <- data.frame(x = rep(-cx:cx, ncol(geotm2)),
		y = rep(-cy:cy, each = nrow(geotm2)))
	pc <- cbind(lags, sector = pc$sector)
	# Keep only points that are in one sector
	pc <- pc[!is.na(pc$sector), ]

	# Calculation of landscape descriptors is done as follows:
	# 1) Create a matrix with 0's [max(pc$sector) columns and nrow(res2) rows]
	land <- matrix(0, nrow = nrow(res2), ncol = max(pc$sector))
	# 2) For each row in pc, resample geotm with correct lag, apply mask, and add these
	# elevations to the pc$sector's column of land
	for (i in 1:nrow(pc)) {
		tmlag <- resample(geotm, x0 = x0 + pc$x[i], y0 = y0 + pc$y[i],
			step = step, nx = nx, ny = ny, strict = TRUE)
		# Make sure we keep only a grid of same size as tm2, and check we have
		# enough data for that
		if (nrow(tmlag) < Xsize | ncol(tmlag) < Ysize)
			stop("Your 'geotm' object must be large enough to be able to calculate landscape descriptors for all points")
		sect <- pc$sector[i]
		land[, sect] <- land[, sect] + as.vector(tmlag)[mask]
	}
	# 3) Divide all columns by the number of points in the corresponding sector
	div <- matrix(as.vector(table(pc$sector)), nrow = nrow(res2),
		ncol = max(pc$sector), byrow = TRUE)
	land <- land / div
	# 4) Substract elevation of the point (tm2) to each column
	el <- res2$z
	# Replace NAs (sea level) by 0's
	el[is.na(el)] <- 0
	el <- matrix(el, nrow = nrow(res2), ncol = max(pc$sector))
	land <- land - el 	# That makes the landscape descriptors
	
	# Perform a PCA on the land data
	pca <- prcomp(land, center = TRUE, scale. = scale)
	# Drop mask from res2
	res2$mask <- NULL
	# Add variables, after filtering them with mask (if their number match res)
	if (!is.null(add.vars)) {
		# TODO: add variables...
	}
	
	# Add the nbr.pc principal components to res
	res <- cbind(res2, pca$x[, 1:nbr.pc])
	
	# This is an 'aurelhy' object
	class(res) <- c("aurelhy", "data.frame")
	# Record parameters
	attr(res, "call") <- call
	attr(res, "nbr.pc") <- nbr.pc
	attr(res, "auremask") <- auremask
	attr(res, "msk") <- pc
	attr(res, "land") <- land
	return(res)
}

# Print and plot methods for aurelhy objects
"print.aurelhy" <- function (x, ...)
{
	cat("An aurelhy object\n")
	# TODO...
	return(invisible(x))
}

# This is the screeplot of the PCA
"plot.aurelhy" <- function (x, y, ...)
{
	# TODO...
}

# Predict creates the interpolation
"predict.aurelhy" <- function (object, geopoints, model, ...)
{
	# TODO: predict the interpolation...
}
