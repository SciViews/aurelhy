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
"aurelhy" <- function (geotm, geomask, auremask, x0 = 30, y0 = 30, step = 12,
nbr.pc = 10, add.vars = NULL)
{	
	call <- match.call()
	
	# Resample geotm and calculate coordinates of points where to define landscape
	tm2 <- resample(geotm, x0 = x0, y0 = y0, step = step)
	res <- coords(tm2, "xy")
	# Add 'z', elevation at these points
	res$z <- as.vector(tm2)
	
	# Replace NAs in geotm by 0's (supposed to be sea level)
	geotm[is.na(geotm)] <- 0
	
	# Resample the geomask the same way and determine which points to keep
	mask <- resample(geomask, x0 = x0, y0 = y0, step = step)
	# All NA values are set to FALSE
	mask[is.na(mask)] <- FALSE
	# add '.mask.' to the table
	mask <- as.vector(mask)
	res$.mask. <- mask
	# res2 contains only points we keep
	res2 <- res[res$.mask., ]

	# We choose a point in the middle of the original geotm object
	coords <- coords(geotm)
	xorig <- coords(geotm, "x")[nrow(geotm) %/% 2]
	yorig <- coords(geotm, "y")[ncol(geotm) %/% 2]
	maxdist <- max(attr(auremask, "dist"))
	maxdeg <- maxdist / 110.9
	m <- maxdeg * 1.05
	xlim <- c(xorig - m, xorig + m)
	ylim <- c(yorig - m, yorig + m)
	# Take a window out of these data
	geotm2 <- window(geotm, xlim, ylim)
	pt <- coords(geotm2, "xy")
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
	nx <- nrow(geotm2) %/% 2
	ny <- ncol(geotm2) %/% 2
	lags <- data.frame(x = rep(-nx:nx, ncol(geotm2)),
		y = rep(-ny:ny, each = nrow(geotm2)))
	pc <- cbind(lags, sector = pc$sector)
	# Keep only points that are in one sector
	pc <- pc[!is.na(pc$sector), ]

	# Calculation of landscape descriptors is done as follows:
	# 1) Create a matrix with 0's [max(pc$sector) columns and nrow(res2) rows]
	land <- matrix(0, nrow = nrow(res2), ncol = max(pc$sector))
	# 2) For each row in pc, resample geotm with correct lag, apply mask, and add these
	# elevations to the pc$sector's column of land
	for (i in 1:nrow(pc)) {
		tmlag <- resample(geotm, x0 = x0 + pc$x[i], y0 = y0 + pc$y[i], step = step)
		sect <- pc$sector[i]
		land[, sect] <- land[, sect] + as.vector(tmlag)[mask]
	}
	# 3) Divide all columns by the number of points in the corresponding sector
	div <- matrix(as.vector(table(ps$sector)), nrow = nrow(res2),
		ncol = max(pc$sector), byrow = TRUE)
	land <- land / div
	# 4) Substract elevation of the point (tm2) to each column
	el <- res2$z
	# Replace NAs (sea level) by 0's
	el[is.na(el)] <- 0
	el <- matrix(el, nrow = nrow(res2), ncol = max(pc$sector))
	land <- land - el 	# That makes the landscape descriptors
attr(res, "msk") <- pc
attr(res, "land") <- land
	# This is an 'aurelhy' object
	class(res) <- c("aurelhy", "data.frame")
	# Record parameters
	attr(res, "call") <- call
	attr(res, "nbr.pc") <- nbr.pc
	attr(res, "auremask") <- auremask
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
