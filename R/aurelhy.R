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
"aurelhy" <- function (geotm, geomask, landmask = auremask(), x0 = 30, y0 = 30,
step = 12, nbr.pc = 10, scale = FALSE, model = "data ~ .", vgmodel = vgm(1, "Sph", 10, 1),
add.vars = NULL, var.name = NULL)
{	
	call <- match.call()
	
	# Check that geotm and geomask are correct objects and are on a grid of same size
	if (!inherits(geotm, "geotm"))
		stop("geotm must be a 'geotm' object")
	if (!inherits(geomask, "geomask"))
		stop("geomask must be a 'geomask' object")
	if (nrow(geotm) != nrow(geomask) || ncol(geotm) != ncol(geomask) ||
		!isTRUE(all.equal(coords(geotm), coords(geomask))))
		stop("geotm and geomask must cover exactly the same area on the same grid")
	# Check lanbdmask
	if (!inherits(landmask, "auremask"))
		stop("landmask must be an 'auremask' object")
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

	# Make sure x0 and y0 are large enough and nx and ny are small enough
	# to leave space around
	maxdist <- max(attr(landmask, "dist"))
	type <- attr(landmask, "type")
	if (type == "rectangular") maxdist <- maxdist * (attr(landmask, "n") / 2)
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
	# Shouldn't we average elevation around these points???
	res$z <- as.vector(tm2)
	
	# Before further calculating, check that add.vars matches nrow(res)
	if (!is.null(add.vars)) {
		# add.vars can be a geomat => check dimensions
		#and transform into a column data frame
		if (inherits(add.vars, "geomat")) {
			if (dim(add.vars) != dim(tm2) ||
				!isTRUE(all.equal(coords(add.vars), coords(tm2),
					tolerance = coords(tm2)["size"] / 2))) {
				cat("The interpolation grid is:\n")
				print(tm2)
				flush.console()
				stop("You must use same grid for 'add.vars' as the interpolation grid")
			}
			add.vars <- as.vector(add.vars)
		}
		add.vars <- as.data.frame(add.vars)
		if (nrow(add.vars) != nrow(res))
			stop("Data provided in 'add.vars' must be measured at same grid points as interpolation")
		if (!is.null(var.name)) {
			if (length(var.name) != 1)
				stop("var.name must be of length 1, when add.vars is a 'geomat' object")
		}
	}
	
	# Replace NAs in geotm by 0's (supposed to be sea level)
	geotm[is.na(geotm)] <- 0
	
	# add 'mask' to the table
	res$mask <- as.vector(mask)
	# res2 contains only points we keep
	res2 <- res[as.vector(mask), ]

	# We choose a point in the middle of the original geotm object
	m <- band * size * 1.01
	xorig <- coords(geotm, "x")[nrow(geotm) %/% 2]
	yorig <- coords(geotm, "y")[ncol(geotm) %/% 2]
	xlim <- c(xorig - m, xorig + m)
	ylim <- c(yorig - m, yorig + m)
	# Take a window out of these data
	geotm2 <- window(geotm, xlim, ylim)
	pt <- coords(geotm2, "xy")
	if (type == "radial") {
		# Get the different groups of points for each sector
		pc <- polar.coords(geotm2, xorig, yorig, maxdist)
		# Make classes for angles and distances
		dists <- attr(landmask, "dist")
		angles <- attr(landmask, "angles")
		pc$dist <- cut(pc$dist, breaks = dists,  labels = 1:(length(dists) - 1))
		pc$angle <- cut(pc$angle, breaks = c(angles, 8),  labels = 1:length(angles))
		# Create a unique vector combining dist and angle to give a number to each sector
		pc$sector <- (as.numeric(pc$dist) - 1) * length(angles) + as.numeric(pc$angle)
	} else { # For a rectangular grid
		# Select rectangular grid sectors and look which points are in each
		# rectangle in the geomat's grid
		pc <- coords(geotm2, "xy")
		xcut <- unique(landmask$x) / 110.9 + xorig
		ycut <- unique(landmask$y) / 110.9 + yorig
		pc$x <- cut(pc$x, breaks = xcut,  labels = 1:(length(xcut) - 1))
		pc$y <- cut(pc$y, breaks = ycut,  labels = 1:(length(ycut) - 1))
		# Create a unique vector combining x and y to give a number to each sector
		pc$sector <- (as.numeric(pc$x) - 1) * (length(ycut) - 1) + as.numeric(pc$y)
		# If we don't keep origin, eliminate it from the sectors
		if (!attr(landmask, "keep.origin")) {
			Center <- (length(xcut) - 1) * (length(ycut) - 1) %/% 2 + 1
			pc$sector[pc$sector == Center] <- NA
			toDecrement <- (pc$sector > Center)
			toDecrement[is.na(toDecrement)] <- FALSE
			pc$sector[toDecrement] <- pc$sector[toDecrement] - 1
		}
	}
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
	Ncol <- length(unique(pc$sector))
	land <- matrix(0, nrow = nrow(res2), ncol = Ncol)
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
	Npts <- as.vector(table(pc$sector))
	# Eliminate null value (for the origin, with keep.origin == FALSE)
	Npts <- Npts[Npts != 0]
	div <- matrix(Npts, nrow = nrow(res2),
		ncol = Ncol, byrow = TRUE)
	land <- land / div
	# 4) Substract elevation of the point (tm2) to each column
	el <- res2$z
	# Replace NAs (sea level) by 0's
	el[is.na(el)] <- 0
	el <- matrix(el, nrow = nrow(res2), ncol = Ncol)
	land <- land - el 	# That makes the landscape descriptors
	
	# Perform a PCA on the land data
	pca <- prcomp(land, center = TRUE, scale. = scale)
	# Drop mask from res2
	res2$mask <- NULL
	
	# Add the nbr.pc principal components to res
	res <- cbind(res2, pca$x[, 1:nbr.pc])

	# Add variables, after masking points with same mask as for interpolation
	if (!is.null(add.vars)) {
		add.vars <- add.vars[mask, ]
		res <- cbind(res, add.vars)
		# Do we have a name for an additional variable to use instead?
		Names <- names(res)
		Names[length(Names)] <- var.name
		names(res) <- Names	
	}
	
	# This is an 'aurelhy' object
	class(res) <- c("aurelhy", "data.frame")
	# Record parameters
	attr(res, "call") <- call
	attr(res, "nbr.pc") <- nbr.pc
	attr(res, "scale") <- scale
	attr(res, "tm") <- tm2
	attr(res, "mask") <- mask
	attr(res, "land") <- land
	attr(res, "auremask") <- landmask
	attr(res, "sectors") <- pc
	attr(res, "pca") <- pca
	attr(res, "model") <- as.formula(model)
	attr(res, "vgm") <- vgmodel
	return(res)
}

# Print and plot methods for aurelhy objects
"print.aurelhy" <- function (x, ...)
{
	cat("An aurelhy object to interpolate points in a terrain model:\n")
	print(attr(x, "tm"))
	if (isTRUE(attr(x, "scale"))) {
		cat("Keeping ", attr(x, "nbr.pc"), " PCs from a PCA on scaled data\n",
			sep = "")
	} else {
		cat("\n\nKeeping ", attr(x, "nbr.pc"), " PCs from a PCA on non-scaled data\n",
			sep = "")
	}
	cat("\nPredictive variables are:\n")
	cat(paste(names(x), collapse = ", "))
	cat("\n\nRegression model is:\n")
	print(attr(x, "model"))
	cat("\nVariogram model is:\n")
	print(attr(x, "vgm"))
	return(invisible(x))
}

# The summary of the PCA
"summary.aurelhy" <- function (object, ...)
{
	summary(attr(object, "pca"))
}

# This is the screeplot of the PCA
"plot.aurelhy" <- function (x, y, main = "PCA on land descriptors", ...)
{
	pca <- attr(x, "pca")
	plot(pca, npcs = length(pca$sdev), main = main, ...)
}

# A points methods that add interpolation points to a map
"points.aurelhy" <- function (x, pch = ".", ...)
{
	points(x$x, x$y, pch = pch, ...)
}

# An update method for the aurelhy object
"update.aurelhy" <- function (object, nbr.pc, scale, model, vgmodel, ...)
{
	if (!missing(model)) attr(object, "model") <- as.formula(model)
	if (!missing(vgmodel))  attr(object, "vgm") <- vgmodel
	
	dropPCs <- function (x) {
		N <- names(x)
		N <- N[!grepl("^PC[0-9+$]", N)]
		Att$names <- N
		return(x[, N])
	}
	
	if (!missing(scale)) {
		scale <- isTRUE(scale)
		if (attr(object, "scale") != scale) {
			attr(object, "scale") <- scale
			# Redo the PCA analysis
			pca <- prcomp(attr(object, "land"),
				center = TRUE, scale. = scale)
			attr(object, "pca") <- pca
			# How many PCs do we keep?
			if (missing(nbr.pc)) nbr.pc <- attr(object, "nbr.pc")
			attr(object, "nbr.pc") <- nbr.pc
			# Add the nbr.pc principal components to res
			Att <- attributes(object)
			res <- cbind(dropPCs(object), pca$x[, 1:nbr.pc])
			Att$names <- names(res)
			attributes(res) <- Att
			return(res)
		}
	}

	if (!missing(nbr.pc)) {
		attr(object, "nbr.pc") <- nbr.pc
		pca <- attr(object, "pca")
		Att <- attributes(object)
		res <- cbind(dropPCs(object), pca$x[, 1:nbr.pc])
		Att$names <- names(res)
		attributes(res) <- Att
		return(res)
	} else return(object)
}

"as.geomat" <- function (x, ...)
	UseMethod("as.geomat")

# Transform any AURELHY predictor into a geomat object
"as.geomat.aurelhy" <- function (x, what = "PC1", nodata = NA, ...)
{
	# We look if what is one of the variables present in this object
	what <- as.character(what)[1]
	if (!what %in% names(x))
		stop("'what' must be one of the variables of the 'aurelhy' x object")
	dat <- x[[what]]
	pos <- as.numeric(rownames(x))	# Position of the points in the matrix
	# Start from the mask to create our geomat object
	mask <- attr(x, "mask")
	res <- matrix(nodata, nrow = nrow(mask), ncol = ncol(mask))
	attr(res, "coords") <- attr(mask, "coords")
	class(res) <- c("geomat", "matrix")
	# Fill corresponding points with the data
	res[pos] <- dat
	return(res)
}

# Predict creates the interpolation
"predict.aurelhy" <- function (object, geopoints, variable, v.fit = NULL, ...)
{
	# Check arguments
	if (!inherits(object, "aurelhy"))
		stop("'object' must be an 'aurelhy' object")
	if (!inherits(geopoints, "geopoints"))
		stop("'geopoints' must be a 'geopoints' object")
	variable <- as.character(variable)
	if (length(variable) < 1)
		stop("You must provide the name of a variable to interpolate")
	if (length(variable) > 1)
		stop("Cannot interpolate more than one variable at a time")
	if (!variable %in% names(geopoints))
		stop("variable must be the name of one column in the 'geopoints' object")
	# Look for the points that are closest to the grid in the 'geopoints' object
	# Quick calculation by transforming coordinates into their closest index
	# in the grid
	Mask <- attr(object, "mask")
	Coords <- coords(Mask)
	X = (geopoints$x - Coords["x"]) / Coords["size"]
	Y = (geopoints$y - Coords["y"]) / Coords["size"]
	# Make a data frame with data, coords and indices
	df <- data.frame(x = geopoints$x, y = geopoints$y,
		data = as.numeric(geopoints[, variable]),
		X = X, Y = Y, Xind = round(X) + 1, Yind = round(Y) + 1)
	# Eliminate points that are outside the grid
	In <- rep(TRUE, nrow(df))
	In[df$Xind < 1] <- FALSE
	In[df$Xind > nrow(Mask)] <- FALSE
	In[df$Yind < 1] <- FALSE
	In[df$Yind > ncol(Mask)] <- FALSE
	df <- df[In, ]
	# Check which point is in the mask
	df$OK <- diag(Mask[df$Xind, df$Yind])
	#
	# We can possibly check too if rejected points do not match either of the four
	# corners surrounding actual location (sometimes it happens for points near
	# or on the border of the mask)
	checkCorner <- function (df, mask, topX, topY) {
		nOK <- !df$OK
		Xout <- df$X[nOK]
		Yout <- df$Y[nOK]
		if (isTRUE(topX)) XoutC <- ceiling(Xout) + 1 else XoutC <- floor(Xout) + 1
		if (isTRUE(topY)) YoutC <- ceiling(Yout) + 1 else YoutC <- floor(Yout) + 1
		OKc <- diag(mask[XoutC, YoutC])
		if (any(OKc)) {
			nOKc <- nOK
			nOKc[nOKc] <- OKc
			df$Xind[nOKc] <- XoutC[OKc]
			df$Yind[nOKc] <- YoutC[OKc]
			df$OK[nOKc] <- TRUE
		}
		return(df)
	}
	# Check each of the four corners around actual coordinates to find a point in the grid
	if (!all(df$OK)) df <- checkCorner(df, Mask, TRUE, TRUE)
	if (!all(df$OK)) df <- checkCorner(df, Mask, TRUE, FALSE)
	if (!all(df$OK)) df <- checkCorner(df, Mask, FALSE, TRUE)
	if (!all(df$OK)) df <- checkCorner(df, Mask, FALSE, FALSE)
	# Keep only those points that are in the mask (df$OK)
	df <- df[df$OK, ]

	# Get AURELHY descriptors for the same points and merge them together with 'data'
	# Selection is done according to rownames of the aurelhy object, after converting
	# Xind and Yind into corresponding row names
	df$pos <- df$Xind + (nrow(Mask) * (df$Yind - 1))
	pred <- as.data.frame(object)
	# Keep only items that match data in df
	pred <- pred[match(df$pos, as.numeric(rownames(pred))), ]
	# Add data to this table
	pred$data <- df$data
	
	# Adjust a linear model on these data
	model <- attr(object, "model")
	lmod <- lm(model, data = pred)
	# Extract residuals
	pred$resid <- resid(lmod)
	# and predict values for all the points of the grid within the mask
	grid.mod <- predict(lmod, newdata = as.data.frame(object))
	# Print a very short information about adjusted R-squared value
	cat("[adjusted R-squared is ",
		round(summary(lmod)$adj.r.squared, digits = 3), "]\n", sep = "")

	# Adjust the semi-variogram with the model
	vgm <- attr(object, "vgm") # The variogram model to use
	Pred <- pred
	coordinates(Pred) <- ~ x + y
	v <- variogram(resid ~ x + y, Pred)
	if (is.null(v.fit)) # Fit the variogram model now, if not provided
		v.fit <- fit.variogram(v, vgm)

	# Krige residuals using this fitted variogram model
	# Note that krige method of gstat can only apply on rectangular areas
	# so, we need to krige on the whole bounding box, and then, select
	# points of interest using the mask
	Pred.grid <- coords(Mask, "xy")
	gridded(Pred.grid) <- ~ x + y
	k <- krige(resid ~ x + y, Pred, Pred.grid, model = v.fit)
	# Extract krigged residuals and keep only those corresponding to the mask
	k.resid <- k["var1.pred"]@data[Mask]
	names(k.resid) <- names(grid.mod)
	# Also extract kriging variance
	k.var <- k["var1.var"]@data[Mask]
	names(k.var) <- names(grid.mod)
	# The final result is the sum of the value predicted by the model (grid.mod)
	# and the kriged residuals (k.resid)
	res <- grid.mod + k.resid

	# Construct a predict.aurelhy object
	attr(res, "variable") <- variable
	attr(res, "mask") <- Mask
	attr(res, "predictors") <- pred
	attr(res, "model") <- model
	attr(res, "lm") <- lmod
	attr(res, "predicted") <- grid.mod
	attr(res, "vgm") <- vgm
	attr(res, "variogram") <- v
	attr(res, "v.fit") <- v.fit
	attr(res, "df") <- df
	attr(res, "krige.resid") <- k.resid
	attr(res, "krige.resid.var") <- k.var
	class(res) <- c("predict.aurelhy", "data.frame")	
	return(res)
}

# Print, summary and plot methods for predict.aurelhy objects
"print.predict.aurelhy" <- function (x, ...)
{
	cat("A predict.aurelhy object with interpolated values for variable '",
		attr(x, "variable"), "':\n", sep = "")
	cat("\nPredictive variables are:\n")
	pv <- names(attr(x, "predictors"))
	# We need to eliminate 'data' and 'resid' here (two last items)
	pv <- pv[-(length(pv) - 1:0)]
	cat(paste(pv, collapse = ", "))
	cat("\n\nRegression model (adjusted R-squared = ",
		round(summary(attr(x, "lm"))$adj.r.squared, digits = 3), ") is:\n", sep = "")
	print(attr(x, "model"))
	cat("Predicted values are distributed as follows:\n")
	print(summary(attr(x, "predicted")))
	cat("Residuals on provided predictors are distributed as follows:\n")
	print(summary(attr(x, "predictors")$resid))
	cat("\nVariogram model for universal kriging of the residuals is:\n")
	print(attr(x, "v.fit"))
	cat("Kriged residuals are distributed as follows:\n")
	print(summary(attr(x, "krige.resid")))
	cat("\nInterpolated values for '", attr(x, "variable"),
		"' are distributed as follows:\n", sep = "")
	print(summary(as.numeric(x)))
	return(invisible(x))
}

# The summary of the regression done on predictors
"summary.predict.aurelhy" <- function (object, ...)
{
	summary(attr(object, "lm"))
}

"plot.predict.aurelhy" <- function (x, y, which = 1, ...)
{
	which <- as.integer(which)[1]
	if (which > 0 && which <= 5) {
		# The five first graphs are residuals graphs for linear model
		plot(attr(x, "lm"), which = which, ...)
	} else if (which == 6) {
		# This is the variogram model
		v <- attr(x, "variogram")
		v.fit <- attr(x, "v.fit")
		v.model <- variogramLine(v.fit, max(v$dist), 50)
		plot(gamma ~ dist, data = v, xlab = "distance", ylab = "semivariance",
			ylim = c(0, max(v$gamma)),
			main = "Semivariogram model used for residuals kriging", ...)
		lines(gamma ~ dist, data = v.model, col  = "red", ...)
	} else if (which == 7) {
		# Graph showing the various components of AURELHY interpolation
		# versus observed values
		pr <- attr(x, "predictors")
		X <- pr$data
		loc <- as.numeric(rownames(pr))
		# Sort X in increasing order according to loc
		X <- X[order(loc)]
		loc <- sort(loc)
		sel <- names(x) %in% rownames(pr)
		mod <- attr(x, "predicted")[sel]
		krige <- attr(x, "krige.resid")[sel]
		df <- data.frame(X = X, reg = mod, krige.resid = krige, item = loc)
		# Create the graph
		plot(df$X, type = "n", xlab = "", ylab = "Data",
			ylim = c(min(df$X, df$pred), max(df$X, df$pred) * 1.1),
			main = "Components of AURELHY prediction", xaxt = "n")
		lines(df$reg, type = "h", col = 3)
		xcoords <- 1:nrow(df) + 0.1
		segments(xcoords, df$reg, xcoords, df$reg + df$krige.resid, col = 2)
		points(xcoords - 0.05, df$X, pch = "_", col = 1)
		axis(1, at = xcoords - 0.05, labels = df$item, las = 2)
		legend("top", c("observed", "linear model", "kriged residuals"),
			col = c(1, 3, 2), lty = 1, horiz = TRUE, inset = 0.02)
		# Return the table of data invisibly
		return(invisible(df))
	} else stop("'which' must be between 1 and 7")
}

"as.geomat.predict.aurelhy" <- function (x,
	what = c("Interpolated", "Predicted", "KrigedResiduals", "KrigeVariance"),
	nodata = NA, ...)
{
	# We extract either interpolated, predicted, or kriged residuals or variance
	what <- match.arg(what)
	dat <- switch(what,
		Interpolated = as.numeric(x),
		Predicted = as.numeric(attr(x, "predicted")),
		KrigedResiduals = as.numeric(attr(x, "krige.resid")),
		KrigeVariance = as.numeric(attr(x, "krige.resid.var")))
	pos <- as.numeric(names(x))	# Position of the points in the matrix
	# Start from the mask to create our geomat object
	mask <- attr(x, "mask")
	res <- matrix(nodata, nrow = nrow(mask), ncol = ncol(mask))
	attr(res, "coords") <- attr(mask, "coords")
	class(res) <- c("geomat", "matrix")
	# Fill corresponding points with the data
	res[pos] <- dat
	return(res)
}
