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

# New add.points method
"add.points" <- function (x, ...)
	UseMethod("add.points")
	
# Augment a geomask with points near geopoints coordinates
"add.points.geomask" <- function (x, geopoints, ...)
{
	# Check arguments
	if (!inherits(x, "geomask"))
		stop("'x' must be a 'geomask' object")
	if (!inherits(geopoints, "geopoints"))
		stop("'geopoints' must be a 'geopoints' object")
	# Look for the points that are closest to the geomask grid in the 'geopoints' object
	# Quick calculation by transforming coordinates into their closest index
	# in the grid
	Coords <- coords(x)
	X = (geopoints$x - Coords["x"]) / Coords["size"]
	Y = (geopoints$y - Coords["y"]) / Coords["size"]
	# Round values for X and Y to match a grid point
	Xind = round(X) + 1
	Yind = round(Y) + 1
	# Eliminate points that are outside the grid
	In <- rep(TRUE, length(X))
	In[Xind < 1] <- FALSE
	In[Xind > nrow(x)] <- FALSE
	In[Yind < 1] <- FALSE
	In[Yind > ncol(x)] <- FALSE

	# Check which point is inside the grid, but not in the mask (avoid negative
	# indices... points eliminated anyway)
	toAdd <- !diag(x[abs(Xind), abs(Yind)]) & In
	# Add these points to the mask
	added <- data.frame(x = Xind[toAdd], y = Yind[toAdd])
	x[added$x, added$y] <- TRUE
	# ... and record the list of added points as "added" attribute
	added2 <- attr(x, "added")
	if (is.null(added2)) {
		attr(x, "added") <- added 
	} else {
		# Merge added points
		attr(x, "added") <- rbind(added2, added)
	}
	return(x)
}

# Distance to the sea from DEM
"dist2sea" <- function (geotm)
{
	# Edges: find sea pixels that have at least one terrestrian neighbour
	"externaledge" <- function (geotm, o) {
		edgematrix <- geotm
		edgematrix[edgematrix >= 0] <- 0
		for (i in 1:nrow(o)) {
		    if (o[i, 1] == 1) x1 <- o[i, 1] else x1 <- o[i, 1] - 1
		    if (o[i, 1] == nrow(geotm)) x2 <- o[i, 1] else x2 <- o[i, 1] + 1
		    if (o[i, 2] == 1) y1 <- o[i, 2] else y1 <- o[i, 2] - 1
		    if (o[i, 2] == ncol(geotm)) y2 <- o[i, 2] else y2 <- o[i, 2] + 1
		    if (min(is.na(geotm[x1:x2, y1:y2])) == 0)
				edgematrix[o[i, 1], o[i, 2]] <- 1
		}
		return(edgematrix)
	}

	dist2seaout <- geotm
    o <- which(is.na(dist2seaout), arr.ind = TRUE)
    edgematrix <- externaledge(dist2seaout, o)
    xyland <- which(edgematrix == 0,  arr.ind = TRUE)
    xyedge <- which(edgematrix == 1,  arr.ind = TRUE)
    cellsize <- coords(geotm)["size"]
    lat0 <- coords(geotm)["y1"]
    for (i in 1:nrow(xyland)) {
        dist2seaout[xyland[i, 1], xyland[i, 2]] <- min(sqrt(((xyland[i, 1] -
			xyedge[, 1]) * cellsize * 111.2 * cos((xyland[i,2] *
			cellsize + lat0) * pi / 180))^2 + ((xyland[i,2] - xyedge[,2]) *
			cellsize * 111.2)^2))
    }
    return(dist2seaout)
}
