# Create a geoshapes object, either for a x,y data.frame, or from a list of
# such data frames
"geoshapes" <- function (x, name = "1", dbf = NULL)
{
	if (inherits(x, "geoshapes")) return(x)	# Nothing to do
	if (!is.null(dbf) && !inherits(dbf, "data.frame"))
		stop("'dbf' must be a data frame or NULL")
	if (inherits(x, "data.frame") && all(c("x", "y") %in% names(x))) {
		# Convert into a list
		res <- list()
		res[[as.character(name)[1]]] <- data.frame(x = x$x, y = x$y)
	} else if (inherits(x, "list")) {
		# Check each item in the list is a data frame with 'x' and 'y' columns
		isOK <- function (x)
			return(inherits(x, "data.frame") && all(names(x) == c("x", "y")))
		test <- sapply(x, isOK)
		if (!all(test))
			stop("'x' must be a list of data frames with only columns 'x' and 'y'")
		res <- x
 	}
	# Add the dbf attribute
	attr(res, "dbf") <- dbf
	# Change class
	class(res) <- c("geoshapes", "list")
	return(res)
}

# Read a simple ESRI shapes file
"read.geoshapes" <- function (shpFile, dbf = TRUE)
{
	shapes <- convert.to.simple(read.shp(shpFile))
	# Rename into 'id', 'x', and 'y'
	colnames(shapes) <- tolower(colnames(shapes))
	# Split into a list of shapes
	res <- by(shapes, shapes[, 1], function(x) x[, -1])
	class(res) <- c("geoshapes", "list")
	attr(res, "shapes") <- shapes
	attr(res, "call") <- NULL # Delete this attribute
	# Do we read the dbf file too?
	dbfFile <- sub("[.][sS][hH][pP]$", ".dbf", shpFile)
	if (isTRUE(dbf) && file.exists(dbfFile)) {
		dbf <- read.dbf(dbfFile)$dbf
		attr(res, "dbf") <- dbf
	}
	return(res)
}

"write.geoshapes" <- function (x, file,
type = c("polygon", "point", "polyLine"), dbf = TRUE, arcgis = FALSE, ...)
{
	type <- match.arg(type)
	type <- switch(type,
		polygon = 5,
		point = 1,
		polyLine = 3,
		stop("Unrecognized type"))
	if (!inherits(x, "geoshapes"))
		stop("'x' must be a 'geoshapes' object")

	df <- attr(x, "shapes")
	if (is.null(df)) {
		# The object does not contain a shapes attribute
		# We must reconstitute the shapes data from the object
		n <- names(x)
		df <- data.frame(id = character(0), x = numeric(0), y = numeric(0))
		for (i in 1:length(n)) {
			dat <- x[[n[i]]][, c("x", "y")]
			dat <- data.frame(id = rep(n[i], ncol(dat)), dat,
				stringsAsFactors = FALSE)
			df <- rbind(df, dat)
		}
	}
	colnames(df) <- c("Id", "X", "Y")
	
	# Convert to shapefile data
	res <- convert.to.shapefile(as.data.frame(df),
		data.frame(index = unique(df[, "Id"])), "index", type)
	# Do we write also the associated dbf file
	dbf <- attr(x, "dbf")
	if (!is.null(dbf)) res$dbf$dbf <- dbf
	
	# write in into an ESRI shapefile
	write.shapefile(res, file, arcgis = arcgis)

	return(invisible(res))
}

"print.geoshapes" <- function (x, ...)
{
	L <- length(x)
	if (L == 1) {
		cat("A 'geoshapes' object containing one shape\n")
	} else {
		cat("A 'geoshapes' object containing", L, "shapes:\n")
		print(names(x))
	}
	# Is there a dbf attribute?
	dbf <- attr(x, "dbf")
	if (!is.null(dbf) && !is.null(names(dbf))) {
		cat("Associated data (first few lines):\n")
		print(head(dbf, n = 5))
	}
	return(invisible(x))
}

# Add a polygon corresponding to one shape in a graph
"lines.geoshapes" <- function (x, which = 1, ...)
{
	# Get the shape
	shp <- x[[which]]
	names(shp) <- c("x", "y")	# Sometimes, it is 'X' and 'Y'!
	# Draw a line for the shape, but we must not draw lines connecting
	# several separate paths. They are recognizable, because we go back to
	# the same coordinates more than once
	shp$y[duplicated(shp$x) & duplicated(shp$y)] <- NA
	lines(shp, ...)
	return(invisible(shp))
}

# Add points to a graph
"points.geoshapes" <- function (x, which = "all", ...) {
	pts <- attr(x, "shapes")
	# Get the points selected by which, if not all
	if (which != "all")  pts <- pts[pts$id %in% which, ]
	points(pts$x, pts$y, ...)
}
