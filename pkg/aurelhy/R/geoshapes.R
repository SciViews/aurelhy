# Create a geoshapes object, either for a x,y data.frame, or from a list of
# such data frames
"geoshapes" <- function (x, name = "1")
{
	if (inherits(x, "geoshapes")) return(x)	# Nothing to do
	if (inherits(x, "data.frame") && c("x", "y") %in% names(x)) {
		# Convert into a list
		res <- list()
		res[[as.character(name)[1]]] <- data.frame(x = x$x, y = x$y)
	} else if (inherits(x, "list")) {
		# Check that each item in the list is a data frame with 'x' and 'y'
		# columns
		isOK <- function (x)
			return(inherits(x, "data.frame") && names(x) == c("x", "y"))
		test <- sapply(x, isOK)
		if (!all(test))
			stop("'x' must be a list of data frames with only columns 'x' and 'y'")
		res <- x
 	}
	# Change class
	class(res) <- c("geoshapes", "list")
	return(res)
}

# Read a simple ESRI shapes file
"read.geoshapes" <- function (shpFile)
{
	res <- convert.to.simple(read.shp(shpFile))
	# Rename into 'id', 'x', and 'y'
	names(res) <- tolower(names(res))
	# Split into a list of shapes
	res <- by(res, res[, 1], function(x) x[, -1])
	class(res) <- c("geoshapes", "list")
	attr(res, "call") <- NULL # Delete this attribute
	return(res)
}

"write.geoshapes" <- function (x, file,
type = c("polygon", "point", "polyLine"), ...)
{
	type <- match.arg(type)
	type <- switch(type,
		polygon = 5,
		point = 1,
		polyLine = 3,
		stop("Unrecognized type"))
	if (!inherits(x, "geoshapes"))
		stop("'x' must be a 'geoshapes' object")
	
	# We need to add a first column with index to each shape
	# and rename columns as 'Id', 'X', and 'Y'
	rework.shapes <- function (shape, item)
		data.frame(Id = rep(item, ncol(shape)), X = shape$x, Y = shape$y)
	
	items <- names(x)
	if (length(items) == 0) stop("No shapes found in 'x'")
	df <- data.frame(Id = character(0), X = numeric(0), Y = numeric(0))
	for (i in 1:length(items))
		df <- rbind(df, rework.shapes(x[[items[i]]], items[i]))	
	
	# Convert to shapefile data
	res <- convert.to.shapefile(df, data.frame(index = items), "index", type)
	# and write in into an ESRI shapefile
	write.shapefile(res, file, arcgis = TRUE)
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
	return(invisible(x))
}

# Add a polygon corresponding to one shape in a graph
"lines.geoshapes" <- function (x, which = 1, ...)
{
	# Get the shape
	shp <- x[[which]]
	# Draw a line for the shape, but we must not draw lines connecting
	# several separate paths. They are recognizable, because we go back to
	# the same coordinates more than once
	shp$y[duplicated(shp$x) & duplicated(shp$y)] <- NA
	lines(shp, ...)
	return(invisible(shp))
}

# Add points to a graph
"points.geoshapes" <- function (x, which = 1, ...)
	lines(x, which, type = "p", ...)
