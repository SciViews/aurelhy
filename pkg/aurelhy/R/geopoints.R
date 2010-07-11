# geopoints data for interpolation. A data frame with x and y variables, plus
# additional data
"geopoints" <- function (x)
{
	if (inherits(x, "geopoints")) return(x)	# Nothing to do
	if (inherits(x, "geoshapes")) {
		# Extract the dbf attribute from the geoshapes object
		res <- attr(x, "dbf")
		if (is.null(res))
		stop("No data associated with the shapes")
		# Check if columns x and y exist, otherwise, extract from the shapes
		Names <- names(res)
		OK <- FALSE
		if (!"x" %in% Names) {
			if ("X" %in% Names) {
				res$x <- as.numeric(as.character(res$X)) # Make sure factor is converted to numeric
				res$X <- NULL
				OK <- TRUE
			}
		} else {
			res$x <- as.numeric(as.character(res$x))	# Make sure factor is converted to numeric
			OK <- TRUE
		}
		if (OK) { # I got x already
			if (!"y" %in% Names) {
				if ("Y" %in% Names) {
					res$y <- as.numeric(as.character(res$Y)) # Make sure factor is converted to numeric
					res$Y <- NULL
				} else OK <- FALSE
			} else res$y <- as.numeric(as.character(res$y)) # Make sure factor is converted to numeric
		}
		if (!OK) {
			# Make sure x is not defined
			res$x <- NULL
			# Try to get x and y from the shapes
			xy <- as.data.frame(attr(x, "shapes"))
			# Make sure these are points and not polygons
			# => 1 line for each id
			if (length(unique(xy$id)) < nrow(xy))
				stop("Shapes appear to contain polygons, not points")
			# Sort according to id and eliminate id
			xy <- xy[order(as.numeric(xy$id)), ]
			xy$id <- NULL
			xy$x <- as.numeric(as.character(xy$x))
			xy$y <- as.numeric(as.character(xy$y))
			# Add x and y columns to res
			res <- cbind(res, xy)
		}
		# Change class
		class(res) <- c("geospoints", "data.frame")
		return(res)
	}
	if (!inherits(x, "data.frame"))
		stop("'x' must be a data frame or geroshapes object")
	
	# Create the geopoints object
	if (all(c("x", "y") %in% names(x))) {
		res <- x
		res$x <- as.numeric(as.character(res$x))
		res$y <- as.numeric(as.character(res$y))
	} else if (all(c("X", "Y") %in% names(x))) {
		res <- x
		Names <- names(x)
		# Convert 'X' and 'Y' in lowercase (R is case-sensitive!)
		Names[Names == "X"] <- "x"
		Names[Names == "Y"] <- "y"
		names(res) <- Names
		res$x <- as.numeric(as.character(res$x))
		res$y <- as.numeric(as.character(res$y))
 	}
	# Change class
	class(res) <- c("geopoints", "data.frame")
	return(res)
}

# Read a simple ESRI shapes file and format it as geopoints, or read a dbf file
"read.geopoints" <- function (File, format)
{
	# Check that format is correct
	if (missing(format)) {
		# Guess format from file extension
		format <- sub("^.+\\.([^.]+)$", "\\1", File)
	} 
	format <- tolower(as.character(format)[1])
	
	# If it is ESRI shapes, first read a geoshapes and transform into a geopoints
	if (format == "shp") {
		res <- geopoints(read.geoshapes(File, dbf = TRUE))
	} else if (format == "dbf") {
		# Read the dbf file directly
		res <- geopoints(read.dbf(File)$dbf)
	} else stop("format must be 'shp' or 'dbf'")
	return(res)
}

# Write a geopoints object as an ESRI shape file
"write.geopoints" <- function (x, file, arcgis = FALSE, ...)
{
	if (!inherits(x, "geopoints"))
		stop("'x' must be a 'geopoints' object")
	# Create shapes with Id, X and Y from the x and y columns of the geopoints object
	Id <- 1:nrow(x)
	df <- data.frame(Id = Id, X = as.numeric(x$x), Y = as.numeric(x$y))	
	df2 <- data.frame(Id = Id)
	# Convert to shapefile data
	res <- convert.to.shapefile(df, df2, "Id", 1) # 1 = point
	# Do we write also the associated dbf file
	x$Id <- NULL
	res$dbf$dbf <- data.frame(Id = Id, x)
	
	# write in into an ESRI shapefile
	write.shapefile(res, file, arcgis = arcgis)

	return(invisible(res))
}

# Print method for geopoints objects
"print.geopoints" <- function (x, ...)
{
	cat("A 'geopoints' object containing:\n")
	X <- x
	class(X) <- "data.frame"
	print(X)
	return(invisible(x))
}

# Add points to a graph
"points.geopoints" <- function (x, ...) {
	points(x$x, x$y, ...)
}
