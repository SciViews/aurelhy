\name{unitTests.aurelhy}
\alias{unitTests.aurelhy}

\title{ Unit tests for the package aurelhy }
\description{
  Performs unit tests defined in this package by running
  \code{example(unitTests.aurelhy)}. Tests are in \code{runit*.R} files located
  in the '/unitTests' subdirectory or one of its subdirectories ('/inst/unitTests'
  and subdirectories in package sources).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\examples{
#if (require(svUnit)) {
#    clearLog()
	
  \dontrun{
    ## This test is now moved to the tests directory
	#runTest(svSuite("package:aurelhy"), "aurelhy")
 
  }
  \dontshow{
    ## Put here test units you want to run during R CMD check but
	## don't want to show or run with example(unitTests.aurelhy)
  }
    ## Check errors at the end (needed to interrupt R CMD check)
#    errorLog()
#}
}

\keyword{utilities}
