## runitaurelhy.R test suite
## by Ph. Grosjean <phgrosjean@sciviews.org>
## Run it simply by example(unitTests.ENtest) TODO: adapt this!

## Create a few objects we need for our tests
# ...

## Create a very simple 'svTest' object
test_R <- svTest(function () {
	checkTrue(1 < 2)
})

## The test cases
.setUp <- function () {
	## Executed before each test function
	library(aurelhy)

	## Create a function (just an example, replace with real code here)
	foo <- function(x) return(x)
}

.tearDown <- function () {
	## Executed after each test function
	## Restore previous exclusion list
	# ...
	## Remove our object with tests in .GlobalEnv
	rm(foo, envir = .GlobalEnv)
}

testAurelhy <- function () {
	checkEqualsNumeric(c(size = 0.1, x = 10, y = 20),
		attr(geomat(matrix(1, nrow = 10), 0.1, 10, 20), "coords"),
		msg = "Coords of a simple geomat() object")
	checkTrue(FALSE, msg = "Is FALSE true?")
	checkException(log("a"))
	checkTrue()
}
