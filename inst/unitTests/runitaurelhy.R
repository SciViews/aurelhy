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
	# ...

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
	checkTrue(TRUE, "a fake test... replace with real one!")
	checkException(log("a"), "log(\"a\") raises an exception")
}
