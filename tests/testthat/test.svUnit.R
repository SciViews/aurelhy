# Run tests using svUnit
# This is a wrapper to run these from within testthat
# so that R Studio's Test function and devtools::test() can be used too
library(svUnit)
clearLog()
runTest(svSuite("package:aurelhy"), "aurelhy")
errorLog()
