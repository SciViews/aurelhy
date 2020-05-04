
# aurelhy

<!-- badges: start -->
[![Linux Build Status](https://travis-ci.com/phgrosjean/aurelhy.svg)](https://travis-ci.com/phgrosjean/aurelhy)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/phgrosjean/aurelhy?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/aurelhy)
[![Coverage Status](https://img.shields.io/codecov/c/github/phgrosjean/aurelhy/master.svg)
](https://codecov.io/github/phgrosjean/aurelhy?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/aurelhy)](https://cran.r-project.org/package=aurelhy)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Life
cycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

> Hydrometeorological interpolation using the AURELHY method. 

## Installation

Make sure you have the **devtools** R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the **aurelhy** package:

```r
devtools::install_github("phgrosjean/aurelhy")
```

R should install all required dependencies automatically, and then it should compile and install **aurelhy**.

Latest devel version in source and Windows binaires format also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/aurelhy/build/artifacts).

## Usage

Make the **aurelhy** package available in your R session:

```r
library("aurelhy")
```

Get help about this package:

```r
library(help = "aurelhy")
?aurelhy
```

## Code of Conduct

Please note that the 'SciViews' project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Note to developers

This package used to be developed on R-Forge in the past. However, the latest [R-Forge version](https://r-forge.r-project.org/projects/sciviews/) was moved to this Github repository on 2015-07-16. **Please, do not use R-Forge anymore for aurelhy development, use this Github repository instead.**
