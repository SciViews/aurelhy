
# aurelhy

[![Linux & OSX Build Status](https://travis-ci.org/phgrosjean/aurelhy.svg)](https://travis-ci.org/phgrosjean/aurelhy)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/phgrosjean/aurelhy?branch=master&svg=true)](http://ci.appveyor.com/project/phgrosjean/aurelhy)
[![Coverage Status](https://img.shields.io/codecov/c/github/phgrosjean/aurelhy/master.svg)
](https://codecov.io/github/phgrosjean/aurelhy?branch=master)
[![CRAN Status](http://www.r-pkg.org/badges/version/aurelhy)](http://cran.r-project.org/package=aurelhy)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Hydrometeorological interpolation using the AURELHY method. Binaries of latest devel versions can be downloaded from [appveyor](https://ci.appveyor.com/project/phgrosjean/aurelhy/build/artifacts).

_**Warning:** As of 16 July 2015, aurelhy is deprecated from R-Forge. Do not use it any more and refer to this Github repository._


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
