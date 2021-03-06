
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recovery

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis build
status](https://travis-ci.org/JSzitas/recovery.svg?branch=master)](https://travis-ci.org/JSzitas/recovery)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JSzitas/recovery?branch=master&svg=true)](https://ci.appveyor.com/project/JSzitas/recovery)
[![Codecov test
coverage](https://codecov.io/gh/JSzitas/recovery/branch/master/graph/badge.svg)](https://codecov.io/gh/JSzitas/recovery?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/recovery)](https://CRAN.R-project.org/package=recovery)
<!-- badges: end -->

## About

**recovery** is the simplest debugging tool you could wish for. It
supports debugging the **dumb** way - running functions line by line,
collecting objects, and returning the line which finally fails. This is
somewhat inefficient, and is not advised for debugging computationally
intensive functions. Nonetheless, for computationally unintensive tasks
(or tasks whose computational requirements can be made reasonably small)
it tends to return better messages than most other debugging tools. It
will return, particularly, the line which caused the code to crash, the
objects which were in scope at **that** time, and their values.

**recovery** is dependency free and written entirely in **R**. What few
other packages are used are used exclusively for testing and package
coverage - you do not need these to make use of the package.

## Examples

TBD(see documented examples within the package in the meantime).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JSzitas/recovery")
```
