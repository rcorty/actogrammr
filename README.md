
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/rcorty/actogrammr.svg?branch=master)](https://travis-ci.org/rcorty/actogrammr) [![Coverage Status](https://img.shields.io/codecov/c/github/rcorty/actogrammr/master.svg)](https://codecov.io/github/rcorty/actogrammr?branch=master)

actogrammr
==========

The goal of actogrammr is to read in data on cirdacian activity, process it, and plot it, e.g. in an actogram.

Installation
------------

You can install the latest release of actogrammr with:

``` r
install.packages(pkgs = 'actogrammr')
```

Or, if you want the newest features, you can install from github:

``` r
# install.packages("devtools")
devtools::install_github("rcorty/actogrammr")
```

Data Import
-----------

``` r
library(actogrammr)

f <- file.path(system.file(package = 'actogrammr'), 'testdata')
d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])
```

Data Processing
---------------

``` r
b <- bin_data(data = d, minutes_per_bin = 6)
```

Plotting
--------

``` r
plot_actogram(data = b, start_date = '2010-01-01')
```
