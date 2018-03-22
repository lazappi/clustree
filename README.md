# clustree

[![Travis-CI Build Status](https://travis-ci.org/lazappi/clustree.svg?branch=master)](https://travis-ci.org/lazappi/clustree)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lazappi/clustree?branch=master&svg=true)](https://ci.appveyor.com/project/lazappi/clustree)
[![Coverage Status](https://img.shields.io/codecov/c/github/lazappi/clustree/master.svg)](https://codecov.io/github/lazappi/clustree?branch=master)

Deciding what resolution to use can be a difficult question when approaching a
clustering analysis. One way to approach this problem is to look at how samples
move as the number of clusters increases. This package allows you to produce
clustering trees, a visualisation for interrogating
clusterings as resolution increases.

## Installation

You can install clustree from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("lazappi/clustree")
```

To also build the vignettes use:

``` r
# install.packages("devtools")
devtools::install_github("lazappi/clustree", build_vignettes = TRUE)
```

**NOTE:** Building the vignettes requires the installation of additional
packages.

## Documentation

To view the vignette and all the package documentation visit 
http://lazappi.github.io/clustree.

## Contributers

Thank you to everyone who has contributed code to the clustree package:

* @andreamrau - added the `edge_arrow_ends` option
