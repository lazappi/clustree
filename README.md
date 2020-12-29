clustree <img src="man/figures/logo.png" align="right" />
=======================================================

[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R-CMD-check](https://github.com/lazappi/clustree/workflows/R-CMD-check/badge.svg)](https://github.com/lazappi/clustree/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/lazappi/clustree/master.svg)](https://codecov.io/github/lazappi/clustree?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/lazappi/clustree/badge)](https://www.codefactor.io/repository/github/lazappi/clustree)
[![CRAN Status](http://www.r-pkg.org/badges/version/clustree)](https://cran.r-project.org/package=clustree)
[![CRAN Monthly Downloads](https://cranlogs.r-pkg.org/badges/clustree)](https://cran.r-project.org/package=clustree)
![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/clustree)

Deciding what resolution to use can be a difficult question when approaching a
clustering analysis. One way to approach this problem is to look at how samples
move as the number of clusters increases. This package allows you to produce
clustering trees, a visualisation for interrogating clusterings as resolution 
increases.

## Installation

You can install the release version of _clustree_ from CRAN with:

``` r
install.packages("clustree")
```

If you want to use the development version that can be installed from GitHub
using the `remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("lazappi/clustree@develop")
```

To also build the vignettes use:

``` r
# install.packages("remotes")
remotes::install_github("lazappi/clustree@develop", dependencies = TRUE,
                         build_vignettes = TRUE)
```

**NOTE:** Building the vignettes requires the installation of additional
packages.

## Documentation

The documentation for _clustree_ is available from CRAN at 
https://cran.r-project.org/package=clustree.

To view the vignette and all the package documentation for the development
version visit http://lazappi.github.io/clustree.

## Citing _clustree_

If you use _clustree_ or the clustering trees approach in your work please cite
our publication ["Zappia L, Oshlack A. Clustering trees: a visualization for 
evaluating clusterings at multiple resolutions. Gigascience. 2018;7. 
DOI:gigascience/giy083][paper].

```
citation("clustree")
 
   Zappia L, Oshlack A. Clustering trees: a visualization for
   evaluating clusterings at multiple resolutions. GigaScience.
   2018;7. DOI:gigascience/giy083
 
A BibTeX entry for LaTeX users is
 
   @Article{,
     author = {Luke Zappia and Alicia Oshlack},
     title = {Clustering trees: a visualization for evaluating clusterings at
              multiple resolutions},
     journal = {GigaScience},
     volume = {7},
     number = {7},
     month = {jul},
     year = {2018},
     url = {http://dx.doi.org/10.1093/gigascience/giy083},
     doi = {10.1093/gigascience/giy083},
   }
```

## Contributors

Thank you to everyone who has contributed code to the clustree package:

* [@andreamrau](https://github.com/andreamrau) - added the `edge_arrow_ends`
  option
* [@mojaveazure](https://github.com/mojaveazure) - added support for _Seurat_
  v3 objects

[paper]: https://doi.org/10.1093/gigascience/giy083
