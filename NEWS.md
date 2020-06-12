# Development versions

## _clustree_ 0.4.2.9003 (2020-06-12)

* Change how metadata columns are extracted for compatibility with `tibble`
  objects (Fixes #52)

## _clustree_ 0.4.2.9002 (2020-06-12)

* Replace the `iris_clusts` dataset with the `nba_clusts` dataset

## _clustree_ 0.4.2.9001 (2020-03-20)

* Add checks to handle features including "-" characters for _Seurat_ and 
  _SingleCellExperiment_ interfaces to `clustree()` and `clustree_overlay()`
  (Fixes #43)

## _clustree_ 0.4.2.9000 (2020-03-19)

* Fix axis label order in `clustree()` (Fixes #44)

# _clustree_ 0.4.2 (2020-01-29)

## Bug fixes

* Fix bug where the `sc_example` dataset required loading additional libraries
  as notified by CRAN (Fixes #40)
  * `sc_example` contained a `DataFrame` rather than a `data.frame` for the
    _SC3_ data, this has now been fixed
* Add fixed alpha scale range to `clustree_overlay()` (Fixes #41)
  * Alpha will now represent in_prop values between 0 and 1 
* Change how point colour resolution name is determined for side plots in
  `clustree_overlay()` (Fixes #38)
* Fix missing alpha scale in `clustree()` (Fixes #32)
* Change prefix matching to simpler exact method without regular expressions
  * This should allow matching the start of column names without the wildcard
    problem in Issue #19
* Fixes for minor bugs discovered with new unit tests

## Other changes

* Add additional unit tests, including some missing from `clustree_overlay()`
  * Tests for matching prefix values
  * Test for point colour in `clustree_overlay()` with rounded resolutions

## Development versions

## _clustree_ 0.4.1.9005 (2020-01-29)

* Add additional unit tests, including some missing from `clustree_overlay()`
* Minor fixes for bugs discovered with new tests

## _clustree_ 0.4.1.9004 (2020-01-28)

* Add additional unit tests

## _clustree_ 0.4.1.9003 (2020-01-28)

* Add fixed alpha scale range to `clustree_overlay()` (Fixes #41)

## _clustree_ 0.4.1.9002 (2020-01-28)

* Add tests for matching prefix values
* Change prefix matching to simpler exact method without regular expressions
* Add test for point colour in `clustree_overlay()` with rounded resolutions
* Change how point colour resolution name is determined (Fixes #38)

## _clustree_ 0.4.1.9001 (2020-01-27)

* Fix missing alpha scale (Fixes #32)

## _clustree_ 0.4.1.9000 (2020-01-27)

* Fix bug where the `sc_example` dataset required loading additional libraries
  (Fixes #40)
  * `sc_example` contained a `DataFrame` rather than a `data.frame` for the
    _SC3_ data

# _clustree_ 0.4.1 (2019-08-19)

## Bug fixes

* Remove requirement for clustering matrix to be numeric (Fixes #33)
* Fix bug where `sc3_stability` became character with character cluster names
* Keep order when all cluster names are numeric
* Improve how `reducedDims` are accessed on `SingleCellExperiment` objects
* Fix minor typos in documentation

## Development versions

### _clustree_ 0.4.0.9002 (2019-08-19)

* Improve how `reducedDims` are accessed on `SingleCellExperiment` objects
* Fix minor typos in documentation

### _clustree_ 0.4.0.9001 (2019-06-05)

* Fix bug where `sc3_stability` became character with character cluster names
* Keep order when all cluster names are numeric

### _clustree_ 0.4.0.9000 (2019-06-03)

* Remove requirement for clustering matrix to be numeric (Fixes #33)

# _clustree_ 0.4.0 (2019-04-18)

## Minor changes

* Add support for _Seurat_ v3 objects (thanks to @mojaveazure)
* Add _SC3_ stability index section to vignette (Fixes #28)

# _clustree_ 0.3.0 (2019-02-24)

## Minor changes

* Make the cluster node attribute a factor to improve use as node colour (Fixes
  #20)
* Add `show_axis` argument that displays the y-axis with resolution values and
  gridlines (Fixes #24)
* Add ability to add additional node labels with custom information (Fixes #23)
* Update **CITATION** to give information about the GigaScience paper
* Update README
* Update vignette
  * Add custom labels section
  * Add references
  * Add table of contents and number sections

## Bug fixes

* Add checks for metadata column and node aesthetic names (Fixes #15)
* Use fixed patterns when identifying clustering columns in `data.frames` (Fixes
  #19)

## Development versions

### _clustree_ 0.3.0.9000 (2019-04-27)

* Add support for _Seurat_ v3 objects (thanks to @mojaveazure)

### _clustree_ 0.3.0.9001 (2019-04-27)

* Add _SC3_ stability index section to vignette (Fixes #28)

# _clustree_ 0.2.2 (2018-07-10)

* Replace `SingleCellExperiment` and `Seurat` example datasets with a `list`
* Add additional checks for suggested packages
* Remove links to suggested packages in documentation

## _clustree_ 0.2.2.9005 (2019-02-24)

* Update **CITATION**
* Update **README**
* Add spelling checks
* Update vignette
  * Add custom labels section
  * Add references
  * Add table of contents and number sections

## _clustree_ 0.2.2.9004 (2019-02-17)

* Add ability to add additional node labels

## _clustree_ 0.2.2.9003 (2019-02-12)

* Add checks for metadata column and node aesthetic names
* Adjust `show_axis` to use gridlines

## _clustree_ 0.2.2.9002 (2019-02-02)

* Add `show_axis` argument

## _clustree_ 0.2.2.9001 (2018-08-06)

* Make the cluster node attribute a factor to improve use as node colour

## _clustree_ 0.2.2.9000 (2018-08-01)

* Use fixed patterns when identifying clustering columns in `data.frames`

# _clustree_ 0.2.1 (2018-07-09)

* Bump version for CRAN

## _clustree_ 0.2.1.9001 (2018-07-10)

* Add additional checks for suggested packages
* Remove links to suggested packages in documentation

## _clustree_ 0.2.1.9000 (2018-07-10)

* Replace `SingleCellExperiment` and `Seurat` example datasets with a `list`

# _clustree_ 0.2.0 (2018-06-24)

* Calculate the _SC3_ stability index for cluster nodes
* Improvements to the `clustree()` function:
    * Add option to use core network to calculate layout
    * Add option to highlight core network
    * Add option to return plot, graph or layout
* New `clustree_overlay()` function
    * Allows overlaying of clustering trees on other data dimensions
* Store trees as `tbl_graph` instead of `igraph` objects
* Switch to GPL-3 license
* Updates to vignette, documentation and tests

# _clustree_ 0.1.2 (2018-04-11)

* Adjust tests to work with new checkmate version (v1.8.6)
* Update _R_ requirement to >= 3.4 (for compatibility with
  _SingleCellExperiment_)
* Add **CITATION** file and update author entries in **DESCRIPTION**

## _clustree_ 0.1.2.9000 (2018-05-08)

* Add functions to calculate the _SC3_ stability index
* Switch to GPL-3 license

## _clustree_ 0.1.2.9001 (2018-05-09)

* Add option to use core network to calculate layout
* Add option to highlight core network
* Add option to return plot, graph or layout

## _clustree_ 0.1.2.9002 (2018-05-22)

* Store tree as `tbl_graph` instead of `igraph` object
* Update vignette

## _clustree_ 0.1.2.9003 (2018-06-20)

* Add checks for suggested packages

## _clustree_ 0.1.2.9004 (2018-06-22)

* Add `clustree_overlay()` function

## _clustree_ 0.1.2.9005 (2018-06-23)

* Add `plot_sides` argument to `clustree_overlay()`

# _clustree_ 0.1.1 (2018-03-23)

* Add `edge_arrow_ends` argument

## _clustree_ 0.1.1.9000 (2018-04-11)

* Adjust tests to match _checkmate_ messages

## _clustree_ 0.1.1.9001 (2018-04-11)

* Update _R_ requirement to >= 3.4

## _clustree_ 0.1.1.9002 (2018-04-11)

* Add **CITATION** file
* Update author entries in **DESCRIPTION**

# _clustree_ 0.1.0 (2018-03-01)

* First release
* Remove `theme_clustree()` function

# _clustree_ 0.0.0 (2018-01-22)

## _clustree_ 0.0.0.9000 (2018-01-22)

* Start development

## _clustree_ 0.0.0.9001 (2018-01-23)

* Implement basic plotting

## _clustree_ 0.0.0.9002 (2018-01-24)

* Add node aesthetic options

## _clustree_ 0.0.0.9003 (2018-01-25)

* Add more control over aesthetics

## _clustree_ 0.0.0.9004 (2018-01-26)

* Add `SingleCellExperiment` example data
* Add `SingleCellExperiment` method

## _clustree_ 0.0.0.9005 (2018-01-27)

* Add `Seurat` example data
* Add `Seurat` method

## _clustree_ 0.0.0.9006 (2018-01-27)

* Add documentation
* Add parameter checks

## _clustree_ 0.0.0.9007 (2018-01-27)

* Add tests and run checks

## _clustree_ 0.0.0.9008 (2018-01-27)

* Add vignettes

## _clustree_ 0.0.0.9009 (2018-02-02)

* Add data option to exprs for `Seurat` objects

## _clustree_ 0.0.0.9010 (2018-02-05)

* Check that at least two resolutions have been provided
* Add node text colour option
* Improve example scRNA-seq datasets

## _clustree_ 0.0.0.9011 (2018-02-09)

* Fix missing node when all aesthetics static

## _clustree_ 0.0.0.9012 (2018-02-15)

* Switch from using function for aggregation to function names
* Improve naming of aesthetics
* Set alpha scale limits
* Add Sugiyama layout
* Update vignette
* Tidy code

## _clustree_ 0.0.0.9013 (2018-02-28)

* Fix resolution colour ordering
