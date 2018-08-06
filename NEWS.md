### clustree 0.2.2.9001

* Make the cluster node attribute a factor to improve use as node colour

### clustree 0.2.2.9000

* Use fixed patterns when identifying clustering columns in data.frames

## clustree 0.2.2 (2018-07-10)

* Replace SingleCellExperiment and Seurat example datasets with a list
* Add additional checks for suggested packages
* Remove links to suggested packages in documentation

### clustree 0.2.1.9001 (2018-07-10)

* Add additional checks for suggested packages
* Remove links to suggested packages in documentation

### clustree 0.2.1.9000 (2018-07-10)

* Replace SingleCellExperiment and Seurat example datasets with a list

## clustree 0.2.1 (2018-07-09)

* Bump version for CRAN

# clustree 0.2.0 (2018-06-24)

* Calculate the SC3 stability index for cluster nodes
* Improvements to the clustree function:
    * Add option to use core network to calculate layout
    * Add option to highlight core network
    * Add option to return plot, graph or layout
* New clustree_overlay function
    * Allows overlaying of clustering trees on other data dimensions
* Store trees as tbl_graph instead of igraph objects
* Switch to GPL-3 license
* Updates to vignette, documentation and tests

### clustree 0.1.2.9005 (2018-06-23)

* Add plot_sides argument to clustree_overlay

### clustree 0.1.2.9004 (2018-06-22)

* Add clustree_overlay function

### clustree 0.1.2.9003 (2018-06-20)

* Add checks for suggested packages

### clustree 0.1.2.9002 (2018-05-22)

* Store tree as tbl_graph instead of igraph object
* Update vignette

### clustree 0.1.2.9001 (2018-05-09)

* Add option to use core network to calculate layout
* Add option to highlight core network
* Add option to return plot, graph or layout

### clustree 0.1.2.9000 (2018-05-08)

* Add functions to calculate the SC3 stability index
* Switch to GPL-3 license

## clustree 0.1.2 (2018-04-11)

* Adjust tests to work with new checkmate version (v1.8.6)
* Update R requirement to >= 3.4 (for compatibility with SingleCellExperiment)
* Add CITATION file and update author entries in DESCRIPTION

### clustree 0.1.1.9002 (2018-04-11)

* Add CITATION file
* Update author entries in DESCRIPTION

### clustree 0.1.1.9001 (2018-04-11)

* Update R requirement to >= 3.4

### clustree 0.1.1.9000 (2018-04-11)

* Adjust tests to match checkmate messages

## clustree 0.1.1 (2018-03-23)

* Add edge_arrow_ends argument

# clustree 0.1.0 (2018-03-01)

* First release
* Remove theme_clustree function

## clustree 0.0.0.9013 (2018-02-28)

* Fix resolution colour ordering

## clustree 0.0.0.9012 (2018-02-15)

* Switch from using function for aggregation to function names
* Improve naming of aesthetics
* Set alpha scale limits
* Add Sugiyama layout
* Update vignette
* Tidy code

## clustree 0.0.0.9011 (2018-02-09)

* Fix missing node when all aesthetics static

## clustree 0.0.0.9010 (2018-02-05)

* Check that at least two resolutions have been provided
* Add node text colour option
* Improve example scRNA-seq datasets

## clustree 0.0.0.9009 (2018-02-02)

* Add data option to exprs for Seurat objects

## clustree 0.0.0.9008 (2018-01-27)

* Add vignettes

## clustree 0.0.0.9007 (2018-01-27)

* Add tests and run checks

## clustree 0.0.0.9006 (2018-01-27)

* Add documentation
* Add parameter checks

## clustree 0.0.0.9005 (2018-01-27)

* Add Seurat example data
* Add Seurat method

## clustree 0.0.0.9004 (2018-01-26)

* Add SingleCellExperiment example data
* Add SingleCellExperiment method

## clustree 0.0.0.9003 (2018-01-25)

* Add more control over aesthetics

## clustree 0.0.0.9002 (2018-01-24)

* Add node aesthetic options

## clustree 0.0.0.9001 (2018-01-23)

* Implement basic plotting

# clustree 0.0.0.9000 (2018-01-22)

* Start development
