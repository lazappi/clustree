context("clustree")

data("iris_clusts")
data("sc_example")

library("SingleCellExperiment")
library("Seurat")
sce <- SingleCellExperiment(assays = list(counts = sc_example$counts,
                                          logcounts = sc_example$logcounts),
                            colData = sc_example$sc3_clusters,
                            reducedDims = SimpleList(TSNE = sc_example$tsne))
seurat <- as.seurat(sce)
seurat@meta.data <- sc_example$seurat_clusters

test_that("data.frame interface works", {
    expect_is(clustree(iris_clusts, prefix = "K"), c("gg", "ggplot"))
})

test_that("SingleCellExperiment interface works", {
    expect_is(clustree(sce, prefix = "sc3_", suffix = "_clusters"),
              c("gg", "ggplot"))
})

test_that("seurat interface works", {
    expect_is(clustree(seurat), c("gg", "ggplot"))
})

test_that("column number check works", {
    expect_error(clustree(iris_clusts[1:5], prefix = "K"),
                 "Less than two column names matched")
    expect_error(clustree(iris_clusts[1:6], prefix = "K"),
                 "Less than two column names matched")
})

test_that("returning graph works", {
    expect_is(clustree(iris_clusts, prefix = "K", return = "graph"),
              c("tbl_graph"))
})

test_that("returning layout works", {
    expect_is(clustree(iris_clusts, prefix = "K", return = "layout"),
              c("layout_igraph", "layout_ggraph"))
})
