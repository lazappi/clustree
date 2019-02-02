context("clustree")

data("iris_clusts")
data("sc_example")

if (requireNamespace("Seurat", quietly = TRUE)) {
    library("Seurat")
    seurat <- CreateSeuratObject(sc_example$counts,
                                 meta.data = sc_example$seurat_clusters)
    seurat <- SetDimReduction(seurat, "TSNE", "cell.embeddings",
                              sc_example$tsne)
}

if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    library("SingleCellExperiment")
    sce <- SingleCellExperiment(assays = list(counts = sc_example$counts,
                                              logcounts = sc_example$logcounts),
                                colData = sc_example$sc3_clusters,
                                reducedDims = SimpleList(TSNE = sc_example$tsne))
}

test_that("data.frame interface works", {
    expect_is(clustree(iris_clusts, prefix = "K"), c("gg", "ggplot"))
})

test_that("SingleCellExperiment interface works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_is(clustree(sce, prefix = "sc3_", suffix = "_clusters"),
              c("gg", "ggplot"))
})

test_that("Seurat interface works", {
    skip_if_not_installed("Seurat")
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

test_that("show_axis works", {
    expect_is(clustree(iris_clusts, prefix = "K", show_axis = TRUE),
              c("gg", "ggplot"))
    expect_is(clustree(seurat, show_axis = TRUE),
              c("gg", "ggplot"))
    expect_is(clustree(sce, prefix = "sc3_", suffix = "_clusters",
                       show_axis = TRUE),
              c("gg", "ggplot"))
})
