context("clustree")

data("iris_clusts")
data("sc_example")

iris_clusts2 <- iris_clusts
iris_clusts2[["A-1"]] <- iris_clusts2$Sepal.Length

iris_clusts3 <- iris_clusts
iris_clusts3$K1 <- "A"

iris_clusts4 <- iris_clusts
iris_clusts4$L1 <- iris_clusts4$K1
iris_clusts4$L2 <- iris_clusts4$K2

iris_clusts6 <- iris_clusts
iris_clusts6$KX <- "X"

seurat_clusters2 <- sc_example$seurat_clusters
seurat_clusters2$resX <- "X"

if (requireNamespace("Seurat", quietly = TRUE) &&
    packageVersion(pkg = "Seurat") < package_version(x = "3.0.0")) {
    library("Seurat")
    seurat <- CreateSeuratObject(sc_example$counts,
                                 meta.data = sc_example$seurat_clusters)
    seurat <- SetDimReduction(seurat, "TSNE", "cell.embeddings",
                              sc_example$tsne)
}

if (requireNamespace("Seurat", quietly = TRUE) &&
    packageVersion(pkg = "Seurat") >= package_version(x = "3.0.0")) {
    library("Seurat")
    seurat <- CreateSeuratObject(counts = sc_example$counts,
                                 meta.data = sc_example$seurat_clusters)
    seurat[["TSNE"]] <- suppressWarnings(CreateDimReducObject(
        embeddings = sc_example$tsne,
        key = "tSNE_",
        assay = DefaultAssay(seurat)
    ))
}

if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    library("SingleCellExperiment")
    sce <- SingleCellExperiment(
        assays = list(counts = sc_example$counts,
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
    expect_is(clustree(seurat, prefix = "res."), c("gg", "ggplot"))
})

test_that("column number check works", {
    expect_error(clustree(iris_clusts[1:5], prefix = "K"),
                 "Less than two column names matched")
    expect_error(clustree(iris_clusts[1:6], prefix = "K"),
                 "Less than two column names matched")
})

test_that("metadata column name check works", {
    expect_warning(clustree(iris_clusts2, prefix = "K"),
                   "The following metadata column names will be converted")
})

test_that("aesthetics name check works", {
    expect_warning(clustree(iris_clusts2, prefix = "K", node_colour = "A-1",
                            node_colour_aggr = "mean"),
                   "node_colour will be converted from")
    expect_warning(clustree(iris_clusts2, prefix = "K", node_size = "A-1",
                            node_size_aggr = "mean"),
                   "node_size will be converted from")
    expect_warning(clustree(iris_clusts2, prefix = "K", node_alpha = "A-1",
                            node_alpha_aggr = "mean"),
                   "node_alpha will be converted from")
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
})

test_that("character cluster names work", {
    expect_is(clustree(iris_clusts3, prefix = "K"),
              c("gg", "ggplot"))
})

test_that("exact prefix selection works", {
    # Fails if matches additional columns
    expect_is(clustree(iris_clusts4, prefix = "L"), c("gg", "ggplot"))
})

test_that("prefix selection doesn't match wildcards", {
    expect_is(clustree(seurat_clusters2, prefix = "res."), c("gg", "ggplot"))
})

test_that("check for non-numeric resolution works", {
    expect_error(clustree(iris_clusts6, prefix = "K"),
                 "The X portion of your clustering column names could not be ")
})
