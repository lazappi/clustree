context("clustree_overlay")

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
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2"),
        c("gg", "ggplot")
    )
})

test_that("SingleCellExperiment interface works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_is(
        clustree_overlay(sce, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TSNE2",
                         red_dim = "TSNE"),
        c("gg", "ggplot")
    )
})

test_that("seurat interface works", {
    skip_if_not_installed("Seurat")
    expect_is(
        clustree_overlay(seurat, x_value = "TSNE1", y_value = "TSNE2",
                         red_dim = "TSNE"),
        c("gg", "ggplot")
    )
})

test_that("column number check works", {
    expect_error(clustree_overlay(iris_clusts[1:5], prefix = "K",
                                  x_value = "PC1", y_value = "PC2"),
                 "Less than two column names matched")
    expect_error(clustree_overlay(iris_clusts[1:6], prefix = "K",
                                  x_value = "PC1", y_value = "PC2"),
                 "Less than two column names matched")
})

test_that("metadata check works", {
    expect_error(clustree_overlay(iris_clusts[, 6:10], prefix = "K",
                                  x_value = "PC1", y_value = "PC2"),
                 "No metadata columns found")
})

test_that("plot_sides works", {
    overlay_list <- clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                                     y_value = "PC2", plot_sides = TRUE)
    expect_is(overlay_list, "list")
    expect_identical(names(overlay_list), c("overlay", "x_side",  "y_side"))
})

test_that("SCE red_dim check works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_error(
        clustree_overlay(sce, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TSNE2",
                         red_dim = "TEST"),
        "red_dim must be the name of")
})

test_that("SCE x_value y_value check works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_error(
        clustree_overlay(sce, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TEST", y_value = "TSNE2",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
    expect_error(
        clustree_overlay(sce, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TEST",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
})

test_that("Seurat x_value y_value check works", {
    skip_if_not_installed("Seurat")
    expect_error(
        clustree_overlay(seurat, x_value = "TEST", y_value = "TSNE2",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
    expect_error(
        clustree_overlay(seurat, x_value = "TSNE1", y_value = "TEST",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
})

test_that("Seurat red_dim check works", {
    skip_if_not_installed("Seurat")
    expect_error(
        clustree_overlay(seurat, x_value = "pca1", y_value = "pca2",
                         red_dim = "test"),
        "red_dim must be the name of")
})
