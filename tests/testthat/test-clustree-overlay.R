context("clustree_overlay")

data("iris_clusts")
data("sim_sc3")
data("sim_seurat")

test_that("data.frame interface works", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2"),
        c("gg", "ggplot")
    )
})

test_that("SingleCellExperiment interface works", {
    expect_is(
        clustree_overlay(sim_sc3, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TSNE2",
                         red_dim = "TSNE"),
        c("gg", "ggplot")
    )
})

test_that("seurat interface works", {
    expect_is(
        clustree_overlay(sim_seurat, x_value = "pca1", y_value = "pca2",
                         red_dim = "pca"),
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

test_that("red_dim check works", {
    expect_error(
        clustree_overlay(sim_sc3, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TSNE2",
                         red_dim = "TEST"),
        "red_dim must be the name of")
    expect_error(
        clustree_overlay(sim_seurat, x_value = "pca1", y_value = "pca2",
                         red_dim = "test"),
        "red_dim must be the name of")
})

test_that("x_value y_value check works", {
    expect_error(
        clustree_overlay(sim_sc3, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TEST", y_value = "TSNE2",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
    expect_error(
        clustree_overlay(sim_seurat, x_value = "TEST", y_value = "pca2",
                         red_dim = "pca"),
        "No data identified for x_value or y_value")
    expect_error(
        clustree_overlay(sim_sc3, prefix = "sc3_", suffix = "_clusters",
                         x_value = "TSNE1", y_value = "TEST",
                         red_dim = "TSNE"),
        "No data identified for x_value or y_value")
    expect_error(
        clustree_overlay(sim_seurat, x_value = "pca1", y_value = "TEST",
                         red_dim = "pca"),
        "No data identified for x_value or y_value")
})
