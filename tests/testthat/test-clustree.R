context("clustree")

data("iris_clusts")
data("sim_sc3")
data("sim_seurat")

test_that("data.frame interface works", {
    expect_is(clustree(iris_clusts, prefix = "K"), c("gg", "ggplot"))
})

test_that("SingleCellExperiment interface works", {
    expect_is(clustree(sim_sc3, prefix = "sc3_", suffix = "_clusters"),
              c("gg", "ggplot"))
})

test_that("seurat interface works", {
    expect_is(clustree(sim_seurat), c("gg", "ggplot"))
})