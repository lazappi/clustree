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
