context("aesthetics")

data("iris_clusts")

test_that("node_colour aesthetic works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_colour = "Sepal.Width",
                       node_colour_aggr = "mean"),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_colour = "Sepal.Width"),
              "Must be of type 'character'")
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_colour = "Sepal.Width",
                          node_colour_aggr = "FAKEFUN"),
                 "'function' was not found")
})

test_that("static node_colour works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_colour = 1),
              c("gg", "ggplot"))
    expect_is(clustree(iris_clusts, prefix = "K", node_colour = "purple"),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K", node_colour = -1),
                 ">= 0")
    expect_error(clustree(iris_clusts, prefix = "K", node_colour = "XXXX"),
                 "not a valid colour name")
})

test_that("node_size aesthetic works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_size = "Sepal.Width",
                       node_size_aggr = "mean"),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_size = "Sepal.Width"),
                 "Must be of type 'character'")
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_size = "Sepal.Width",
                          node_size_aggr = "FAKEFUN"),
                 "'function' was not found")
})

test_that("static node_size works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_size = 1),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K", node_size = -1),
                 ">= 0")
    expect_error(clustree(iris_clusts, prefix = "K", node_size = "XXXX"),
                 "must be one of")
})

test_that("node_alpha aesthetic works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_alpha = "Sepal.Width",
                       node_alpha_aggr = "mean"),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_alpha = "Sepal.Width"),
                 "Must be of type 'character'")
    expect_error(clustree(iris_clusts, prefix = "K",
                          node_alpha = "Sepal.Width",
                          node_alpha_aggr = "FAKEFUN"),
                 "'function' was not found")
})

test_that("static node_size works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_alpha = 1),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K", node_alpha = -1),
                 ">= 0")
    expect_error(clustree(iris_clusts, prefix = "K", node_alpha = 2),
                 "<= 1")
    expect_error(clustree(iris_clusts, prefix = "K", node_alpha = "XXXX"),
                 "must be one of")
})

test_that("all static works", {
    expect_is(clustree(iris_clusts, prefix = "K", node_colour = "red",
                       node_size = 10, node_alpha = 1),
              c("gg", "ggplot"))
})

test_that("layout argument works", {
    expect_is(clustree(iris_clusts, prefix = "K", layout = "sugiyama"),
              c("gg", "ggplot"))
    expect_error(clustree(iris_clusts, prefix = "K", layout = "apple"),
                 "should be one of")
})

test_that("node labels work", {
    expect_is(clustree(iris_clusts, prefix = "K", node_label = "cluster"),
              c("gg", "ggplot"))
})
