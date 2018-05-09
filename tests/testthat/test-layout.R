context("layout")

data("iris_clusts")

test_that("non-core layout works", {
    expect_is(clustree(iris_clusts, prefix = "K", use_core_network = FALSE),
              c("gg", "ggplot"))
})

test_that("highlighting core works", {
    expect_is(clustree(iris_clusts, prefix = "K", highlight_core = TRUE),
              c("gg", "ggplot"))
})
