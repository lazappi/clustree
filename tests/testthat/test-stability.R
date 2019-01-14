context("stability")

data("iris_clusts")

test_that("colour by stability works", {
    expect_is(clustree(iris_clusts, prefix = "K",
                       node_colour = "sc3_stability"),
              c("gg", "ggplot"))
})
