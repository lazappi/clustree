context("stability")

data("nba_clusts")

test_that("colour by stability works", {
    expect_is(clustree(nba_clusts, prefix = "K",
                       node_colour = "sc3_stability"),
              c("gg", "ggplot"))
})
