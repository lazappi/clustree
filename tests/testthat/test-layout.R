context("layout")

data("nba_clusts")

test_that("non-core layout works", {
    expect_is(clustree(nba_clusts, prefix = "K", use_core_edges = FALSE),
              c("gg", "ggplot"))
})

test_that("highlighting core works", {
    expect_is(clustree(nba_clusts, prefix = "K", highlight_core = TRUE),
              c("gg", "ggplot"))
})
