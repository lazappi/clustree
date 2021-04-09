test_that("plot_clustree works", {
    graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
    plot <- plot_clustree(graph)
    expect_s3_class(plot, "ggraph")
})

test_that("plot_clustree.default errors", {
    expect_error(
        plot_clustree(list()),
        "No support for list objects"
    )
})
