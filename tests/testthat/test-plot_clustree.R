graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
graph <- summarise_metadata(graph, PC1 = mean(.data$PC1), PC2 = mean(.data$PC2))

test_that("plot_clustree default works", {
    expect_s3_class(plot_clustree(graph), "ggraph")
})

test_that("plot_clustree tree layout works", {
    expect_s3_class(plot_clustree(graph, layout = "tree"), "ggraph")
})

test_that("plot_clustree sugiyama layout works", {
    expect_s3_class(plot_clustree(graph, layout = "sugiyama"), "ggraph")
})

test_that("plot_clustree overlay layout works", {
    expect_s3_class(
        plot_clustree(
            graph, layout = "overlay", x_dim = "PC1", y_dim = "PC2"
        ),
        "ggraph"
    )
})

test_that("plot_clustree not use_core_nodes works", {
    expect_s3_class(plot_clustree(graph, use_core_edges = FALSE), "ggraph")
})
