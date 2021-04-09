test_that("which_geom_layers works", {
    layers <- list(
        ggplot2::geom_point(),
        ggplot2::geom_bar(),
        ggplot2::geom_point()
    )
    which_layers <- which_geom_layers(layers, "GeomPoint")
    expect_identical(which_layers, c(1L, 3L))
})

test_that("split_layer works for GeomClustreePoint", {
    layer <- geom_clustree_point()
    layer_split <- split_layer(layer, "split", 1:3)
    expect_true(is.list(layer_split))
    expect_length(layer_split, 3)
})

test_that("split_layer works for GeomClustreeEdge", {
    layer <- geom_clustree_edge()
    layer_split <- split_layer(layer, "split", 1:3, edge = TRUE)
    expect_true(is.list(layer_split))
    expect_length(layer_split, 3)
})

graph <- tidygraph::create_citation(10)
graph <- tidygraph::mutate(graph, Filter = rep(1:2, each = 5))
layout_data <- ggraph::create_layout(graph, "sugiyama")

test_that("filter_node_data works", {
    filtered_data <- filter_node_data(layout_data, "Filter", 1)
    expect_s3_class(filtered_data, "layout_tbl_graph")
    expect_equal(nrow(filtered_data), 5)
})

test_that("filter_edge_data works", {
    filtered_data <- filter_edge_data(layout_data, "Filter", 1)
    expect_s3_class(filtered_data, "data.frame")
    expect_equal(nrow(filtered_data), 10)
})

test_that("layer data filter levels check works", {
    expect_error(
        filter_node_data(layout_data, "Filter", 3),
        "There are less than 3 levels",
    )
    expect_error(
        filter_edge_data(layout_data, "Filter", 3),
        "There are less than 3 levels",
    )
})

test_that("alternate_clustree_layers works", {
    graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
    graph <- summarise_metadata(graph, PC1 = mean(PC1), PC2 = mean(PC2))
    overlay <- overlay_clustree(graph, x_dim = "PC1", y_dim = "PC2")
    plot <- plot_clustree(overlay) +
        geom_clustree_point() +
        geom_clustree_edge()

    modified_plot <- alternate_clustree_layers(plot)
    expect_s3_class(modified_plot, "ggplot")
    expect_length(modified_plot$layers, 9)

    which_nodes <- which_geom_layers(modified_plot$layers, "GeomClustreePoint")
    expect_identical(which_nodes, seq(1L, 9L, 2L))

    which_edges <- which_geom_layers(modified_plot$layers, "GeomClustreeEdge")
    expect_identical(which_edges, seq(2L, 8L, 2L))
})
