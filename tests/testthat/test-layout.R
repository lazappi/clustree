graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
graph <- summarise_metadata(graph, PC1 = mean(.data$PC1), PC2 = mean(.data$PC2))

test_that("layout_overlay works", {
    overlay <- layout_overlay(graph, x_dim = "PC1", y_dim = "PC2")
    expect_s3_class(overlay, "layout_tbl_graph")
})

test_that("layout_overlay column checks work", {
    expect_error(
        layout_overlay(graph, x_dim = "FAIL", y_dim = "PC2"),
        "x_dim must be the name of a node column"
    )
    expect_error(
        layout_overlay(graph, x_dim = "PC1", y_dim = "FAIL"),
        "y_dim must be the name of a node column"
    )
})
