test_that("layout_clustree works", {
    graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
    layout <- layout_clustree(graph)
    expect_s3_class(layout, "layout_tbl_graph")
})

test_that("layout_clustree.default errors", {
    expect_error(
        layout_clustree(list()),
        "No support for list objects"
    )
})
