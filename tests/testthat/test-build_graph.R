test_that("extract_clusterings works", {
    clusterings <- extract_clusterings(nba_clusts, pattern = "K(.*)")
    expect_true(is.matrix(clusterings))
    expect_identical(dim(clusterings), c(150L, 5L))
    expect_identical(colnames(clusterings), c("1", "2", "3", "4", "5"))
})

test_that("extract_clusterings pattern match check works", {
    expect_error(
        extract_clusterings(nba_clusts, pattern = "NOMATCH(.*)"),
        "Less than two columns matched the pattern"
    )
})

test_that("extract_metadata works", {
    metadata <- extract_metadata(nba_clusts, pattern = "K(.*)")
    expect_s3_class(metadata, "tbl_df")
    expect_identical(dim(metadata), c(150L, 7L))
    expect_identical(
        colnames(metadata),
        c("Position", "TurnoverPct", "ReboundPct", "AssistPct", "FieldGoalPct",
          "PC1", "PC2")
    )
})

test_that("extract_metadata returns NULL with no extra columns", {
    expect_null(extract_metadata(nba_clusts[, 6:10], pattern = "K(.*)"))
})

test_that("extract_metadata warns about name conversion", {
    test_data <- nba_clusts
    test_data[["test-col"]] <- "Test"
    expect_warning(
        extract_metadata(test_data, pattern = "K(.*)"),
        "The following metadata column names will be converted"
    )
})

test_that("get_tree_nodes works", {
    clusterings <- extract_clusterings(nba_clusts, "K(.*)")
    nodes <- get_tree_nodes(clusterings)
    expect_s3_class(nodes, "tbl_df")
    expect_identical(
        colnames(nodes),
        c("node", "res", "cluster", ".clustree_indices")
    )
})

test_that("get_tree_edges works", {
    clusterings <- extract_clusterings(nba_clusts, "K(.*)")
    edges <- get_tree_edges(clusterings)
    expect_s3_class(edges, "tbl_df")
    expect_identical(
        colnames(edges),
        c("from_node", "to_node", "from_res", "to_res", "from_cluster",
          "to_cluster", ".clustree_indices")
    )
})

test_that("build_clustree_graph from matrix works", {
    test_data <- as.matrix(nba_clusts[, 6:10])
    graph <- build_clustree_graph(test_data)
    expect_s3_class(graph, "clustree_graph")
})

test_that("build_clustree_graph from data.frame works", {
    graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
    expect_s3_class(graph, "clustree_graph")
})
