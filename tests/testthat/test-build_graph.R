test_that("select_clust_cols works with pattern", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    expect_identical(
        clust_cols,
        c("1" = "K1", "2" = "K2", "3" = "K3", "4" = "K4", "5" = "K5")
    )
})

test_that("select_clust_cols works with prefix", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), prefix = "K",
                                    suffix = "")
    expect_identical(
        clust_cols,
        c("1" = "K1", "2" = "K2", "3" = "K3", "4" = "K4", "5" = "K5")
    )
})

test_that("select_clust_cols works with names", {
    clust_cols <- select_clust_cols(colnames(nba_clusts),
                                    clust_cols = paste0("K", 1:5))
    expect_identical(clust_cols, paste0("K", 1:5))
})

test_that("select_clust_cols errors with multiple options", {
    expect_error(
        select_clust_cols(colnames(nba_clusts), pattern = "K(.*)",
                          prefix = "K"),
        "Specify one of pattern, prefix"
    )
    expect_error(
        select_clust_cols(colnames(nba_clusts), pattern = "K(.*)",
                            clust_cols = paste0("K", 1:5)),
        "Specify one of pattern, prefix"
    )
    expect_error(
        select_clust_cols(colnames(nba_clusts), prefix = "K",
                            clust_cols = paste0("K", 1:5)),
        "Specify one of pattern, prefix"
    )
    expect_error(
        select_clust_cols(colnames(nba_clusts), prefix = "K", pattern = "K(.*)",
                            clust_cols = paste0("K", 1:5)),
        "Specify one of pattern, prefix"
    )
})

test_that("select_clust_cols pattern match check works", {
    expect_error(
        select_clust_cols(colnames(nba_clusts), pattern = "NOMATCH(.*)"),
        "Less than two columns matched the pattern"
    )
    expect_error(
        select_clust_cols(colnames(nba_clusts), prefix = "NOMATCH"),
        "Less than two columns matched the pattern"
    )
    expect_warning(
        select_clust_cols(colnames(nba_clusts),
                          clust_cols = c(paste0("K", 1:5), "NOMATCH")),
        "Some supplied column names did not match"
    )
    expect_error(
        suppressWarnings(
            select_clust_cols(colnames(nba_clusts),
                              clust_cols = c("K1", "NOMATCH"))
        ),
        "Less than two supplied column names matched"
    )
})

test_that("extract_clusterings works", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    clusterings <- extract_clusterings(nba_clusts, clust_cols)
    expect_true(is.matrix(clusterings))
    expect_identical(dim(clusterings), c(150L, 5L))
    expect_identical(colnames(clusterings), as.character(1:5))
})

test_that("extract_metadata works", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    metadata <- extract_metadata(nba_clusts, clust_cols)
    expect_s3_class(metadata, "tbl_df")
    expect_identical(dim(metadata), c(150L, 7L))
    expect_identical(
        colnames(metadata),
        c("Position", "TurnoverPct", "ReboundPct", "AssistPct", "FieldGoalPct",
          "PC1", "PC2")
    )
})

test_that("extract_metadata returns NULL with no extra columns", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    expect_null(extract_metadata(nba_clusts[, 6:10], clust_cols))
})

test_that("extract_metadata warns about name conversion", {
    test_data <- nba_clusts
    test_data[["test-col"]] <- "Test"
    clust_cols <- select_clust_cols(colnames(test_data), pattern = "K(.*)")
    expect_warning(
        extract_metadata(test_data, clust_cols),
        "The following metadata column names will be converted"
    )
})

test_that("get_tree_nodes works", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    clusterings <- extract_clusterings(nba_clusts, clust_cols)
    nodes <- get_tree_nodes(clusterings)
    expect_s3_class(nodes, "tbl_df")
    expect_identical(
        colnames(nodes),
        c("node", "res", "cluster", ".clustree_indices")
    )
})

test_that("get_tree_edges works", {
    clust_cols <- select_clust_cols(colnames(nba_clusts), pattern = "K(.*)")
    clusterings <- extract_clusterings(nba_clusts, clust_cols)
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

test_that("build_clustree_graph from SCE works", {
    skip_if_not_installed("SingleCellExperiment")
    graph <- build_clustree_graph(sce_example(), pattern = "sc3_(.*)_clusters")
    expect_s3_class(graph, "clustree_graph")
})

test_that("extracting features from SCE works", {
    skip_if_not_installed("SingleCellExperiment")
    graph <- build_clustree_graph(sce_example(), pattern = "sc3_(.*)_clusters",
                                  features = c("Gene1", "Gene4"))
    metadata <- igraph::graph_attr(graph, ".clustree_metadata")
    expect_true(all(c("Gene1", "Gene4") %in% colnames(metadata)))
})

test_that("exprs check for SCE works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_error(
        build_clustree_graph(sce_example(), pattern = "sc3_(.*)_clusters",
                             features = c("Gene1", "Gene4"), exprs = "FAIL"),
        "exprs must be the name of an assay in x"
    )
})

test_that("features check for SCE works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_error(
        build_clustree_graph(sce_example(), pattern = "sc3_(.*)_clusters",
                             features = "FAIL"),
        "Some supplied features are not present"
    )
})
