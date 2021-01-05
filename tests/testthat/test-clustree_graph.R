nodes <- tibble::tibble(
    node = c("A", "B"),
    .clustree_indices = list(c("1", "2"), c("1"))
)

edges <- tibble::tibble(
    from = "A",
    to   = "B",
    .clustree_indices = list(c("1"))
)

metadata <- data.frame(
    .clustree_idx = c("1", "2"),
    value = c("Example1", "Example2")
)

test_that("Creating clustree_graph works", {
    graph <- clustree_graph(nodes = nodes, edges = edges, metadata = metadata)
    expect_s3_class(graph, "clustree_graph")
})

test_that("Creating clustree_graph without metadata works", {
    graph <- clustree_graph(nodes = nodes, edges = edges)
    expect_s3_class(graph, "clustree_graph")
})

test_that("Creating clustree_graph without node .clustree_indices errors", {
    broken_nodes <- nodes
    broken_nodes[, ".clustree_indices"] <- NULL
    expect_error(
        clustree_graph(nodes = broken_nodes, edges = edges, metadata = metadata)
    )
})

test_that("Creating clustree_graph without edge .clustree_indices errors", {
    broken_edges <- edges
    broken_edges[, ".clustree_indices"] <- NULL
    expect_error(
        clustree_graph(nodes = nodes, edges = broken_edges, metadata = metadata)
    )
})

test_that("Creating clustree_graph without metadata .clustree_idx informs", {
    broken_metadata <- metadata
    broken_metadata[, ".clustree_idx"] <- NULL
    expect_message(
        clustree_graph(nodes = nodes, edges = edges, metadata = broken_metadata)
    )
})
