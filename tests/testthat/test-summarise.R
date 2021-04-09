graph <- build_clustree_graph(nba_clusts, patter = "K(.*)")

test_that("summarise_metadata works for nodes", {
    graph <- tidygraph::activate(graph, "nodes")
    test_graph <- summarise_metadata(
        graph,
        MeanTurnoverPct = mean(.data$TurnoverPct)
    )
    expect_s3_class(test_graph, "clustree_graph")
    expect_true("MeanTurnoverPct" %in% colnames(as.data.frame(test_graph)))
})

test_that("summarise_metadata works for edges", {
    graph <- tidygraph::activate(graph, "edges")
    test_graph <- summarise_metadata(
        graph,
        MeanAssistPct = mean(.data$AssistPct)
    )
    expect_s3_class(test_graph, "clustree_graph")
    expect_true("MeanAssistPct" %in% colnames(as.data.frame(test_graph)))
})

test_that("summarize_metadata works", {
    graph <- tidygraph::activate(graph, "nodes")
    test_graph <- summarize_metadata(
        graph,
        MeanReboundPct = mean(.data$ReboundPct)
    )
    expect_s3_class(test_graph, "clustree_graph")
    expect_true("MeanReboundPct" %in% colnames(as.data.frame(test_graph)))
})
