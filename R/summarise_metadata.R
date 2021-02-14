#' Summarise clustering tree metadata
#'
#' Tidyverse style function for summarising sample metadata by node or edge for
#' a `clustree_graph` object.
#'
#' `summarise_metadata` and `summarize_metadata` are synonyms.
#'
#' @param .graph A `clustree_graph` object
#' @param ... Arguments based to [dplyr::summarise()]
#'
#' @details
#' This function is designed to make it easy to add additional summarised
#' information to nodes or edges in as flexible a way as possible. It uses the
#' following steps:
#'
#' 1. Extract nodes/edges (whichever are active) as a `tibble` using
#' [tibble::as_tibble()]
#' 2. Unnest the "`.clustree_indices`" column using [tidyr::unnest()]
#' 3. Join the metadata table by the indices using [dplyr::left_join()]
#' 4. Calculated summaries using [dplyr::summarise()]
#' 5. Join the results to the original nodes/edges using [dplyr::left_join()]
#'
#' @seealso [dplyr::summarise()] for details on summarisation and [clustree_graph()] for `clustree_graph` objects
#'
#' @return A `clustree_graph` object
#' @export
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, patter = "K(.*)")
#'
#' # Summarise by node
#' graph <- tidygraph::activate(graph, "nodes")
#' summarise_metadata(graph, MeanReboundPct = mean(ReboundPct))
#'
#' # Summarise by edge
#' graph <- tidygraph::activate(graph, "edges")
#' summarise_metadata(graph, MeanReboundPct = mean(ReboundPct))
summarise_metadata <- function(.graph, ...) {
    UseMethod("summarise_metadata")
}
#' @rdname summarise_metadata
#' @export
summarize_metadata <- summarise_metadata

#' @rdname summarise_metadata
#' @export
summarise_metadata.clustree_graph <- function(.graph, ...) {

    active <- attr(.graph, "active")

    nested_tbl <- tibble::as_tibble(.graph)
    metadata   <- igraph::graph_attr(.graph, ".clustree_metadata")

    tbl <- tidyr::unnest(nested_tbl, .data$.clustree_indices)
    tbl <- dplyr::left_join(
        tbl,
        metadata,
        by = c(".clustree_indices" = ".clustree_idx")
    )

    tbl <- switch (active,
                   nodes = dplyr::group_by(tbl, .data$node),
                   edges = dplyr::group_by(tbl, .data$from, .data$to)
    )

    summarised <- dplyr::summarise(tbl, ...)

    switch (active,
            nodes = tidygraph::left_join(.graph, summarised, by = "node"),
            edges = tidygraph::left_join(
                .graph,
                summarised,
                by = c("from", "to")
            )
    )
}
