#' Plot clustering tree
#'
#' Create a clustering tree plot
#'
#' @param graph Object containing a clustering tree graph
#' @param layout String specifying the "tree" ([igraph::layout_as_tree()]),
#' "sugiyama" ([igraph::layout_with_sugiyama()]) or overlay ([layout_overlay()])
#' layout. See functions for details.
#' @param count_filter Count threshold for filtering edges before computing
#' layout and plotting
#' @param prop_filter In proportion threshold for filtering edges before
#' computing layout and plotting
#' @param use_core_edges Whether to only use core tree (edges with maximum in
#' proportion for a node) when computing the layout. All edges that pass
#' filtering will still be plotted.
#' @param ... Arguments passed to the layout function
#'
#' @details
#' The purpose of [plot_clustree()] is to create a plot object in a similar way
#' to [ggraph::ggraph()] or [ggplot2::ggplot()]. It is a wrapper around
#' [ggraph::ggraph()] that first applies some filtering to the provided graph
#' and computes the plot layout. It also sets some default scales and theme
#' elements. If the supplied `graph` object is not already a `clustree_graph`
#' object it will be passed to [as_clustree_graph()] first.
#'
#' @return A `ggraph` object, see [ggraph::ggraph()] for details
#' @export
#' @importFrom ggraph ggraph
#'
#' @seealso [ggraph::ggraph()] for details on the `ggraph` object and
#' [ggplot2::ggplot()] for details on the `ggplot` object
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph)
plot_clustree <- function(graph, layout = c("tree", "sugiyama", "overlay"),
                          count_filter = 0, prop_filter = 0.1,
                          use_core_edges = TRUE, ...) {

    if (!is.clustree_graph(graph)) {
        graph <- as_clustree_graph(graph)
    }
    layout <- match.arg(layout)
    abort_number(count_filter, lower = 0, upper = 1)
    abort_number(prop_filter, lower = 0, upper = 1)

    graph <- tidygraph::activate(graph, "edges")
    graph <- tidygraph::filter(
        graph,
        .data$count >= count_filter,
        .data$in_prop >= prop_filter
    )

    if (use_core_edges) {
        layout_graph <- tidygraph::filter(graph, .data$is_core)
    } else {
        layout_graph <- graph
    }

    layout_data <- switch (layout,
        tree     = ggraph::create_layout(layout_graph, "tree", ...),
        sugiyama = ggraph::create_layout(layout_graph, "sugiyama", ...),
        overlay  = layout_overlay(layout_graph, ...)
    )

    if (use_core_edges) {
        attributes(layout_data)$graph <- graph
    }

    ggraph::ggraph(layout_data) +
        ggraph::theme_graph(
            base_family = "",
            plot_margin = ggplot2::margin(2, 2, 2, 2)
        ) +
        ggplot2::scale_size(range = c(4, 15)) +
        ggraph::scale_edge_colour_viridis() +
        ggraph::scale_edge_alpha(limits = c(0, 1))
}
