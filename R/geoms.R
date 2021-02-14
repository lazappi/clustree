#' Clustree nodes
#'
#' The clustree nodes geom is used to add points representing each node in the
#' graph to a clustering tree plot. It is a minimal wrapper around
#' [ggraph::geom_node_point()] that sets some default aesthetics.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' @param ... Other argument passed on to [ggraph::geom_node_point()]
#'
#' @section Default aesthetics:
#'
#' * `colour = res`
#' * `size = size`
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_text()] and [geom_clustree_edges()] for other clustering tree
#' geoms. See [ggraph::geom_node_point()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_nodes()
geom_clustree_nodes <- function(mapping = NULL, ...) {

    default_aes <- ggplot2::aes(colour = .data$res, size = .data$size)

    mapping <- set_default_aes(mapping, default_aes)

    ggraph::geom_node_point(mapping = mapping, ...)
}

#' Clustree text
#'
#' The clustree text geom is used to add text labelling each node in a
#' graph to a clustering tree plot. It is a minimal wrapper around
#' [ggraph::geom_node_text()] that sets some default aesthetics.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' @param ... Other argument passed on to [ggraph::geom_node_text()]
#'
#' @section Default aesthetics:
#'
#' * `label = cluster`
#' * `size = 3`
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_nodes()] and [geom_clustree_edges()] for other clustering tree
#' geoms. See [ggraph::geom_node_text()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_text()
geom_clustree_text <- function(mapping = NULL, ...) {

    user_args <- list(...)

    default_aes <- ggplot2::aes(label = .data$cluster)

    args <- list(
        mapping = set_default_aes(mapping, default_aes),
        size    = 3
    )

    args[names(user_args)] <- user_args

    do.call(ggraph::geom_node_text, args)
}

#' Clustree edges
#'
#' The clustree edges geom is used to add lines representing edges in a
#' graph to a clustering tree plot. It is a minimal wrapper around
#' [ggraph::geom_edge_link()] that sets some default aesthetics.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' @param ... Other argument passed on to [ggraph::geom_edge_link()]
#'
#' @section Default aesthetics:
#'
#' * `edge_colour = count`
#' * `edge_alpha = in_prop`
#' * `width = 1.5`
#' * `arrow = grid::arrow(length = grid::unit(7.5, "points"), ends = "last")`
#' * `end_cap = ggraph::circle(15.5, "points")`
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_nodes()] and [geom_clustree_text()] for other clustering tree
#' geoms. See [ggraph::geom_edge_link()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_edges()
geom_clustree_edges <- function(mapping = NULL, ...) {

    user_args <- list(...)

    default_aes <- ggplot2::aes(
        edge_colour = .data$count,
        edge_alpha  = .data$in_prop
    )

    args <- list(
        mapping = set_default_aes(mapping, default_aes, edge = TRUE),
        width   = 1.5,
        arrow   = grid::arrow(
            length = grid::unit(7.5, "points"),
            ends   = "last"
        ),
        end_cap = ggraph::circle(15.5, "points")
    )

    args[names(user_args)] <- user_args

    do.call(ggraph::geom_edge_link, args)
}
