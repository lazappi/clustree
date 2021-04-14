#' Layout overlay
#'
#' Create a layout for a clustering tree by overlaying nodes onto given
#' dimensions. Usually used by setting `layout = "overlay"` in [plot_clustree()]
#' rather than by calling this function directly.
#'
#' @param graph Object containing a clustering tree graph
#' @param x_dim Name of the node column to be used as the x dimension
#' @param y_dim Name of the node column to be used as the y dimension
#'
#' @return A `layout_ggraph` object, see [ggraph::layout_ggraph()] for details
#' @export
#'
#' @seealso [ggraph::layout_ggraph()] for details on the `layout_ggraph` object
#' and layouts in general
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' graph <- summarise_metadata(graph, PC1 = mean(PC1), PC2 = mean(PC2))
#' layout_overlay(graph, x_dim = "PC1", y_dim = "PC2")
layout_overlay <- function(graph, x_dim, y_dim) {

    if (!tidygraph::is.tbl_graph(graph)) {
        graph <- tidygraph::as_tbl_graph(graph)
    }
    abort_character(x_dim, len = 1, any.missing = FALSE)
    abort_character(y_dim, len = 1, any.missing = FALSE)

    graph <- tidygraph::activate(graph, "nodes")
    node_data <- as.data.frame(graph)

    if (!(x_dim %in% colnames(node_data))) {
        abort("x_dim must be the name of a node column")
    }

    if (!(y_dim %in% colnames(node_data))) {
        abort("y_dim must be the name of a node column")
    }

    layout <- dplyr::select(node_data, x = {{ x_dim }}, y = {{ y_dim }})

    if (!is.numeric(layout$x)) {
        layout$x <- as.numeric(as.factor(layout$x))
    }

    if (!is.numeric(layout$y)) {
        layout$y <- as.numeric(as.factor(layout$y))
    }

    ggraph::create_layout(graph, layout)
}
