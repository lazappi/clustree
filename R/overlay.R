#' Overlay clustering tree
#'
#' Create a layout for a clustering tree by overlaying nodes onto given
#' dimensions
#'
#' @param x Object containing a clustering tree graph
#' @param x_dim Name of the node column to be used as the x dimension
#' @param y_dim Name of the node column to be used as the y dimension
#' @param ... Arguments used by other methods
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
#' overlay <- overlay_clustree(graph, x_dim = "PC1", y_dim = "PC2")
overlay_clustree <- function(x, ...) {
    UseMethod("overlay_clustree")
}

#' @describeIn overlay_clustree Default method. Tries to call
#' [as_clustree_graph()] on the input before creating the layout.
#' @export
overlay_clustree.default <- function(x, ...) {
    tryCatch({
        overlay_clustree(as_clustree_graph(x, ...), ...)
    }, error = function(err) {
        abort(paste0("No support for ", class(x)[1], " objects'"))
    })
}

#' @describeIn overlay_clustree Method for `clustree_graph` objects. Creates
#' the layout.
#' @export
overlay_clustree.clustree_graph <- function(x, x_dim, y_dim, ...) {

    x <- tidygraph::activate(x, "nodes")
    node_data <- as.data.frame(x)

    abort_character(x_dim, len = 1, any.missing = FALSE)
    abort_character(y_dim, len = 1, any.missing = FALSE)

    if (!(x_dim %in% colnames(node_data))) {
        abort("x_dim must be the name of a node column")
    }

    if (!(y_dim %in% colnames(node_data))) {
        abort("y_dim must be the name of a node column")
    }

    layout <- dplyr::select(node_data, x = {{ x_dim }}, y = {{ y_dim }})

    ggraph::create_layout(x, layout)
}
