#' Plot clustering tree
#'
#' Create a clustering tree plot
#'
#' @param x Object containing a clustering tree graph layout
#' @param ... Arguments used by other methods
#'
#' @details
#' The purpose of [plot_clustree()] is to create a plot object in a similar way
#' to [ggplot2::ggplot()]. It is a minimal wrapper around [ggraph::ggraph()]
#' that sets some default scales. If the supplied object is not already a
#' `layout_ggraph` object it will be passed to [layout_clustree()] first.
#'
#' @return A `ggplot` object, see [ggplot2::ggplot()] for details
#' @export
#'
#' @seealso [ggplot2::ggplot()] for details on the `ggplot` object
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph)
plot_clustree <- function(x, ...) {
    UseMethod("plot_clustree")
}

#' @describeIn plot_clustree Default method. Tries to call
#' [as_clustree_graph()] on the input before creating the plot.
#' @export
plot_clustree.default <- function(x, ...) {
    tryCatch({
        plot_clustree(as_clustree_graph(x, ...), ...)
    }, error = function(err) {
        abort(paste0("No support for ", class(x)[1], " objects'"))
    })
}

#' @describeIn plot_clustree Method for `clustree_graph` objects. Calls
#' [layout_clustree()] on the input before creating the plot.
#' @export
plot_clustree.clustree_graph <- function(x, ...) {
    plot_clustree(layout_clustree(x, ...), ...)
}

#' @describeIn plot_clustree Method for `layout_ggraph` objects. Creates
#' the plot.
#' @export
plot_clustree.layout_ggraph <- function(x, ...) {
    ggraph::ggraph(x, ...) +
        ggraph::theme_graph(
            base_family = "",
            plot_margin = ggplot2::margin(2, 2, 2, 2)
        ) +
        ggplot2::scale_size(range = c(4, 15)) +
        ggraph::scale_edge_colour_gradientn(colours = viridis::viridis(256)) +
        ggraph::scale_edge_alpha(limits = c(0, 1))
}
