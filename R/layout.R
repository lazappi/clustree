#' Layout clustering tree
#'
#' Create a layout for a clustering tree
#'
#' @param x Object containing a clustering tree graph
#' @param layout String specifying the "tree" or "sugiyama" layout. See
#' [igraph::layout_as_tree()] and [igraph::layout_with_sugiyama()] for details.
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
#' layout <- layout_clustree(graph)
layout_clustree <- function(x, ...) {
    UseMethod("layout_clustree")
}

#' @describeIn layout_clustree Default method. Tries to call
#' [as_clustree_graph()] on the input before creating the layout.
#' @export
layout_clustree.default <- function(x, ...) {
    tryCatch({
        layout_clustree(as_clustree_graph(x, ...), ...)
    }, error = function(err) {
        abort(paste0("No support for ", class(x)[1], " objects"))
    })
}

#' @describeIn layout_clustree Method for `clustree_graph` objects. Creates
#' the layout.
#' @export
layout_clustree.clustree_graph <- function(x,
                                           layout = c("tree", "sugiyama"),
                                           ...) {

    layout <- match.arg(layout)

    ggraph::create_layout(x, layout)
}
