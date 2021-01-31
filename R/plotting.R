#' Plot clustering tree
#'
#' Plot a clustering tree
#'
#' @param ... Arguments used by other methods
#'
#' @return A `ggplot` object, see [ggplot2::ggplot()] for details
#' @export
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

plot_clustree_nodes <- function(mapping = NULL, ...) {

    mapping <- set_default_node_aes(mapping)

    ggraph::geom_node_point(mapping = mapping, ...)
}

plot_clustree_text <- function(mapping = NULL, ...) {

    user_args <- list(...)

    args <- list(
        mapping = set_default_text_aes(mapping),
        size    = 3
    )

    args[names(user_args)] <- user_args

    do.call(ggraph::geom_node_text, args)
}

plot_clustree_edges <- function(mapping = NULL, ...) {

    user_args <- list(...)

    args <- list(
        mapping = set_default_edge_aes(mapping),
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

set_default_node_aes <- function(aes) {

    default_aes <- ggplot2::aes(colour = .data$res, size = .data$size)

    if (is.null(aes)) {
        return(default_aes)
    }

    if (any(names(aes) == "color")) {
        names(aes)[names(aes)] == "color" <- "colour"
    }

    aes <- c(as.list(aes), default_aes[!(names(default_aes) %in% names(aes))])

    class(aes) <- "uneval"

    return(aes)
}

set_default_text_aes <- function(aes) {

    default_aes <- ggplot2::aes(label = .data$cluster)

    if (is.null(aes)) {
        return(default_aes)
    }

    if (any(names(aes) == "color")) {
        names(aes)[names(aes)] == "color" <- "colour"
    }

    aes <- c(as.list(aes), default_aes[!(names(default_aes) %in% names(aes))])

    class(aes) <- "uneval"

    return(aes)
}

set_default_edge_aes <- function(aes) {

    default_aes <- ggplot2::aes(
        edge_colour = .data$count,
        edge_alpha  = .data$in_prop
    )

    if (is.null(aes)) {
        return(default_aes)
    }

    if (any(names(aes) == "color")) {
        names(aes)[names(aes) == "color"] <- "colour"
    }

    if (any(names(aes) == "edge_color")) {
        names(aes)[names(aes) == "edge_color"] <- "edge_colour"
    }

    is_short_names <- names(aes) %in%
        c("colour", "fill", "linetype", "shape", "size", "width", "alpha")

    names(aes)[is_short_names] <- paste0("edge_", names(aes)[is_short_names])

    aes <- c(as.list(aes), default_aes[!(names(default_aes) %in% names(aes))])

    class(aes) <- "uneval"

    return(aes)
}
