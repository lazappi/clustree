#' Clustering tree all-in-one functions
#'
#' All-in-one clustering tree plotting functions. In previous versions of
#' **{clustre}** these were the default (and only) interface. They are now
#' soft-deprecated with limited functionality and it is recommended to instead
#' use [build_clustree_graph()] to construct the clustering tree graph, followed
#' by [plot_clustree()] to create the plot.
#'
#' @param x Object containing clustering data
#' @param ... Arguments passed to [build_clustree_graph()] and
#' [plot_clustree()]. Any additional arguments not used by these functions will
#' be ignored with a warning.
#'
#' @details
#' This functions are maintained for compatibility with code developed for
#' older versions of **{clustree}**, however the functionality is more limited
#' and you should receive a warning if you attempt to use arguments that are no
#' longer supported. It is recommended that code is updated to use
#' [build_clustree_graph()] and [plot_clustree()] directly.
#'
#' The supplied `x` object is based to [build_clustree_graph()] to create the
#' clustering tree graph which is plotted using [plot_clustree()] with the
#' default [geom_clustree_edge()], [geom_clustree_point()] and
#' [geom_clustree_text()] geoms. For `clustree_overlay()` an additional step
#' involving [summarise_metadata()] is used to add additional metadata columns
#' from `x_value` and `y_value`.
#'
#' @return A `ggraph` object, see [ggraph::ggraph()] for details
#' @importFrom methods formalArgs
#' @importFrom utils getS3method
#' @export
#'
#' @examples
#' clustree(nba_clusts, prefix = "K")
#'
clustree <- function(x, ...) {

    warn(paste(
        "The clustree() function is soft-deprecated and has limited",
        "functionality. It is recommended you use the build_clustree_graph()",
        "and plot_clustree() functions instead. Please see the clustree",
        "vignettes for details."
    ), .frequency = "once", .frequency_id = "clustree")

    build_graph_fun <- getS3method("build_clustree_graph", class(x))
    build_graph_args <- formalArgs(build_graph_fun)

    plot_clustree_args <- formalArgs("plot_clustree")

    all_args <- c(build_graph_args, plot_clustree_args)

    dots <- list(...)
    if (!all(names(dots) %in% all_args)) {
        warn(paste(
            "The following additional arguments cannot be passed to either",
            "build_clustree_graph() or plot_clustree() and will be ignored:",
            paste0(names(dots)[!(names(dots) %in% all_args)], collapse = ", ")
        ))
    }

    dots$x <- x
    graph <- do.call(
        build_clustree_graph,
        dots[names(dots) %in% build_graph_args]
    )

    dots$graph <- graph
    plot <- do.call(
        plot_clustree,
        dots[names(dots) %in% plot_clustree_args]
    )

    plot +
        geom_clustree_edge() +
        geom_clustree_point() +
        geom_clustree_text()
}

#' @rdname clustree
#'
#' @param x_value Name of a numeric metadata column to use as the x-axis
#' @param y_value Name of a numeric metadata column to use as the y-axis
#'
#' @export
#'
#' @examples
#' clustree_overlay(nba_clusts, prefix = "K", x_value = "PC1", y_value = "PC2")
clustree_overlay <- function(x, x_value, y_value, ...) {

    warn(paste(
        "The clustree_overlay() function is soft-deprecated and has limited",
        "functionality. It is recommended you use the build_clustree_graph()",
        "and plot_clustree() functions instead. Please see the clustree",
        "vignettes for details."
    ), .frequency = "once", .frequency_id = "clustree")

    build_graph_fun <- getS3method("build_clustree_graph", class(x))
    build_graph_args <- formalArgs(build_graph_fun)

    plot_clustree_args <- formalArgs("plot_clustree")

    all_args <- c(build_graph_args, plot_clustree_args)

    dots <- list(...)
    if (!all(names(dots) %in% all_args)) {
        warn(paste(
            "The following additional arguments cannot be passed to either",
            "build_clustree_graph() or plot_clustree() and will be ignored:",
            paste0(names(dots)[!(names(dots) %in% all_args)], collapse = ", ")
        ))
    }

    dots$x <- x
    graph <- do.call(
        build_clustree_graph,
        dots[names(dots) %in% build_graph_args]
    )
    graph <- summarize_metadata(
        graph,
        !!x_value := mean(!!rlang::sym(x_value)),
        !!y_value := mean(!!rlang::sym(y_value))
    )

    dots$graph <- graph
    dots$layout <- "overlay"
    dots$x_dim <- x_value
    dots$y_dim <- y_value
    plot <- do.call(
        plot_clustree,
        dots[names(dots) %in% c(plot_clustree_args, "x_dim", "y_dim")]
    )

    plot <- plot +
        geom_clustree_edge(end_cap = ggraph::circle(0, "points")) +
        geom_clustree_point() +
        geom_clustree_text()

    alternate_clustree_layers(plot)
}
