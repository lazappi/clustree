#' Plot a clustering tree
#'
#' Creates a plot of a clustering tree showing the relationship between
#' clusterings at different resolutions.
#'
#' @param x object containing clustering data
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param prefix string indicating columns containing clustering information
#' @param suffix string at the end of column names containing clustering
#' information
#' @param count_filter count threshold for filtering edges in the clustering
#' graph
#' @param prop_filter in proportion threshold for filtering edges in the
#' clustering graph
#' @param layout string specifying the "tree" or "sugiyama" layout, see
#' [igraph::layout_as_tree()] and [igraph::layout_with_sugiyama()] for details
#' @param use_core_edges logical, whether to only use core tree (edges with
#' maximum in proportion for a node) when creating the graph layout, all
#' (unfiltered) edges will still be displayed
#' @param highlight_core logical, whether to increase the edge width of the core
#' network to make it easier to see
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_colour_aggr if `node_colour` is a column name than a string
#' giving the name of a function to aggregate that column for samples in each
#' cluster
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_size_aggr if `node_size` is a column name than a string
#' giving the name of a function to aggregate that column for samples in each
#' cluster
#' @param node_size_range numeric vector of length two giving the maximum and
#' minimum point size for plotting nodes
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param node_alpha_aggr if `node_aggr` is a column name than a string
#' giving the name of a function to aggregate that column for samples in each
#' cluster
#' @param node_text_size numeric value giving the size of node labels if
#' `scale_node_text` is `FALSE`
#' @param scale_node_text logical indicating whether to scale node labels along
#' with the node size
#' @param node_text_colour colour value for node labels
#' @param edge_width numeric value giving the width of plotted edges
#' @param edge_arrow logical indicating whether to add an arrow to edges
#' @param edge_arrow_ends string indicating which ends of the line to draw arrow
#' heads if `edge_arrow` is `TRUE`, one of "last", "first", or "both"
#' @param exprs source of gene expression information to use as node aesthetics,
#' for `SingleCellExperiment` objects it must be a name in
#' [SummarizedExperiment::assayNames()], for a `seurat` object it must be one of
#' `data`, `raw.data` or `scale.data`
#' @param return string specifying what to return, either "plot" (a `ggplot`
#' object), "graph" (a `tbl_graph` object) or "layout" (a `ggraph` layout
#' object)
#' @param ... extra parameters passed to other methods
#'
#' @details
#'
#' **Data sources**
#'
#' Plotting a clustering tree requires information about which cluster each
#' sample has been assigned to at different resolutions. This information can
#' be supplied in various forms, as a matrix, data.frame or more specialised
#' object. In all cases the object provided must contain numeric columns with
#' the naming structure `PXS` where `P` is a prefix indicating that the column
#' contains clustering information, `X` is a numeric value indicating the
#' clustering resolution and `S` is any additional suffix to be removed. For
#' `SingleCellExperiment` objects this information must be in the `colData` slot
#' and for `Seurat` objects it must be in the `meta.data` slot. For all objects
#' except matrices any additional columns can be used as aesthetics, for
#' matrices an additional metadata data.frame can be supplied if required.
#'
#' **Filtering**
#'
#' Edges in the graph can be filtered by adjusting the `count_filter` and
#' `prop_filter` parameters. The `count_filter` removes any edges that represent
#' less than that number of samples, while the `prop_filter` removes edges that
#' represent less than that proportion of cells in the node it points towards.
#'
#' **Node aesthetics**
#'
#' The aesthetics of the plotted nodes can be controlled in various ways. By
#' default the colour indicates the clustering resolution, the size indicates
#' the number of samples in that cluster and the transparency is set to 100%.
#' Each of these can be set to a specific value or linked to a supplied metadata
#' column. For a `SingleCellExperiment` or `Seurat` object the names of genes
#' can also be used. If a metadata column is used than an aggregation function
#' must also be supplied to combine the samples in each cluster. This function
#' must take a vector of values and return a single value.
#'
#' **Layout**
#'
#' The clustering tree can be displayed using either the Reingold-Tilford tree
#' layout algorithm or the Sugiyama layout algorithm for layered directed
#' acyclic graphs. These layouts were selected as the are the algorithms
#' available in the `igraph` package designed for trees. The Reingold-Tilford
#' algorithm places children below their parents while the Sugiyama places
#' nodes in layers while trying to minimise the number of crossing edges. See
#' [igraph::layout_as_tree()] and [igraph::layout_with_sugiyama()] for more
#' details. When `use_core_edges` is `TRUE` (default) only the core tree of the
#' maximum in proportion edges for each node are used for constructing the
#' layout. This can often lead to more attractive layouts where the core tree is
#' more visible.
#'
#' @return a `ggplot` object (default), a `tbl_graph` object or a `ggraph`
#' layout object depending on the value of `return`
#'
#' @examples
#' data(iris_clusts)
#' clustree(iris_clusts, prefix = "K")
#'
#' @export
clustree_overlay <- function (x, ...) {
    UseMethod("clustree_overlay", x)
}


#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn scale_edge_alpha
#' scale_edge_width_manual
#' @importFrom ggplot2 arrow aes_ guides guide_legend scale_size
#' @importFrom grid unit
#' @importFrom dplyr %>%
#'
#' @rdname clustree_overlay
#' @export
clustree_overlay.matrix <- function(
                            x, prefix, metadata, x_value, y_value,
                            suffix           = NULL,
                            count_filter     = 0,
                            prop_filter      = 0.1,
                            node_colour      = prefix,
                            node_colour_aggr = NULL,
                            node_size        = "size",
                            node_size_aggr   = NULL,
                            node_size_range  = c(4, 15),
                            node_alpha       = 1,
                            node_alpha_aggr  = NULL,
                            node_text_size   = 3,
                            node_text_colour = "black",
                            edge_width       = 1,
                            point_colour     = "black",
                            point_size       = 1,
                            point_alpha      = 0.1,
                            point_shape      = 18,
                            return           = c("plot", "graph", "layout"),
                            ...) {

    node_aes_list <- list(
        x_value = list(value = x_value, aggr = "mean"),
        y_value = list(value = y_value, aggr = "mean"),
        colour = list(value = node_colour, aggr = node_colour_aggr),
        size = list(value = node_size, aggr = node_size_aggr),
        alpha = list(value = node_alpha, aggr = node_alpha_aggr)
    )

    points <- metadata[, c(x_value, y_value)]
    colnames(points) <- c("x", "y")
    points$cluster <- x[, ncol(x)]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter,
                              metadata, node_aes_list)

    graph_attr <- igraph::graph_attr(graph)

    nodes <- graph %>%
        tidygraph::activate("nodes") %>%
        data.frame()

    x_val <- paste0("mean_", x_value)
    y_val <- paste0("mean_", y_value)

    edges <- graph %>%
        tidygraph::activate("edges") %>%
        tidygraph::mutate(from_x = tidygraph::.N()[[x_val]][from],
                          from_y = tidygraph::.N()[[y_val]][from],
                          to_x = tidygraph::.N()[[x_val]][to],
                          to_y = tidygraph::.N()[[y_val]][to]) %>%
        data.frame()

    ggplot(points, aes(x = x, y = y)) +
        geom_point(colour = point_colour, size = point_size,
                   alpha = point_alpha, shape = point_shape) +
        overlay_node_points(nodes, graph_attr$node_x_value,
                            graph_attr$node_y_value, graph_attr$node_colour,
                            graph_attr$node_size, graph_attr$node_alpha) +
        geom_segment(data = edges,
                    aes(x = from_x, y = from_y,
                        xend = to_x, yend = to_y,
                        alpha = in_prop, colour = factor(from_res)),
                     arrow = arrow(length = unit(0.02, "npc")),
                     size = edge_width) +
        scale_size(range = c(node_size_range[1], node_size_range[2])) +
        scale_colour_viridis_d() +
        cowplot::theme_cowplot()
}

#' Add node points
#'
#' Add node points to a clustering tree plot with the specified aesthetics.
#'
#' @param prefix string indicating columns containing clustering information
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param allowed vector of allowed node attributes to use as aesthetics
#'
#' @importFrom ggraph geom_node_point
#' @importFrom ggplot2 aes_
overlay_node_points <- function(nodes, x_value, y_value, node_colour, node_size,
                                node_alpha) {

    is_allowed <- c(node_colour, node_size, node_alpha) %in% colnames(nodes)

    if (all(is_allowed == FALSE)) {
        aes_allowed <- "none"
    } else {
        aes_allowed <- c("col", "size", "alpha")[is_allowed]
        aes_allowed <- paste(aes_allowed, collapse = "_")
    }

    switch(aes_allowed,
           col_size_alpha = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            fill = as.name(node_colour),
                                            size = as.name(node_size),
                                            alpha = as.name(node_alpha)),
                                       shape = 21),
           col_alpha      = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            fill = as.name(node_colour),
                                            alpha = as.name(node_alpha)),
                                       size = node_size,
                                       shape = 21),
           col_size       = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            fill = as.name(node_colour),
                                            size = as.name(node_size)),
                                       alpha = node_alpha,
                                       shape = 21),
           col            = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            fill = as.name(node_colour)),
                                        size = node_size,
                                        alpha = node_alpha,
                                        shape = 21),
           size_alpha     = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            fill = as.name(node_size),
                                            alpha = as.name(node_alpha)),
                                       colour = node_colour,
                                       shape = 21),
           size           = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            size = as.name(node_size)),
                                       fill = node_colour,
                                       alpha = node_alpha,
                                       shape = 21),
           alpha          = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value),
                                            alpha = as.name(node_alpha)),
                                       fill = node_colour,
                                       size = node_size,
                                       shape = 21),
           none           = geom_point(data = nodes,
                                       aes_(x = as.name(x_value),
                                            y = as.name(y_value)),
                                       fill = node_colour,
                                       size = node_size,
                                       alpha = node_alpha,
                                       shape = 21)
    )

}