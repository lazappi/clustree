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
#' @param node_text_size numeric value giving the size of node text if
#' `scale_node_text` is `FALSE`
#' @param scale_node_text logical indicating whether to scale node text along
#' with the node size
#' @param node_text_colour colour value for node text (and label)
#' @param node_text_angle the rotation of the node text
#' @param node_label additional label to add to nodes
#' @param node_label_aggr if `node_label` is a column name than a string
#' giving the name of a function to aggregate that column for samples in each
#' cluster
#' @param node_label_size numeric value giving the size of node label text
#' @param node_label_nudge numeric value giving nudge in y direction for node
#' labels
#' @param edge_width numeric value giving the width of plotted edges
#' @param edge_arrow logical indicating whether to add an arrow to edges
#' @param edge_arrow_ends string indicating which ends of the line to draw arrow
#' heads if `edge_arrow` is `TRUE`, one of "last", "first", or "both"
#' @param show_axis whether to show resolution axis
#' @param exprs source of gene expression information to use as node aesthetics,
#' for `SingleCellExperiment` objects it must be a name in `assayNames(x)`, for
#' a `seurat` object it must be one of `data`, `raw.data` or `scale.data` and
#' for a `Seurat` object it must be one of `data`, `counts` or `scale.data`
#' @param assay name of assay to pull expression and clustering data from for
#' `Seurat` objects
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
#' data(nba_clusts)
#' clustree(nba_clusts, prefix = "K")
#'
#' @export
clustree <- function (x, ...) {
    UseMethod("clustree", x)
}


#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn scale_edge_alpha
#' scale_edge_width_manual
#' @importFrom ggplot2 arrow aes_ guides guide_legend scale_size
#' scale_y_continuous theme element_text
#' @importFrom grid unit
#' @importFrom dplyr %>%
#'
#' @rdname clustree
#' @export
clustree.matrix <- function(x, prefix,
                            suffix           = NULL,
                            metadata         = NULL,
                            count_filter     = 0,
                            prop_filter      = 0.1,
                            layout           = c("tree", "sugiyama"),
                            use_core_edges   = TRUE,
                            highlight_core   = FALSE,
                            node_colour      = prefix,
                            node_colour_aggr = NULL,
                            node_size        = "size",
                            node_size_aggr   = NULL,
                            node_size_range  = c(4, 15),
                            node_alpha       = 1,
                            node_alpha_aggr  = NULL,
                            node_text_size   = 3,
                            scale_node_text  = FALSE,
                            node_text_colour = "black",
                            node_text_angle  = 0,
                            node_label       = NULL,
                            node_label_aggr  = NULL,
                            node_label_size  = 3,
                            node_label_nudge = -0.2,
                            edge_width       = 1.5,
                            edge_arrow       = TRUE,
                            edge_arrow_ends  = c("last", "first", "both"),
                            show_axis        = FALSE,
                            return           = c("plot", "graph", "layout"),
                            ...) {

    checkmate::assert_matrix(x, any.missing = FALSE, col.names = "unique",
                             min.cols = 2)
    checkmate::assert_character(prefix, any.missing = FALSE, len = 1)
    checkmate::assert_character(suffix, any.missing = FALSE, len = 1,
                                null.ok = TRUE)
    checkmate::assert_number(count_filter, lower = 0, upper = nrow(x))
    checkmate::assert_number(prop_filter, lower = 0, upper = 1)
    checkmate::assert_data_frame(metadata, nrows = nrow(x),
                                 col.names = "unique", null.ok = TRUE)
    assert_colour_node_aes("node_colour", prefix, metadata, node_colour,
                           node_colour_aggr)
    assert_numeric_node_aes("node_size", prefix, metadata, node_size,
                            node_size_aggr, 0, Inf)
    assert_numeric_node_aes("node_alpha", prefix, metadata, node_alpha,
                            node_alpha_aggr, 0, 1)
    checkmate::assert_number(node_text_size, lower = 0)
    if (!is.null(node_label)) {
        assert_node_aes("node_label", prefix, metadata, node_label,
                        node_label_aggr)
    }
    checkmate::assert_number(node_label_nudge)
    checkmate::assert_logical(scale_node_text, any.missing = FALSE, len = 1)
    checkmate::assert_number(edge_width, lower = 0)
    checkmate::assert_logical(edge_arrow, any.missing = FALSE, len = 1)
    layout <- match.arg(layout)
    checkmate::assert_flag(use_core_edges)
    return <- match.arg(return)
    edge_arrow_ends <- match.arg(edge_arrow_ends)
    checkmate::assert_flag(highlight_core)

    if (!is.null(suffix)) {
        colnames(x) <- gsub(suffix, "", colnames(x))
    }

    res_clean <- gsub(prefix, "", colnames(x))
    is_num <- suppressWarnings(!any(is.na(as.numeric(res_clean))))
    if (!is_num) {
        stop("The X portion of your clustering column names could not be ",
             "converted to a number. Please check that your prefix and suffix ",
             "are correct: prefix = '", prefix, "', suffix = '", suffix, "'",
             call. = FALSE)
    }

    res_order <- order(as.numeric(res_clean))
    x <- x[, res_order]
    res_clean <- res_clean[res_order]

    if (!(is.null(metadata))) {
        metadata_names <- make.names(colnames(metadata))
        metadata_diff <- metadata_names != colnames(metadata)
        if (any(metadata_diff)) {
            warning(
                "The following metadata column names will be converted from:\n",
                paste(colnames(metadata)[metadata_diff], collapse = ", "), "\n",
                "to:\n",
                paste(metadata_names[metadata_diff], collapse = ", "),
                call. = FALSE
            )
            colnames(metadata) <- metadata_names
        }
    }

    node_aes_list <- list(
        colour = list(value = node_colour, aggr = node_colour_aggr),
        size = list(value = node_size, aggr = node_size_aggr),
        alpha = list(value = node_alpha, aggr = node_alpha_aggr)
    )

    if (!is.null(node_label)) {
        node_aes_list$label <- list(value = node_label, aggr = node_label_aggr)
    }

    node_aes_list <- check_node_aes_list(node_aes_list)

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter,
                              metadata, node_aes_list)

    graph_attr <- igraph::graph_attr(graph)

    graph <- graph %>%
        tidygraph::activate("edges") %>%
        tidygraph::mutate(width = edge_width) %>%
        tidygraph::group_by(.data$to) %>%
        tidygraph::mutate(is_core = .data$in_prop == max(.data$in_prop)) %>%
        tidygraph::ungroup()

    if (use_core_edges) {
        layout <- graph %>%
            tidygraph::activate("edges") %>%
            tidygraph::filter(.data$is_core) %>%
            ggraph::create_layout(layout)

        attributes(layout)$graph <- graph
    } else {
        layout <- ggraph::create_layout(graph, layout)
    }

    gg <- ggraph(layout)

    # Plot edges
    if (edge_arrow) {
        if (is.numeric(node_size)) {
            circle_size_end <- ifelse(edge_arrow_ends == "first", 0.1,
                                      node_size * 1.5)
            circle_size_start <- ifelse(edge_arrow_ends == "last", 0.1,
                                        node_size * 1.5)
        } else {
            circle_size_end <- ifelse(edge_arrow_ends == "first", 0.1,
                                      mean(node_size_range) * 1.5)
            circle_size_start <- ifelse(edge_arrow_ends == "last", 0.1,
                                        mean(node_size_range) * 1.5)
        }
        gg <- gg + geom_edge_link(arrow = arrow(length = unit(edge_width * 5,
                                                              "points"),
                                                ends = edge_arrow_ends),
                                  end_cap = circle(circle_size_end, "points"),
                                  start_cap = circle(circle_size_start, "points"),
                                  aes(colour = .data$count,
                                      alpha = .data$in_prop,
                                      edge_width = .data$is_core))

    } else {
        gg <- gg + geom_edge_link(aes(colour = .data$count,
                                      alpha = .data$in_prop,
                                      edge_width = .data$is_core))
    }

    if (highlight_core) {
        core_width <- edge_width * 2
        gg <- gg + scale_edge_width_manual(values = c(edge_width, core_width))
    } else {
        gg <- gg + scale_edge_width_manual(values = c(edge_width, edge_width),
                                           guide = "none")
    }

    gg <- gg + scale_edge_colour_gradientn(colours = viridis::viridis(256)) +
        scale_edge_alpha(limits = c(0, 1))

    # Plot nodes
    gg <- gg + add_node_points(graph_attr$node_colour, graph_attr$node_size,
                               graph_attr$node_alpha,
                               names(igraph::vertex_attr(graph)))

    # Plot node text
    if (scale_node_text && !is.numeric(node_size)) {
        gg <- gg + geom_node_text(aes(label = .data$cluster,
                                      size = .data[[graph_attr$node_size]]),
                                  colour = node_text_colour,
                                  angle = node_text_angle
                                  )
    } else {
        gg <- gg + geom_node_text(aes(label = .data$cluster),
                                  size = node_text_size,
                                  colour = node_text_colour,
                                  angle = node_text_angle
                                 )
    }

    if (!(is.null(node_label))) {
        gg <- gg + add_node_labels(graph_attr$node_label,
                                   graph_attr$node_colour,
                                   node_label_size,
                                   node_text_colour,
                                   node_label_nudge,
                                   names(igraph::vertex_attr(graph)))
    }

    gg <- gg + scale_size(range = node_size_range) +
        ggraph::theme_graph(base_family = "",
                            plot_margin = ggplot2::margin(2, 2, 2, 2))

    if (show_axis) {
        gg <- gg +
            ylab(prefix) +
            scale_y_continuous(breaks = sort(unique(layout$y)),
                               labels = rev(res_clean)) +
            theme(axis.text.y = element_text(),
                  axis.title = element_text(),
                  axis.title.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey92"))
    }

    if (return == "plot") {
        return(gg)
    } else if (return == "graph") {
        return(graph)
    } else if (return == "layout") {
        return(layout)
    }
}


#' @rdname clustree
#' @export
clustree.data.frame <- function(x, prefix, ...) {

    checkmate::assert_data_frame(x, col.names = "unique")
    checkmate::assert_character(prefix, any.missing = FALSE, len = 1)

    cols_prefix <- substr(colnames(x), 1, nchar(prefix))
    clust_cols <- cols_prefix == prefix

    if (sum(clust_cols) < 2) {
        stop("Less than two column names matched the prefix: ", prefix,
             call. = FALSE)
    }

    clusterings <- as.matrix(x[, clust_cols])

    if (sum(!clust_cols) > 0) {
        metadata <- x[, !clust_cols, drop = FALSE]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, prefix, metadata = metadata, ...)
}

#' @rdname clustree
#' @export
clustree.SingleCellExperiment <- function(x, prefix, exprs = "counts", ...) {

    if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
        stop("The SingleCellExperiment package is missing, this must be",
             "installed for clustree to use SingleCellExperiment objects",
             call. = FALSE)
    }

    checkmate::assert_class(x, "SingleCellExperiment")
    checkmate::assert_character(exprs, any.missing = FALSE, len = 1)

    if (!(exprs %in% names(x@assays))) {
        stop("exprs must be the name of an assay in x: ",
             paste0(names(x@assays), collapse = ", "), call. = FALSE)
    } else {
        exprs_mat <- SummarizedExperiment::assay(x, exprs)
    }

    args <- list(...)
    node_aes_sel <- c("node_colour", "node_size", "node_alpha")
    node_aes_sel <- node_aes_sel[node_aes_sel %in% names(args)]
    for (node_aes in node_aes_sel) {
        node_aes_value <- args[[node_aes]]
        if (node_aes_value %in% rownames(x)) {
            node_aes_name <- make.names(node_aes_value)
            if (node_aes_value != node_aes_name) {
                warning(
                    "The feature name ", node_aes_value,
                    " will be converted to ", node_aes_name,
                    call. = FALSE
                )
            }
            aes_name <- paste0(exprs, "_", node_aes_name)
            x@colData[aes_name] <- exprs_mat[node_aes_value, ]
            args[[node_aes]] <- aes_name
        }
    }

    args$x <- data.frame(x@colData)
    args$prefix <- prefix

    do.call(clustree, args)
}


#' @rdname clustree
#'
#' @importFrom methods slot
#' @importFrom utils packageVersion
#'
#' @export
clustree.seurat <- function(x, prefix = "res.",
                            exprs = c("data", "raw.data", "scale.data"), ...) {

    if (!requireNamespace("Seurat", quietly = TRUE)) {
        stop("The Seurat package is missing, this must be installed for ",
             "clustree to use seurat objects", call. = FALSE)
    }

    warning(
        "This interface is for the older seurat object in Seurat < 3.0.0 and ",
        "may be deprecated in the future. You currently have Seurat v",
        packageVersion("Seurat"), " installed. Consider installing a newer ",
        "version of Seurat and updating your object.",
        call. = FALSE
    )

    checkmate::assert_class(x, "seurat")
    checkmate::assert_character(exprs, any.missing = FALSE)

    exprs <- match.arg(exprs)

    args <- list(...)
    gene_names <- rownames(x@raw.data)
    for (node_aes in c("node_colour", "node_size", "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% gene_names) {
                aes_name <- paste0(exprs, "_", node_aes_value)
                x@meta.data[aes_name] <-
                    slot(x, exprs)[node_aes_value, ]
                args[[node_aes]] <- aes_name
            }
        }
    }

    args$x <- x@meta.data
    args$prefix <- prefix

    do.call(clustree, args)
}


#' @rdname clustree
#'
#' @export
clustree.Seurat <- function(x, prefix = paste0(assay, "_snn_res."),
                            exprs = c("data", "counts", "scale.data"),
                            assay = NULL, ...) {

    if (!requireNamespace("Seurat", quietly = TRUE)) {
        stop("The Seurat package is missing, this must be installed for ",
             "clustree to use Seurat objects", call. = FALSE)
    }

    checkmate::assert_class(x, "Seurat")
    checkmate::assert_character(exprs, any.missing = FALSE)

    if (is.null(x = assay)) {
        assay <- Seurat::DefaultAssay(object = x)
    } else {
        Seurat::DefaultAssay(x) <- assay
    }

    exprs <- match.arg(arg = exprs)
    args <- list(...)
    node_aes_sel <- c("node_colour", "node_size", "node_alpha")
    node_aes_sel <- node_aes_sel[node_aes_sel %in% names(args)]
    for (node_aes in node_aes_sel) {
        node_aes_value <- args[[node_aes]]
        if (node_aes_value %in% rownames(x)) {
            node_aes_name <- make.names(node_aes_value)
            if (node_aes_value != node_aes_name) {
                warning(
                    "The feature name ", node_aes_value,
                    " will be converted to ", node_aes_name,
                    call. = FALSE
                )
            }
            aes_name <- paste0(exprs, "_", node_aes_name)
            if (packageVersion("SeuratObject") >= package_version("5.0.0")) {
                x[[aes_name]] <- Seurat::FetchData(x, vars = node_aes_value,
                                                   layer = exprs)
            } else {
                x[[aes_name]] <- Seurat::FetchData(x, vars = node_aes_value,
                                                   slot = exprs)
            }
            args[[node_aes]] <- aes_name
        }
    }

    args$x <- x[[]]
    args$prefix <- prefix

    do.call(clustree, args)
}


#' Add node points
#'
#' Add node points to a clustering tree plot with the specified aesthetics.
#'
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param allowed vector of allowed node attributes to use as aesthetics
#'
#' @keywords internal
#'
#' @importFrom ggraph geom_node_point
#' @importFrom ggplot2 aes_
add_node_points <- function(node_colour, node_size, node_alpha, allowed) {

    is_allowed <- c(node_colour, node_size, node_alpha) %in% allowed

    if (all(is_allowed == FALSE)) {
        aes_allowed <- "none"
    } else {
        aes_allowed <- c("col", "size", "alpha")[is_allowed]
        aes_allowed <- paste(aes_allowed, collapse = "_")
    }

    switch(aes_allowed,
           col_size_alpha = geom_node_point(aes(colour = .data[[node_colour]],
                                                size = .data[[node_size]],
                                                alpha = .data[[node_alpha]])),
           col_alpha      = geom_node_point(aes(colour = .data[[node_colour]],
                                                alpha = .data[[node_alpha]]),
                                            size = node_size),
           col_size       = geom_node_point(aes(colour = .data[[node_colour]],
                                                size = .data[[node_size]]),
                                            alpha = node_alpha),
           col            = geom_node_point(aes(colour = .data[[node_colour]]),
                                            size = node_size,
                                            alpha = node_alpha),
           size_alpha     = geom_node_point(aes(size = .data[[node_size]],
                                                alpha = .data[[node_alpha]]),
                                            colour = node_colour),
           size           = geom_node_point(aes(size = .data[[node_size]]),
                                            colour = node_colour,
                                            alpha = node_alpha),
           alpha          = geom_node_point(aes(alpha = .data[[node_alpha]]),
                                            colour = node_colour,
                                            size = node_size),
           none           = geom_node_point(colour = node_colour,
                                            size = node_size,
                                            alpha = node_alpha)
    )

}


#' Add node labels
#'
#' Add node labels to a clustering tree plot with the specified aesthetics.
#'
#' @param node_label the name of a metadata column for node labels
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_label_size size of node label text
#' @param node_label_colour colour of node_label text
#' @param node_label_nudge numeric value giving nudge in y direction for node
#' labels
#' @param allowed vector of allowed node attributes to use as aesthetics
#'
#' @keywords internal
#'
#' @importFrom ggraph geom_node_label
#' @importFrom ggplot2 aes_
add_node_labels <- function(node_label, node_colour, node_label_size,
                            node_label_colour, node_label_nudge, allowed) {

    is_allowed <- c(node_colour) %in% allowed

    if (is_allowed) {
        geom_node_label(aes(label = .data[[node_label]],
                            fill = .data[[node_colour]]),
                        size = node_label_size,
                        colour = node_label_colour,
                        nudge_y = node_label_nudge)
    } else {
        geom_node_label(aes(label = .data[[node_label]]),
                        fill = node_colour,
                        size = node_label_size,
                        colour = node_label_colour,
                        nudge_y = node_label_nudge)
    }

}
