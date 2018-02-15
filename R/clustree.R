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
#' @param layout character specifying the "tree" or "sugiyama" layout, see
#' [igraph::layout_as_tree()] and [igraph::layout_with_sugiyama()] for details
#' @param exprs source of gene expression information to use as node aesthetics,
#' for `SingleCellExperiment` objects it must be a name in
#' [SummarizedExperiment::assayNames()], for a `seurat` object it must be one of
#' `data`, `raw.data` or `scale.data`
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
#' @return [ggplot2::ggplot] object containing a clustering tree
#'
#' @examples
#' data(iris_clusts)
#' clustree(iris_clusts, prefix = "K")
#'
#' @export
clustree <- function (x, ...) {
    UseMethod("clustree", x)
}


#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn scale_edge_alpha
#' @importFrom ggplot2 arrow aes_ guides guide_legend scale_size
#' @importFrom grid unit
#'
#' @rdname clustree
#' @export
clustree.matrix <- function(x, prefix,
                            suffix           = NULL,
                            count_filter     = 0,
                            prop_filter      = 0.1,
                            metadata         = NULL,
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
                            edge_width       = 1.5,
                            edge_arrow       = TRUE,
                            layout           = c("tree", "sugiyama"),
                            ...) {

    checkmate::assert_matrix(x, mode = "numeric", any.missing = FALSE,
                             col.names = "unique", min.cols = 2)
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
    checkmate::assert_logical(scale_node_text, any.missing = FALSE, len = 1)
    checkmate::assert_number(edge_width, lower = 0)
    checkmate::assert_logical(edge_arrow, any.missing = FALSE, len = 1)
    layout <- match.arg(layout)

    if (!is.null(suffix)) {
        colnames(x) <- gsub(suffix, "", colnames(x))
    }

    res_clean <- gsub(prefix, "", colnames(x))
    is_num <- suppressWarnings(!any(is.na(as.numeric(res_clean))))
    if (!is_num) {
        stop("The X portion of your clustering column names could not be ",
             "converted to a number. Please check that your prefix and suffix ",
             "are correct: prefix = '", prefix, "', suffix = '", suffix, "'")
    }

    x <- x[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter,
                              metadata, node_colour, node_colour_aggr,
                              node_size, node_size_aggr, node_alpha,
                              node_alpha_aggr)

    graph_attr <- igraph::graph_attr(graph)

    gg <- ggraph(graph, layout = layout)

    # Plot edges
    if (edge_arrow) {
        if (is.numeric(node_size)) {
            circle_size <- node_size * 1.5
        } else {
            circle_size <- mean(node_size_range) * 1.5
        }
        gg <- gg + geom_edge_link(arrow = arrow(length = unit(edge_width * 5,
                                                              "points")),
                                  end_cap = circle(circle_size, "points"),
                                  edge_width = edge_width,
                                  aes_(colour = ~count,
                                      alpha = ~in_prop))
    } else {
        gg <- gg + geom_edge_link(edge_width = edge_width,
                                 aes_(colour = ~count, alpha = ~in_prop))
    }

    gg <- gg + scale_edge_colour_gradientn(colours = viridis::viridis(256)) +
        scale_edge_alpha(limits = c(0, 1))

    # Plot nodes
    gg <- gg + add_node_points(prefix, graph_attr$node_colour,
                               graph_attr$node_size, graph_attr$node_alpha,
                               names(igraph::vertex_attr(graph)))

    # Plot node labels
    if (scale_node_text && !is.numeric(node_size)) {
        gg <- gg + geom_node_text(aes_(label = ~cluster,
                                       size = as.name(node_size)),
                                  colour = node_text_colour)
    } else {
        gg <- gg + geom_node_text(aes_(label = ~cluster), size = node_text_size,
                                  colour = node_text_colour)
    }

    gg <- gg + scale_size(range = node_size_range) +
        theme_clustree()

    return(gg)
}


#' @rdname clustree
#' @export
clustree.data.frame <- function(x, prefix, ...) {

    checkmate::assert_data_frame(x, col.names = "unique")
    checkmate::assert_character(prefix, any.missing = FALSE, len = 1)

    clust_cols <- grepl(prefix, colnames(x))
    if (sum(clust_cols) < 2) {
        stop("Less than two column names matched the prefix: ", prefix,
             call. = FALSE)
    }

    clusterings <- as.matrix(x[, clust_cols])
    mode(clusterings) <- "numeric"

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

    checkmate::assert_class(x, "SingleCellExperiment")
    checkmate::assert_character(exprs, any.missing = FALSE, len = 1)

    if (!(exprs %in% names(x@assays))) {
        stop("exprs must be the name of an assay in x: ",
             paste0(names(x@assays), collapse = ", "))
    }

    args <- list(...)
    for (node_aes in c("node_colour", "node_size", "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% rownames(x)) {
                aes_name <- paste0(exprs, "_", node_aes_value)
                x@colData[aes_name] <-
                    x@assays[[exprs]][node_aes_value, ]
                args[[node_aes]] <- aes_name
            }
        }
    }

    args$x <- data.frame(x@colData)
    args$prefix <- prefix

    do.call(clustree, args)

}


#' @rdname clustree
#'
#' @importFrom methods slot
#' @export
clustree.seurat <- function(x, prefix = "res.",
                            exprs = c("data", "raw.data", "scale.data"), ...) {

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
add_node_points <- function(prefix, node_colour, node_size, node_alpha,
                            allowed) {

    is_allowed <- c(node_colour, node_size, node_alpha) %in% allowed

    if (all(is_allowed == FALSE)) {
        aes_allowed <- "none"
    } else {
        aes_allowed <- c("col", "size", "alpha")[is_allowed]
        aes_allowed <- paste(aes_allowed, collapse = "_")
    }

    switch(aes_allowed,
           col_size_alpha = geom_node_point(aes_(colour = as.name(node_colour),
                                                 size = as.name(node_size),
                                                 alpha = as.name(node_alpha))),
           col_alpha      = geom_node_point(aes_(colour = as.name(node_colour),
                                                 alpha = as.name(node_alpha)),
                                            size = node_size),
           col_size       = geom_node_point(aes_(colour = as.name(node_colour),
                                                 size = as.name(node_size)),
                                            alpha = node_alpha),
           col            = geom_node_point(aes_(colour = as.name(node_colour)),
                                            size = node_size,
                                            alpha = node_alpha),
           size_alpha     = geom_node_point(aes_(size = as.name(node_size),
                                                 alpha = as.name(node_alpha)),
                                            colour = node_colour),
           size           = geom_node_point(aes_(size = as.name(node_size)),
                                            colour = node_colour,
                                            alpha = node_alpha),
           alpha          = geom_node_point(aes_(alpha = as.name(node_alpha)),
                                            colour = node_colour,
                                            size = node_size),
           none           = geom_node_point(colour = node_colour,
                                            size = node_size,
                                            alpha = node_alpha)
    )

}


#' Assert node aesthetics
#'
#' Raise error if an incorrect set of node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation funciton associated with the node aesthetic
#'
#' @importFrom utils head
assert_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                            node_aes_aggr) {

    allowed <- c(prefix, "cluster", "size")

    checkmate::assert_character(node_aes, len = 1, .var.name = node_aes_name)

    if (!is.null(metadata)) {
        if (!(node_aes %in% c(colnames(metadata), allowed))) {
            stop(node_aes_name, " must be one of: ",
                 paste(allowed, collapse = ", "),
                 ", or a column in metadata: ",
                 paste(head(colnames(metadata)), collapse = ", "), "...",
                 call. = FALSE)
        }
    } else {
        if (!(node_aes %in% allowed)) {
            stop("If metadata is not supplied ", node_aes_name,
                 "can only be one of: ", paste(allowed, collapse = ", "),
                 call. = FALSE)
        }
    }

    if (!(node_aes %in% allowed)) {
        checkmate::assert_character(node_aes_aggr, len = 1, any.missing = FALSE,
                                    .var.name = paste0(node_aes_name, "_aggr"))
        if (!is.null(node_aes_aggr)) {
            node_aes_aggr_fun <- match.fun(node_aes_aggr)
            checkmate::assert_function(node_aes_aggr_fun,
                                       .var.name = paste0(node_aes_name,
                                                          "_aggr"))
        }
    }
}


#' Assert numeric node aesthetics
#'
#' Raise error if an incorrect set of numeric node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation funciton associated with the node aesthetic
#' @param min minimum numeric value allowed
#' @param max maximum numeric value allowed
assert_numeric_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                                    node_aes_aggr, min, max) {

    num_chk <- checkmate::check_number(node_aes)

    if (!(num_chk == TRUE)) {
        assert_node_aes(node_aes_name, prefix, metadata, node_aes,
                        node_aes_aggr)
    } else {
        checkmate::assert_number(node_aes, lower = min, upper = max,
                                 .var.name = node_aes_name)
    }

}


#' Assert colour node aesthetics
#'
#' Raise error if an incorrect set of colour node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation funciton associated with the node aesthetic
#' @param min minimum numeric value allowed
#' @param max maximum numeric value allowed
#'
#' @importFrom grDevices col2rgb
assert_colour_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                                   node_aes_aggr, min, max) {

    num_chk <- checkmate::check_number(node_aes)
    allowed <- c(prefix, "cluster", "size", colnames(metadata))

    if (!(num_chk == TRUE)) {
        if (node_aes %in% allowed) {
            assert_node_aes(node_aes_name, prefix, metadata, node_aes,
                            node_aes_aggr)
        } else {
            tryCatch(col2rgb(node_aes),
                     error = function(e) {
                         stop(node_aes_name, " is set to '", node_aes, "' ",
                              "which is not a valid colour name. Other options",
                              " include a number or the name of a metadata ",
                              "column.", call. = FALSE)
                     })
        }
    } else {
        checkmate::assert_number(node_aes, lower = 0, .var.name = node_aes_name)
    }

}


#' clustree theme
#'
#' Default theme used for plotting clustering trees
#'
#' @param base_size overall font size
#' @param base_family base font family
#'
#' @examples
#' qplot(1:10, (1:10) ^ 2) + theme_clustree()
#'
#' @export
theme_clustree <- function(base_size = 14, base_family = "") {

    cowplot::theme_nothing() +
        ggplot2::theme(legend.position = "right")

}
