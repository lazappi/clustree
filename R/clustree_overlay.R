#' Overlay a clustering tree
#'
#' Creates a plot of a clustering tree overlaid on a scatter plot of individual
#' samples.
#'
#' @param x object containing clustering data
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param x_value numeric metadata column to use as the x axis
#' @param y_value numeric metadata column to use as the y axis
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
#' @param use_colour one of "edges" or "points" specifying which element to
#' apply the colour aesthetic to
#' @param alt_colour colour value to be used for edges or points (whichever is
#' NOT given by `use_colour`)
#' @param point_size numeric value giving the size of sample points
#' @param point_alpha numeric value giving the alpha of sample points
#' @param point_shape numeric value giving the shape of sample points
#' @param label_nodes logical value indicating whether to add labels to
#' clustering graph nodes
#' @param label_size numeric value giving the size of node labels is
#' `label_nodes` is `TRUE`
#' @param edge_width numeric value giving the width of plotted edges
#' @param exprs source of gene expression information to use as node aesthetics,
#' for `SingleCellExperiment` objects it must be a name in
#' [SummarizedExperiment::assayNames()], for a `seurat` object it must be one of
#' `data`, `raw.data` or `scale.data`
#' @param red_dim dimensionality reduction to use as a source for x_value and
#' y_value
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
#' except matrices any additional columns can be used as aesthetics.
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
#' **Colour aesthetic**
#'
#' The colour aesthetic can be applied to either edges or sample points by
#' setting `use_colour`. If "edges" is selected edges will be coloured according
#' to the clustering resolution they orginate at. If "points" is selected they
#' will be coloured according to the cluster they are assigned to at the highest
#' resolution.
#'
#' **Dimensionality reductions**
#'
#' For `SingleCellExperiment` and `Seurat` objects precomputed dimensionality
#' reductions can be used for x or y aesthetics. To do so `red_dim` must be set
#' to the name of a dimensionality reduction in
#' [SingleCellExperiment::reducedDimNames()] (for a `SingleCellExperiment`) or
#' `object@dr` (for a `Seurat` object). `x_value` and `y_value` can then be set
#' to `red_dimX` when `red_dim` matches the `red_dim` argument and `X` is the
#' column of the dimensionality reduction to use.
#'
#' @return a `ggplot` object
#'
#' @examples
#' data(iris_clusts)
#' clustree(iris_clusts, prefix = "K")
#'
#' @export
clustree_overlay <- function (x, ...) {
    UseMethod("clustree_overlay", x)
}


#' @importFrom ggplot2 arrow aes_ guides
#' @importFrom grid unit
#' @importFrom dplyr %>%
#'
#' @rdname clustree_overlay
#' @export
clustree_overlay.matrix <- function(x, prefix, metadata, x_value, y_value,
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
                                    edge_width       = 1,
                                    use_colour       = c("edges", "points"),
                                    alt_colour       = "black",
                                    point_size       = 3,
                                    point_alpha      = 0.2,
                                    point_shape      = 18,
                                    label_nodes      = FALSE,
                                    label_size       = 3,
                                    ...) {

    checkmate::assert_matrix(x, mode = "numeric", any.missing = FALSE,
                             col.names = "unique", min.cols = 2)
    checkmate::assert_character(prefix, any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(metadata, nrows = nrow(x),
                                 col.names = "unique")
    checkmate::assert_character(x_value, any.missing = FALSE, len = 1)
    checkmate::assert_character(y_value, any.missing = FALSE, len = 1)
    checkmate::assert_character(suffix, any.missing = FALSE, len = 1,
                                null.ok = TRUE)
    checkmate::assert_number(count_filter, lower = 0, upper = nrow(x))
    checkmate::assert_number(prop_filter, lower = 0, upper = 1)
    assert_colour_node_aes("node_colour", prefix, metadata, node_colour,
                           node_colour_aggr)
    assert_numeric_node_aes("node_size", prefix, metadata, node_size,
                            node_size_aggr, 0, Inf)
    assert_numeric_node_aes("node_alpha", prefix, metadata, node_alpha,
                            node_alpha_aggr, 0, 1)
    checkmate::assert_number(edge_width, lower = 0)
    use_colour <- match.arg(use_colour)
    tryCatch(col2rgb(alt_colour),
             error = function(e) {
                 stop("alt_colour is set to '", node_aes, "' ",
                      "which is not a valid colour name.", call. = FALSE)
            }
    )
    checkmate::assert_number(point_size, finite = TRUE)
    checkmate::assert_number(point_alpha, lower = 0, upper = 1)
    checkmate::assert_number(point_shape, lower = 0, upper = 25)
    checkmate::assert_flag(label_nodes)
    checkmate::assert_number(label_size, lower = 0, finite = TRUE)

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

    node_aes_list <- list(
        x_value = list(value = x_value, aggr = "mean"),
        y_value = list(value = y_value, aggr = "mean"),
        colour = list(value = node_colour, aggr = node_colour_aggr),
        size = list(value = node_size, aggr = node_size_aggr),
        alpha = list(value = node_alpha, aggr = node_alpha_aggr)
    )

    x_val <- paste0("mean_", x_value)
    y_val <- paste0("mean_", y_value)
    hi_res <- colnames(x)[ncol(x)]

    points <- metadata[, c(x_value, y_value)]
    points$cluster <- factor(x[, hi_res])
    colnames(points) <- c(x_value, y_value, paste0(hi_res, "_cluster"))

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter,
                              metadata, node_aes_list)

    graph_attr <- igraph::graph_attr(graph)

    nodes <- graph %>%
        tidygraph::activate("nodes") %>%
        data.frame() %>%
        dplyr::mutate(!!as.name(prefix) := factor(!!as.name(prefix)),
                      cluster = factor(cluster))

    edges <- graph %>%
        tidygraph::activate("edges") %>%
        tidygraph::mutate(from_x = tidygraph::.N()[[x_val]][from],
                          from_y = tidygraph::.N()[[y_val]][from],
                          to_x = tidygraph::.N()[[x_val]][to],
                          to_y = tidygraph::.N()[[y_val]][to]) %>%
        data.frame() %>%
        dplyr::mutate_at(1:6, factor)

    if (use_colour == "points") {
        gg <- ggplot(points, aes_(x = as.name(x_value), y = as.name(y_value))) +
            geom_point(aes_(colour = as.name(paste0(hi_res, "_cluster"))),
                       size = point_size, alpha = point_alpha,
                       shape = point_shape)
    } else {
        gg <- ggplot(points, aes_(x = as.name(x_value), y = as.name(y_value))) +
            geom_point(colour = alt_colour, size = point_size,
                       alpha = point_alpha, shape = point_shape)
    }

    # Plot tree in layers from the bottom up
    for (res in rev(sort(unique(nodes[[prefix]])))) {
        nodes_res <- dplyr::filter(nodes, !!as.name(prefix) == res)
        edges_res <- dplyr::filter(edges,
                                   !!as.name(paste0("to_", prefix)) == res)

        gg <- gg +
            overlay_node_points(nodes_res, graph_attr$node_x_value,
                                graph_attr$node_y_value, graph_attr$node_colour,
                                graph_attr$node_size, graph_attr$node_alpha)

        if (use_colour == "edges") {
            gg <- gg +
                geom_segment(data = edges_res,
                             aes_(x = ~ from_x, y = ~ from_y,
                                  xend = ~ to_x, yend = ~ to_y,
                                  alpha = ~ in_prop,
                                  colour = as.name(paste0("from_", prefix))),
                             arrow = arrow(length = unit(0.02, "npc")),
                             size = edge_width)
        } else {
            gg <- gg +
                geom_segment(data = edges_res,
                             aes(x = from_x, y = from_y,
                                 xend = to_x, yend = to_y,
                                 alpha = in_prop),
                             arrow = arrow(length = unit(edge_width * 5,
                                                         "points")),
                             size = edge_width,
                             colour = alt_colour)
        }
    }

    if (label_nodes) {
        gg <- gg +
            ggrepel::geom_label_repel(data = nodes,
                                      aes_(x = as.name(x_val),
                                           y = as.name(y_val),
                                           label = ~ node),
                                      size = label_size)
    }

    gg <- gg +
        scale_size(range = c(node_size_range[1], node_size_range[2])) +
        cowplot::theme_cowplot()

    return(gg)
}


#' @rdname clustree_overlay
#' @export
clustree_overlay.data.frame <- function(x, prefix, ...) {

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
        stop("No metadata columns found. Additional columns must be supplied ",
             "containing x and y dimensions.", call. = FALSE)
    }

    clustree_overlay(clusterings, prefix, metadata = metadata, ...)
}


#' @rdname clustree_overlay
#' @export
clustree_overlay.SingleCellExperiment <- function(x, prefix, x_value, y_value,
                                                  exprs = "counts",
                                                  red_dim = NULL,
                                                  ...) {

    if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
        stop("The SingleCellExperiment package is missing, this must be",
             "installed for clustree to use SingleCellExperiment objects")
    }

    checkmate::assert_class(x, "SingleCellExperiment")
    checkmate::assert_character(exprs, any.missing = FALSE, len = 1)
    checkmate::assert_character(red_dim, len = 1, null.ok = TRUE)

    if (!(exprs %in% names(x@assays))) {
        stop("exprs must be the name of an assay in x: ",
             paste0(names(x@assays), collapse = ", "))
    } else {
        exprs_mat <- SummarizedExperiment::assay(sim_sc3, exprs)
    }

    if (!is.null(red_dim)) {
        if (!(red_dim %in% names(x@reducedDims))) {
            stop("red_dim must be the name of a reducedDim in x: ",
                 paste0(names(x@reducedDims), collapse = ", "))
        }
    }

    if (!is.null(red_dim)) {
        if (grepl(red_dim, x_value)) {
            idx <- as.numeric(gsub(red_dim, "", x_value))
            x@colData[x_value] <- x@reducedDims[[red_dim]][, idx]
        }
    }

    if (!is.null(red_dim)) {
        if (grepl(red_dim, y_value)) {
            idx <- as.numeric(gsub(red_dim, "", y_value))
            x@colData[y_value] <- x@reducedDims[[red_dim]][, idx]
        }
    }

    args <- list(...)
    args$x_value <- x_value
    args$y_value <- y_value

    for (node_aes in c("x_value", "y_value", "node_colour", "node_size",
                       "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% rownames(x)) {
                aes_name <- paste0(exprs, "_", node_aes_value)
                x@colData[aes_name] <- exprs_mat[node_aes_value, ]
                args[[node_aes]] <- aes_name
                if (node_aes == "x_value") {
                    x_value <- paste0(exprs, "_", x_value)
                }
                if (node_aes == "y_value") {
                    y_value <- paste0(exprs, "_", y_value)
                }
            }
        }
    }

    if (!(x_value %in% colnames(x@colData)) |
        !(y_value %in% colnames(x@colData))) {
        stop("No data identified for x_value or y_value. Check that red_dim ",
             "is set correctly.", call. = FALSE)
    }

    args$x <- data.frame(x@colData)
    args$prefix <- prefix

    do.call(clustree_overlay, args)
}


#' @rdname clustree_overlay
#'
#' @importFrom methods slot
#' @export
clustree_overlay.seurat <- function(x, x_value, y_value, prefix = "res.",
                                    exprs = c("data", "raw.data", "scale.data"),
                                    red_dim = NULL, ...) {

    if (!requireNamespace("Seurat", quietly = TRUE)) {
        stop("The Seurat package is missing, this must be installed for ",
             "clustree to use Seurat objects")
    }

    checkmate::assert_class(x, "seurat")
    checkmate::assert_character(exprs, any.missing = FALSE)
    checkmate::assert_character(red_dim, len = 1, null.ok = TRUE)

    exprs <- match.arg(exprs)

    if (!is.null(red_dim)) {
        if (!(red_dim %in% names(x@dr))) {
            stop("red_dim must be the name of a dr in x: ",
                 paste0(names(x@dr), collapse = ", "))
        }
    }

    if (!is.null(red_dim)) {
        if (grepl(red_dim, x_value)) {
            idx <- as.numeric(gsub(red_dim, "", x_value))
            x@meta.data[x_value] <- x@dr[[red_dim]]@cell.embeddings[, idx]
        }
    }

    if (!is.null(red_dim)) {
        if (grepl(red_dim, y_value)) {
            idx <- as.numeric(gsub(red_dim, "", y_value))
            x@meta.data[y_value] <- x@dr[[red_dim]]@cell.embeddings[, idx]
        }
    }

    args <- list(...)
    args$x_value <- x_value
    args$y_value <- y_value

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

    if (!(x_value %in% colnames(x@meta.data)) |
        !(y_value %in% colnames(x@meta.data))) {
        stop("No data identified for x_value or y_value. Check that red_dim ",
             "is set correctly.", call. = FALSE)
    }

    args$x <- x@meta.data
    args$prefix <- prefix

    do.call(clustree_overlay, args)

}


#' Overlay node points
#'
#' Overlay clustering tree nodes on a scatter plot with the specified
#' aesthetics.
#'
#' @param nodes data.frame describing nodes
#' @param x_value column of nodes to use for the x position
#' @param y_value column of nodes to use for the y position
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#'
#' @importFrom ggplot2 aes_ geom_point
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