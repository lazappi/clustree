#' Plot a clustering tree
#'
#' Creates a plot of a clustering tree showing the relationship between
#' clusterings at different resolutions.
#'
#' @param x object containing clustering data
#' @param df data.frame containing clustering information
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param sce [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment]
#' object with clustering information held in the `colData` slot
#' @param seurat [Seurat][seurat] object with clustering information held in
#' the `meta.data` slot
#' @param prefix string indicating columns containing clustering information
#' @param suffix string at the end of column names containing clustering
#' information
#' @param count_filter count threshold for filtering edges in the clustering
#' graph
#' @param prop_filter proportion threshold for filtering edges in the clustering
#' graph
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_colour_aggr if `node_colour` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_size_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_size_range numeric vector of length two giving the maximum and
#' minimum point size for plotting nodes
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param node_alpha_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_text_size numeric value giving the size of node labels if
#' `scale_node_text` is `FALSE`
#' @param scale_node_text logical indicating whether to scale node labels along
#' with the node size
#' @param edge_width numeric value giving the width of plotted edges
#' @param edge_arrow logical indicating whether to add an arrow to edges
#' @param exprs source of gene expression information to use as node aesthetics,
#' for SingleCellExperiment objects it must be a name in
#' [SummarizedExperiment::assayNames()], for a Seurat object it must be one of
#' `raw.data` or `scale.data`
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
#' the naming structure `PXS` where `P`` is a prefix indicating that the column
#' contains clustering information, `X` is a numeric value indicating the
#' clustering resolution and `S` is any additional suffix to be removed. For
#' all objects except matrices any additional columns can be used as aesthetics,
#' for matrices an additional metadata data.frame can be supplied if required.
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
#' @examples
#' data(iris_clusts)
#' clustree(iris_clusts, prefix = "K")
#'
#' @export
clustree <- function (x, ...) {
    UseMethod("clustree", x)
}

#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn
#' @importFrom ggplot2 arrow aes aes_ guides guide_legend
#' @importFrom grid unit
#'
#' @rdname clustree
#' @export
clustree.matrix <- function(clusterings, prefix, suffix = NULL,
                            count_filter = 0, prop_filter = 0.1,
                            metadata = NULL, node_colour = prefix,
                            node_colour_aggr = NULL, node_size = "size",
                            node_size_aggr = NULL, node_size_range = c(4, 15),
                            node_alpha = 1, node_alpha_aggr = NULL,
                            node_text_size = 3, scale_node_text = FALSE,
                            edge_width = 1.5, edge_arrow = TRUE) {

    assert_numeric_node_aes("node_size", prefix, metadata, node_size,
                            node_size_aggr, 0, Inf)
    assert_numeric_node_aes("node_alpha", prefix, metadata, node_alpha,
                            node_alpha_aggr, 0, 1)

    if (!is.null(suffix)) {
        colnames(clusterings) <- gsub(suffix, "", colnames(clusterings))
    }

    res_clean <- gsub(prefix, "", colnames(clusterings))
    is_num <- suppressWarnings(!any(is.na(as.numeric(res_clean))))
    if (!is_num) {
        stop("The X portion of your clustering column names could not be ",
             "converted to a number. Please check that your prefix and suffix ",
             "are correct: prefix = '", prefix, "' suffix = '", suffix, "'")
    }

    clusterings <- clusterings[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(clusterings, prefix, count_filter, prop_filter,
                              metadata, node_colour, node_colour_aggr,
                              node_size, node_size_aggr, node_alpha,
                              node_alpha_aggr)

    gg <- ggraph(graph, layout = "tree")

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
                                  aes(colour = count, alpha = proportion))
    } else {
        gg <- gg + geom_edge_link(edge_width = edge_width,
                                 aes(colour = count, alpha = proportion))
    }

    gg <- gg + scale_edge_colour_gradientn(colours = viridis::viridis(256))

    # Plot nodes
    gg <- gg + add_node_points(prefix, node_colour, node_size, node_alpha,
                               metadata)

    if (scale_node_text && !is.numeric(node_size)) {
        gg <- gg + geom_node_text(aes_(label = ~ cluster,
                                       size = as.name(node_size)))
    } else {
        gg <- gg + geom_node_text(aes(label = cluster), size = node_text_size)
    }
    gg <- gg + scale_size(range = node_size_range) +
        theme_clustree()

    return(gg)
}

#' @rdname clustree
#' @export
clustree.data.frame <- function(df, prefix, ...) {

    checkmate::assert_character(prefix, len = 1)

    clust_cols <- grepl(prefix, colnames(df))

    clusterings <- as.matrix(df[, clust_cols])
    mode(clusterings) <- "numeric"

    if (sum(!clust_cols) > 0) {
        metadata <- df[, !clust_cols]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, prefix, metadata = metadata, ...)
}

#' @rdname clustree
#' @export
clustree.SingleCellExperiment <- function(sce, prefix, exprs = "counts", ...) {

    if (!(exprs %in% names(sce@assays))) {
        stop("exprs must be the name of an assay in sce: ",
             paste0(names(sce@assays), collapse = ", "))
    }

    args <- list(...)
    for (node_aes in c("node_colour", "node_size", "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% rownames(sce)) {
                sce@colData[node_aes_value] <-
                    sce@assays[[exprs]][node_aes_value, ]
            }
        }
    }

    clustree(data.frame(sce@colData), prefix, ...)

}

#' @rdname clustree
#' @export
clustree.seurat <- function(seurat, prefix = "res.",
                            exprs = c("raw.data", "scale.data"), ...) {

    exprs <- match.arg(exprs)

    args <- list(...)
    gene_names <- rownames(seurat@raw.data)
    for (node_aes in c("node_colour", "node_size", "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% gene_names) {
                seurat@meta.data[node_aes_value] <-
                    slot(seurat, exprs)[node_aes_value, ]
            }
        }
    }

    clustree(seurat@meta.data, prefix, ...)

}

#' @importFrom ggraph geom_node_point
#' @importFrom ggplot2 aes_
add_node_points <- function(prefix, node_colour, node_size, node_alpha,
                            metadata) {

    allowed <- c(prefix, "cluster", "size")
    if (!is.null(metadata)) {
        allowed <- c(allowed, colnames(metadata))
    }

    col_allowed <- node_colour %in% allowed
    size_allowed <- node_size %in% allowed
    alpha_allowed <- node_alpha %in% allowed

    is_allowed <- c(col_allowed, size_allowed, alpha_allowed)
    aes_allowed <- c("col", "size", "alpha")[is_allowed]
    aes_allowed <- paste(aes_allowed, collapse = "_")

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
                                            size = node_size)
    )

}

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
        checkmate::assert_function(node_aes_aggr,
                                   .var.name = paste0(node_aes_name, "_aggr"))
    }
}

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

theme_clustree <- function(base_size = 14, base_family = "") {

    # Modified from cowplot::theme_nothing
    theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(line                  = element_blank(),
          rect                  = element_blank(),
          text                  = element_text(family     = base_family,
                                               face       = "plain",
                                               colour     = "black",
                                               size       = base_size,
                                               lineheight = 0.9,
                                               hjust      = 0.5,
                                               vjust      = 0.5,
                                               angle      = 0,
                                               margin     = margin(),
                                               debug      = FALSE),
          axis.line             = element_blank(),
          axis.line.x           = NULL,
          axis.line.y           = NULL,
          axis.text             = element_blank(),
          axis.text.x           = element_blank(),
          axis.text.x.top       = element_blank(),
          axis.text.y           = element_blank(),
          axis.text.y.right     = element_blank(),
          axis.ticks            = element_blank(),
          axis.ticks.length     = unit(0, "pt"),
          axis.title.x          = element_blank(),
          axis.title.x.top      = element_blank(),
          axis.title.y          = element_blank(),
          axis.title.y.right    = element_blank(),
          legend.background     = element_blank(),
          legend.spacing        = unit(0.4, "cm"),
          legend.spacing.x      = NULL,
          legend.spacing.y      = NULL,
          legend.margin         = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          legend.key            = element_blank(),
          legend.key.size       = unit(1.2, "lines"),
          legend.key.height     = NULL,
          legend.key.width      = NULL,
          legend.text           = element_text(size = rel(0.8)),
          legend.text.align     = NULL,
          legend.title          = element_text(hjust = 0),
          legend.title.align    = NULL,
          legend.position       = "right",
          legend.direction      = NULL,
          legend.justification  = "center",
          legend.box            = NULL,
          legend.box.margin     = margin(0, 0, 0, 0, "cm"),
          legend.box.background = element_blank(),
          legend.box.spacing    = unit(0.4, "cm"),
          panel.background      = element_blank(),
          panel.border          = element_blank(),
          panel.grid.major      = element_blank(),
          panel.grid.minor      = element_blank(),
          panel.spacing         = unit(0, "pt"),
          panel.spacing.x       = NULL,
          panel.spacing.y       = NULL,
          panel.ontop           = FALSE,
          strip.background      = element_blank(),
          strip.text            = element_blank(),
          strip.text.x          = element_blank(),
          strip.text.y          = element_blank(),
          strip.placement       = "inside",
          strip.placement.x     = NULL,
          strip.placement.y     = NULL,
          strip.switch.pad.grid = unit(0, "cm"),
          strip.switch.pad.wrap = unit(0, "cm"),
          plot.background       = element_blank(),
          plot.title            = element_blank(),
          plot.subtitle         = element_blank(),
          plot.caption          = element_blank(),
          plot.margin           = margin(0, 0, 0, 0),
          complete              = TRUE)
}
