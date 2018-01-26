clustree <- function (x, ...) {
    UseMethod("clustree", x)
}

clustree.SingleCellExperiment <- function(x, prefix, exprs = "counts", ...) {

    checkmate::assert_character(prefix, len = 1)

    if (!(exprs %in% SummarizedExperiment::assayNames(x))) {
        stop("exprs must be the name of an assay in x: ",
             paste0(SummarizedExperiment::assayNames(x), collapse = ", "))
    }

    args <- list(...)
    for (node_aes in c("node_colour", "node_size", "node_alpha")) {
        if (node_aes %in% names(args)) {
            node_aes_value <- args[[node_aes]]
            if (node_aes_value %in% rownames(x)) {
                SummarizedExperiment::colData(x)[node_aes_value] <-
                    SummarizedExperiment::assay(x, exprs)[node_aes_value, ]
            }
        }
    }

    clustree(data.frame(SummarizedExperiment::colData(x)), prefix, ...)

}

clustree.data.frame <- function(x, prefix, ...) {

    checkmate::assert_character(prefix, len = 1)

    clust_cols <- grepl(prefix, colnames(x))

    clusterings <- as.matrix(x[, clust_cols])
    mode(clusterings) <- "numeric"

    if (sum(!clust_cols) > 0) {
        metadata <- x[, !clust_cols]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, prefix, metadata = metadata, ...)
}

#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn
#' @importFrom ggplot2 arrow aes aes_ guides guide_legend
#' @importFrom grid unit
clustree.matrix <- function(x, prefix, suffix = NULL,
                            count_filter = 0, prop_filter = 0.1,
                            metadata = NULL, node_colour = prefix,
                            node_colour_aggr = NULL, node_size = "size",
                            node_size_aggr = NULL, node_size_range = c(4, 15),
                            node_alpha = 1, node_alpha_aggr = NULL,
                            node_text_size = 3, scale_node_text = FALSE,
                            edge_width = 1.5, edge_arrow = TRUE) {


    #assert_node_aes("node_colour", prefix, metadata, node_colour,
    #                node_colour_aggr)
    assert_numeric_node_aes("node_size", prefix, metadata, node_size,
                            node_size_aggr, 0, Inf)
    assert_numeric_node_aes("node_alpha", prefix, metadata, node_alpha,
                            node_alpha_aggr, 0, 1)

    if (!is.null(suffix)) {
        colnames(x) <- gsub(suffix, "", colnames(x))
    }

    res_clean <- gsub(prefix, "", colnames(x))
    is_num <- suppressWarnings(!any(is.na(as.numeric(res_clean))))
    if (!is_num) {
        stop("The X portion of your clustering column names could not be ",
             "converted to a number. Please check that your prefix and suffix ",
             "are correct: prefix = '", prefix, "' suffix = '", suffix, "'")
    }

    x <- x[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter, metadata,
                              node_colour, node_colour_aggr, node_size,
                              node_size_aggr, node_alpha, node_alpha_aggr)

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

    gg <- gg + scale_edge_colour_gradientn(colours = viridis::viridis(256)) +
        # Plot nodes
        add_node_points(prefix, node_colour, node_size, node_alpha, metadata)

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
