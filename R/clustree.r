clustree <- function (x, ...) {
    UseMethod("clustree", x)
}

clustree.data.frame <- function(x, prefix, count_filter = 0, prop_filter = 0.1,
                                node_colour = prefix, node_colour_aggr = NULL,
                                node_size = "size", node_size_aggr = NULL,
                                node_alpha = 1, node_alpha_aggr = NULL) {

    checkmate::assert_character(prefix, len = 1)

    clust_cols <- grepl(prefix, colnames(x))

    clusterings <- as.matrix(x[, clust_cols])

    if (sum(!clust_cols) > 0) {
        metadata <- x[, !clust_cols]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, prefix, count_filter, prop_filter, metadata,
             node_colour, node_colour_aggr, node_size, node_size_aggr,
             node_alpha, node_alpha_aggr)
}

#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn
#' @importFrom ggplot2 arrow aes aes_ guides guide_legend
#' @importFrom grid unit
clustree.matrix <- function(x, prefix, count_filter = 0, prop_filter = 0.1,
                            metadata = NULL, node_colour = prefix,
                            node_colour_aggr = NULL, node_size = "size",
                            node_size_aggr = NULL, node_alpha = 1,
                            node_alpha_aggr = NULL) {


    #assert_node_aes("node_colour", prefix, metadata, node_colour,
    #                node_colour_aggr)
    assert_numeric_node_aes("node_size", prefix, metadata, node_size,
                            node_size_aggr, 0, Inf)
    assert_numeric_node_aes("node_alpha", prefix, metadata, node_alpha,
                            node_alpha_aggr, 0, 1)

    res_clean <- gsub(prefix, "", colnames(x))
    x <- x[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter, metadata,
                              node_colour, node_colour_aggr, node_size,
                              node_size_aggr, node_alpha, node_alpha_aggr)

    gg <- ggraph(graph, layout = "tree") +
        # Plot edges
        geom_edge_link(arrow = arrow(length = unit(1, "mm")),
                       end_cap = circle(3.5, "mm"),
                       edge_width = 1,
                       aes(colour = count, alpha = proportion)) +
        scale_edge_colour_gradientn(colours = viridis::viridis(256)) +
        #guides(colour = guide_legend(title = prefix,
        #                             title.position = "top")) +
        # Plot nodes
        #geom_node_point(aes_(colour = as.name(node_colour),
        #                     size = as.name(node_size))) +
        add_node_points(node_colour, node_size, node_alpha, metadata) +
        geom_node_text(aes(label = cluster), size = 3)

    return(gg)
}

#' @importFrom ggraph geom_node_point
#' @importFrom ggplot2 aes_
add_node_points <- function(node_colour, node_size, node_alpha, metadata) {

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
