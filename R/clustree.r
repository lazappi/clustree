clustree <- function (x, ...) {
    UseMethod("clustree", x)
}

clustree.data.frame <- function(x, prefix, ...) {

    checkmate::assert_character(prefix, len = 1)

    clust_cols <- grepl(prefix, colnames(x))

    clusterings <- as.matrix(x[, clust_cols])

    if (sum(!clust_cols) > 0) {
        metadata <- x[, !clust_cols]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, prefix, ...)
}

#' @importFrom ggraph ggraph geom_edge_link circle geom_node_point
#' geom_node_text scale_edge_colour_gradientn
#' @importFrom ggplot2 arrow aes aes_ guides guide_legend
#' @importFrom grid unit
clustree.matrix <- function(x, prefix, count_filter = 0, prop_filter = 0.1) {

    res_clean <- gsub(prefix, "", colnames(x))
    x <- x[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter)

    gg <- ggraph(graph, layout = "tree") +
        # Plot edges
        geom_edge_link(arrow = arrow(length = unit(1, "mm")),
                       end_cap = circle(3.5, "mm"),
                       edge_width = 1,
                       aes(colour = log(count), alpha = proportion)) +
        scale_edge_colour_gradientn(colours = viridis::viridis(100)) +
        #guides(colour = guide_legend(title = prefix,
        #                             title.position = "top")) +
        # Plot nodes
        geom_node_point(aes_(colour = as.name(prefix),
                             size = ~size)) +
        geom_node_text(aes(label = cluster), size = 3)

    return(gg)
}

