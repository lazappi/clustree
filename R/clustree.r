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

clustree.matrix <- function(x, prefix, count_filter = 0, prop_filter = 0.1) {

    res_clean <- gsub(prefix, "", colnames(x))
    x <- x[, order(as.numeric(res_clean))]

    graph <- build_tree_graph(x, prefix, count_filter, prop_filter)

    return(graph)
}
