#' Build clustering tree graph
#'
#' Construct a clustering tree graph from various objects
#'
#' @param x Object containing clustering data
#' @param metadata data.frame containing metadata about individual samples
#' @param pattern A regular expression matching column names for columns
#' containing clustering data. Should have a single capture group.
#' @param ... Arguments used by other methods
#'
#' @return A `clustree_graph` object
#' @export
#'
#' @seealso [clustree_graph()] for more details on the `clustree_graph` object
#'
#' @examples
#' build_clustree_graph(nba_clusts, pattern = "K(.*)")
build_clustree_graph <- function (x, ...) {
    UseMethod("build_clustree_graph", x)
}

#' @describeIn build_clustree_graph Default method. Computes nodes and edges and
#' constructs the graph.
#' @export
build_clustree_graph.default <- function(x, metadata = NULL, ...) {

    abort_matrix(x, any.missing = FALSE, col.names = "unique", min.cols = 2)
    abort_data_frame(metadata, nrows = nrow(x), col.names = "unique",
                     null.ok = TRUE)

    if (is.null(rownames(x))) {
        rownames(x) <- seq_len(nrow(x))
    }

    if (!is.null(metadata)) {
        metadata[, ".clustree_idx"] <- rownames(x)
    }

    nodes <- get_tree_nodes(x)
    edges <- get_tree_edges(x)

    graph <- clustree_graph(nodes = nodes, edges = edges, metadata = metadata)
    graph <- add_default_stats(graph)

    tidygraph::activate(graph, "nodes")
}

#' @describeIn build_clustree_graph Method for `matrix` objects. Calls default
#' method.
#' @export
build_clustree_graph.matrix <- function(x, metadata = NULL, ...) {
    build_clustree_graph.default(x, metadata)
}

#' @describeIn build_clustree_graph Method for `data.frame` objects. Extracts
#' clusterings and metadata and passes them to other methods.
#' @export
build_clustree_graph.data.frame <- function(x, pattern = "(.*)", ...) {

    abort_data_frame(x, col.names = "unique")
    abort_character(pattern, any.missing = FALSE, len = 1)

    clusterings <- extract_clusterings(x, pattern)
    metadata <- extract_metadata(x, pattern)

    build_clustree_graph.matrix(clusterings, metadata)
}

get_tree_nodes <- function(clusterings) {

    nodes <- lapply(colnames(clusterings), function(.res) {
        clustering <- clusterings[, .res]
        clusters <- sort(unique(clustering))

        node <- lapply(clusters, function(cluster) {

            node_name <- paste0("Res", .res, "C", cluster)

            node_data <- list(
                node_name,
                .res,
                cluster,
                list(rownames(clusterings)[which(clustering == cluster)])
            )
            names(node_data) <- c("node", "res", "cluster",
                                  ".clustree_indices")

            node_data <- tibble::as_tibble(node_data)

            return(node_data)
        })

        node <- do.call("rbind", node)
    })

    do.call("rbind", nodes)
}

get_tree_edges <- function(clusterings) {

    res_values <- colnames(clusterings)

    edges <- lapply(seq_len(ncol(clusterings)) - 1, function(.idx) {
        from_res <- res_values[.idx]
        to_res   <- res_values[.idx + 1]

        from_clustering <- clusterings[, from_res]
        to_clustering   <- clusterings[, to_res]

        from_clusters <- sort(unique(from_clustering))
        to_clusters   <- sort(unique(to_clustering))

        res_edges <- expand.grid(
            from_res         = from_res,
            from_cluster     = from_clusters,
            to_res           = to_res,
            to_cluster       = to_clusters,
            stringsAsFactors = FALSE
        )

        res_indices <- lapply(seq_len(nrow(res_edges)), function(.edge_idx) {
            matches <- which(
                from_clustering == res_edges$from_cluster[.edge_idx] &
                    to_clustering == res_edges$to_cluster[.edge_idx]
            )
            rownames(clusterings)[matches]
        })

        tibble::tibble(res_edges, .clustree_indices = res_indices)
    })

    edges <- do.call("rbind", edges)

    edges$from_node <- paste0("Res", edges$from_res, "C", edges$from_cluster)
    edges$to_node   <- paste0("Res", edges$to_res,   "C", edges$to_cluster)

    edges[, c("from_node", "to_node", "from_res", "to_res", "from_cluster",
              "to_cluster", ".clustree_indices")]
}

add_default_stats <- function(graph) {

    graph <- tidygraph::activate(graph, "edges")
    graph <- summarise_metadata(graph, count = length(.data$.clustree_indices))

    graph <- tidygraph::activate(graph, "nodes")
    graph <- summarise_metadata(graph, size = length(.data$.clustree_indices))

    node_sizes <- dplyr::pull(graph, "size")

    graph <- tidygraph::activate(graph, "edges")
    graph <- dplyr::mutate(graph, in_prop = .data$count / node_sizes[.data$to])
}

extract_clusterings <- function(x, pattern) {

    clust_cols <- grep(pattern, colnames(x))

    if (length(clust_cols) < 2) {
        abort(
            paste0("Less than two columns matched the pattern '", pattern, "'")
        )
    }

    clusterings <- as.matrix(x[, clust_cols])
    colnames(clusterings) <- gsub(pattern, "\\1", colnames(clusterings))

    return(clusterings)
}

extract_metadata <- function(x, pattern) {

    is_not_clust_col <- !grepl(pattern, colnames(x))

    if (!any(is_not_clust_col)) {
        return(NULL)
    }

    metadata <- x[, is_not_clust_col, drop = FALSE]

    metadata_names <- make.names(colnames(metadata))
    metadata_diff <- metadata_names != colnames(metadata)
    if (any(metadata_diff)) {
        warn(
            paste0(
                "The following metadata column names will be converted:\n",
                paste(
                    paste(
                        colnames(metadata)[metadata_diff],
                        metadata_names[metadata_diff],
                        sep = " -> "
                    ),
                    collapse = "\n"
                )
            )
        )
        colnames(metadata) <- metadata_names
    }

    tibble::as_tibble(metadata)
}

order_clusterings <- function(clusterings, pattern) {

    res_values <- gsub(pattern, "\\1", colnames(clusterings))

    is_num <- suppressWarnings(!any(is.na(as.numeric(res_values))))
    if (!is_num) {
        abort(
            paste0(
                "The extracted resolution values could not be converted to a",
                "number. Please check that your column pattern is correct",
                "(pattern = '", pattern,
                "', colnames = c('",
                paste(colnames(clusterings), collapse = "', '"), "'))"
            )
        )
    }

    res_order <- order(as.numeric(res_values))

    clusterings[, res_order]
}
