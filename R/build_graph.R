#' Build clustering tree graph
#'
#' Construct a clustering tree graph from various objects
#'
#' @param x Object containing clustering data
#' @param metadata data.frame containing metadata about individual samples
#' @param pattern A regular expression matching column names for columns
#' containing clustering data. Should have a single capture group.
#' @param prefix Prefix string used to select columns containing clustering
#' information. Combined with `suffix` to create a regular expression.
#' @param suffix Suffix string used to select columns containing clustering
#' information. Combined with `prefix` to create a regular expression.
#' @param clust_cols Vector of column names used to select columns containing
#' clustering information.
#' @param ... Arguments used by other methods
#'
#' @details
#' **Column selection**
#' A key part of building a clustering tree graph is selecting columns in an
#' object which contain clustering information. This can be done in a variety
#' of ways. The primary method is to specify the `pattern` argument. This is a
#' regular expression containing a single capture group which is applied to
#' column names. For example `pattern = "K(.*)"` would match columns starting
#' with "K" and capture the rest of the column name. Setting
#' `pattern = "^Res_(.*)_cluster"` would match columns starting with "Res_" and
#' ending with "_cluster" and extract the part in between.
#'
#' A simpler alternative to setting `pattern` is to use the `prefix` and
#' `suffix` arguments. These are pasted together to create a pattern so that
#' `prefix = "Res", suffix = "Cluster"` would be equivalent to
#' `pattern = "Res(.*)Cluster`.
#'
#' Exact column names can also be supplied using the `clust_cols` argument.
#' For example `clust_cols = c("K1", "K3", "K5")` would select only those
#' columns. If `clust_cols` is named those names will be used for each
#' resolution level.
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
build_clustree_graph.data.frame <- function(x,
                                            pattern = NULL,
                                            prefix = NULL,
                                            suffix = NULL,
                                            clust_cols = NULL,
                                            ...) {

    abort_data_frame(x, col.names = "unique")

    clust_cols <- select_clust_cols(
        colnames(x),
        pattern    = pattern,
        prefix     = prefix,
        suffix     = suffix,
        clust_cols = clust_cols
    )
    clusterings <- extract_clusterings(x, clust_cols)
    metadata <- extract_metadata(x, clust_cols)

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
            names(node_data) <- c("node", "res", "cluster", ".clustree_indices")

            node_data <- tibble::as_tibble(node_data)

            return(node_data)
        })

        node <- do.call("rbind", node)
    })

    nodes <- do.call("rbind", nodes)
    nodes$res <- factor(nodes$res, levels = colnames(clusterings))

    return(nodes)
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

    graph <- tidygraph::activate(graph, "nodes")
    graph <- summarise_metadata(graph, size = length(.data$.clustree_indices))

    node_sizes <- dplyr::pull(graph, "size")

    graph <- tidygraph::activate(graph, "edges")
    graph <- summarise_metadata(graph, count = length(.data$.clustree_indices))
    graph <- dplyr::mutate(graph, in_prop = .data$count / node_sizes[.data$to])

    # Extract the edges and use dplyr instead of using tidygraph directly to
    # avoid this warning https://github.com/thomasp85/tidygraph/issues/131
    edges <- as.data.frame(graph)
    edges <- dplyr::group_by(edges, .data$to)
    edges <- dplyr::mutate(
        edges,
        is_core = .data$in_prop == max(.data$in_prop, na.rm = TRUE)
    )
    graph <- tidygraph::mutate(graph, is_core = edges$is_core)

    tidygraph::ungroup(graph)
}

select_clust_cols <- function(col_names, pattern = NULL, prefix = NULL,
                              suffix = NULL, clust_cols = NULL) {

    abort_character(pattern, any.missing = FALSE, len = 1, null.ok = TRUE)
    abort_character(prefix, any.missing = FALSE, len = 1, null.ok = TRUE)
    abort_character(suffix, any.missing = FALSE, len = 1, null.ok = TRUE)
    abort_character(clust_cols, any.missing = FALSE, min.len = 2,
                    null.ok = TRUE)

    selection_option <- c("pattern", "prefix", "names")[c(
        !is.null(pattern),
        !is.null(prefix) || !is.null(suffix),
        !is.null(clust_cols)
    )]
    if (length(selection_option) != 1) {
        abort(
            "Specify one of pattern, prefix (and/or suffix) or clust_cols"
        )
    }

    clust_cols <- switch (selection_option,
        pattern   = col_names[grep(pattern, col_names)],
        prefix    = {
            pattern <- paste0(prefix, "(.*)", suffix)
            inform(paste0(
                "Created pattern '", pattern, "' from prefix '", prefix,
                "' and suffix '", suffix, "'"
            ))
            col_names[grep(pattern, col_names)]
        },
        names = {
            matched_cols <- clust_cols %in% col_names
            if (!all(matched_cols)) {
                warn(paste0(
                    "Some supplied column names did not match columns in x: ",
                    paste(clust_cols[!matched_cols], collapse = ", ")
                ))
            }
            clust_cols[matched_cols]
        }
    )

    if (length(clust_cols) < 2) {
        msg <- switch (selection_option,
            pattern = paste0(
               "Less than two columns matched the pattern '", pattern, "'"
            ),
            prefix = paste0(
               "Less than two columns matched the pattern '", pattern, "'",
               "from prefix '", prefix, "' and suffix '", suffix, "'"
            ),
            names = paste0(
               "Less than two supplied column names matched columns in x"
            )
        )
        abort(message = msg)
    }

    if (!is.null(pattern)) {
        names(clust_cols) <- gsub(pattern, "\\1", clust_cols)
    }

    return(clust_cols)
}

extract_clusterings <- function(x, clust_cols) {

    clusterings <- as.matrix(x[, clust_cols])
    if (!is.null(names(clust_cols))) {
        colnames(clusterings) <- names(clust_cols)
    }
    if (is.null(rownames(clusterings))) {
        rownames(clusterings) <- seq_len(nrow(clusterings))
    }

    return(clusterings)
}

extract_metadata <- function(x, clust_cols) {

    is_not_clust_col <- !(colnames(x) %in% clust_cols)

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

# order_clusterings <- function(clusterings, pattern) {
#
#     res_values <- gsub(pattern, "\\1", colnames(clusterings))
#
#     is_num <- suppressWarnings(!any(is.na(as.numeric(res_values))))
#     if (!is_num) {
#         abort(
#             paste0(
#                 "The extracted resolution values could not be converted to a",
#                 "number. Please check that your column pattern is correct",
#                 "(pattern = '", pattern,
#                 "', colnames = c('",
#                 paste(colnames(clusterings), collapse = "', '"), "'))"
#             )
#         )
#     }
#
#     res_order <- order(as.numeric(res_values))
#
#     clusterings[, res_order]
# }
