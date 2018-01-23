#' @importFrom dplyr %>%
build_tree_graph <- function(clusterings, prefix, count_filter, prop_filter) {

    nodes <- get_tree_nodes(clusterings, prefix)

    edges <- get_tree_edges(clusterings, prefix) %>%
        dplyr::filter(count > count_filter) %>%
        dplyr::filter(proportion > prop_filter)

    graph <- igraph::graph_from_data_frame(edges, vertices = nodes)

    return(graph)
}

get_tree_nodes <- function(clusterings, prefix) {

    nodes <- lapply(colnames(clusterings), function(res) {
        clustering <- clusterings[, res]
        clusters <- sort(unique(clustering))

        node <- lapply(clusters, function(cluster) {
            is_cluster <- clustering == cluster
            size <- sum(is_cluster)

            res_clean <- as.numeric(gsub(prefix, "", res))
            node_name <- paste0(prefix, res_clean, "C", cluster)

            node_data <- list(node_name, res_clean, cluster, size)
            names(node_data) <- c("node", prefix, "cluster", "size")

            node_data <- data.frame(node_data, stringsAsFactors = FALSE)

            return(node_data)
        })

        node <- do.call("rbind", node)

    })

    nodes <- do.call("rbind", nodes)
    nodes[, prefix] <- factor(nodes[, prefix])

    return(nodes)
}

#' @importFrom dplyr %>%
get_tree_edges <- function(clusterings, prefix) {

    res_values <- colnames(clusterings)

    edges <- lapply(seq_len(ncol(clusterings) - 1), function(idx) {
        from_res <- res_values[idx]
        to_res <- res_values[idx + 1]

        from_clusters <- sort(unique(clusterings[, from_res]))
        to_clusters <- sort(unique(clusterings[, to_res]))

        from_tos <- expand.grid(from_clust = from_clusters,
                                to_clust = to_clusters,
                                stringsAsFactors = FALSE)

        transitions <- apply(from_tos, 1, function(from_to) {
            from_clust <- from_to[1]
            to_clust <- from_to[2]

            is_from <- clusterings[, from_res] == from_clust
            is_to <- clusterings[, to_res] == to_clust

            trans_count <- sum(is_from & is_to)

            to_size <- sum(is_to)

            trans_prop <- trans_count / to_size

            return(c(trans_count, trans_prop))
        })

        from_tos$from_res <- as.numeric(gsub(prefix, "", from_res))
        from_tos$to_res <- as.numeric(gsub(prefix, "", to_res))
        from_tos$count <- transitions[1, ]
        from_tos$proportion <- transitions[2, ]

        return(from_tos)
    })

    edges <- dplyr::bind_rows(edges) %>%
        dplyr::mutate(from_node = paste0(prefix, from_res, "C", from_clust)) %>%
        dplyr::mutate(to_node = paste0(prefix, to_res, "C", to_clust)) %>%
        dplyr::select(from_node, to_node, dplyr::everything())

    return(edges)

}
