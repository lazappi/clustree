#' Build tree graph
#'
#' Build a tree graph from a set of clusterings, metadata and associated
#' aesthetics
#'
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param prefix string indicating columns containing clustering information
#' @param count_filter count threshold for filtering edges in the clustering
#' graph
#' @param prop_filter in proportion threshold for filtering edges in the
#' clustering graph
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_colour_aggr if `node_colour` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_size_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param node_alpha_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#'
#' @return [igraph::igraph] object containing the tree graph
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
build_tree_graph <- function(clusterings, prefix, count_filter, prop_filter,
                             metadata, node_colour, node_colour_aggr,
                             node_size, node_size_aggr, node_alpha,
                             node_alpha_aggr) {

    nodes <- get_tree_nodes(clusterings, prefix, metadata, node_colour,
                            node_colour_aggr, node_size, node_size_aggr,
                            node_alpha, node_alpha_aggr)

    edges <- get_tree_edges(clusterings, prefix) %>%
        dplyr::filter(.data$count > count_filter) %>%
        dplyr::filter(.data$in_prop > prop_filter)

    graph <- igraph::graph_from_data_frame(edges, vertices = nodes)

    # Convert resolution to factor
    igraph::vertex_attr(graph)[[prefix]] <-
        factor(as.numeric(igraph::vertex_attr(graph)[[prefix]]))

    graph <- store_node_aes(graph, "colour", node_colour, node_colour_aggr,
                            metadata)
    graph <- store_node_aes(graph, "size", node_size, node_size_aggr,
                            metadata)
    graph <- store_node_aes(graph, "alpha", node_alpha, node_alpha_aggr,
                            metadata)

    return(graph)
}

#' Get tree nodes
#'
#' Extract the nodes from a set of clusterings and add relevant attributes
#'
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param prefix string indicating columns containing clustering information
#' @param node_colour either a value indicating a colour to use for all nodes or
#' the name of a metadata column to colour nodes by
#' @param node_colour_aggr if `node_colour` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_size either a numeric value giving the size of all nodes or the
#' name of a metadata column to use for node sizes
#' @param node_size_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#' @param node_alpha either a numeric value giving the alpha of all nodes or the
#' name of a metadata column to use for node transparency
#' @param node_alpha_aggr if `node_size` is a column name than a function to
#' aggregate that column for samples in each cluster
#'
#' @return data.frame containing node information
get_tree_nodes <- function(clusterings, prefix, metadata, node_colour,
                           node_colour_aggr, node_size, node_size_aggr,
                           node_alpha, node_alpha_aggr) {

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

            aes_list <- list(list(aes = node_colour, aggr = node_colour_aggr),
                             list(aes = node_size, aggr = node_size_aggr),
                             list(aes = node_alpha, aggr = node_alpha_aggr))

            for (aes in aes_list) {
                node_data <- aggr_metadata(node_data, aes[[1]], aes[[2]],
                                           metadata, is_cluster)
            }

            node_data <- data.frame(node_data, stringsAsFactors = FALSE)

            return(node_data)
        })

        node <- do.call("rbind", node)

    })

    nodes <- do.call("rbind", nodes)

    return(nodes)
}

#' Get tree edges
#'
#' Extract the edges from a set of clusterings
#'
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#' @param prefix string indicating columns containing clustering information
#'
#' @return data.frame containing edge information
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
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

            in_prop <- trans_count / to_size

            return(c(trans_count, in_prop))
        })

        from_tos$from_res <- as.numeric(gsub(prefix, "", from_res))
        from_tos$to_res <- as.numeric(gsub(prefix, "", to_res))
        from_tos$count <- transitions[1, ]
        from_tos$in_prop <- transitions[2, ]

        return(from_tos)
    })

    edges <- dplyr::bind_rows(edges) %>%
        dplyr::mutate(from_node = paste0(prefix, .data$from_res,
                                         "C", .data$from_clust)) %>%
        dplyr::mutate(to_node = paste0(prefix, .data$to_res,
                                       "C", .data$to_clust)) %>%
        dplyr::select(.data$from_node, .data$to_node, dplyr::everything())

    return(edges)

}


#' Aggregate metadata
#'
#' Aggregate a metadata column to get a summarized value for a cluster node
#'
#' @param node_data data.frame containing information about a set of cluster
#' nodes
#' @param col_name the name of the metadata column to aggregate
#' @param col_aggr string naming a function used to aggregate the column
#' @param metadata data.frame providing metadata on samples
#' @param is_cluster logical vector indicating which rows of metadata are in the
#' node to be summarized
#'
#' @return data.frame with aggregated data
aggr_metadata <- function(node_data, col_name, col_aggr, metadata,
                          is_cluster) {

    if (col_name %in% colnames(metadata)) {
        clust_meta <- metadata[is_cluster, col_name]
        col_aggr_fun <- match.fun(col_aggr)
        aggr_col_name <- paste0(col_aggr, "_", col_name)
        node_data[aggr_col_name] <- col_aggr_fun(clust_meta)
    }

    return(node_data)
}


#' Store node aesthetics
#'
#' Store the names of node attributes to use as aesthetics as graph attributes
#'
#' @param graph graph to store attributes in
#' @param node_aes_name name of the aesthetic to store
#' @param node_aes value of the aesthetic to store
#' @param node_aes_aggr name of an aggregation function associated with the
#' aesthetic to store
#' @param metadata data.frame containing metadata that can be used as aesthetics
#'
#' @return graph with additional attributes
store_node_aes <- function(graph, node_aes_name, node_aes, node_aes_aggr,
                           metadata) {

    node_aes_value <- node_aes

    if (node_aes %in% colnames(metadata)) {
        node_aes_value <- paste0(node_aes_aggr, "_", node_aes)
    }

    graph <- igraph::set_graph_attr(graph, paste0("node_", node_aes_name),
                                    node_aes_value)

    return(graph)
}
