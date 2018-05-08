#' Calculate SC3 stability
#'
#' Calculate the SC3 stability index for every cluster at every resolution in a
#' set of clusterings. The index varies from 0 to 1, where 1 suggests that a
#' cluster is more stable across resolutions. See [calc_sc3_stability_cluster()]
#' for more details.
#'
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#'
#' @return matrix with stability score for each cluster
calc_sc3_stability <- function(clusterings) {

    stabilities <- lapply(seq_len(ncol(clusterings)), function(res) {
        clusters <- sort(unique(clusterings[, res]))
        cluster_ss <- sapply(clusters, function(cluster) {
            s <- calc_sc3_stability_single(clusterings, res, cluster)
            c(resolution = res, cluster = cluster, stability = s)
        })
        t(cluster_ss)
    })

    stabilities <- do.call("rbind", stabilities)

    return(stabilities)
}


#' Calculate single SC3 stability
#'
#' Calculate the SC3 stability index for a single cluster in a set of
#' clusterings. The index varies from 0 to 1, where 1 suggests that a cluster is
#' more stable across resolutions.
#'
#' @param clusterings numeric matrix containing clustering information, each
#' column contains clustering at a separate resolution
#' @param res resolution of the cluster to calculate stability for
#' @param cluster index of the cluster to calculate stability for
#'
#' @details
#'
#' This index was originally introduced in the `SC3` package for clustering
#' single-cell RNA-seq data. Clusters are awarded increased stability if they
#' share the same samples as a cluster at another resolution and penalised at
#' higher resolutions. We use a slightly different notation to describe the
#' score but the results are the same:
#'
#' \deqn{
#'   s(c_{k, i}) =
#'   \frac{1}{size(L) + 1}
#'   \sum_{l \in L}
#'     \sum_{j \in N_l}
#'       \frac{size(c_{k, i} \cap c_{l, j})}{size(c_{l, j}) * size(N_l) ^ 2}
#' }{
#'   s(c_{k, i}) = (1 / (size(L) + 1)) * (sum_{l in L} sum_{j in N_l}
#'   (size(intersect(c_{k, i}, c_{l, j}))) / (size(c_{l, j}) * size(N_l) ^ 2))
#' }
#'
#' Where:
#'
#' * `c_{x, y}` is cluster `y` at resolution `x`
#' * `k` is the resolution of the cluster we want to score
#' * `i` is the index of the cluster we want to score
#' * `L` is the set of all resolutions except `k`
#' * `l` is a resolution in `L`
#' * `N_l` is the set of clusters at resolution `l` that share samples with
#'   `c_{k, i}`
#' * `j` is a cluster in `N_l`
#'
#' @seealso The documentation for the `calculate_stability` function in the
#' SC3 package
#'
#' @return SC3 stability index
calc_sc3_stability_cluster <- function(clusterings, res, cluster) {

    is_c1 <- clusterings[, res] == cluster

    s <- 0

    for (res2 in seq_len(ncol(clusterings))) {
        if (res2 == res) {
            next
        }

        clusters <- unique(clusterings[is_c1, res2])

        for (cluster2 in clusters) {
            is_c2 <- clusterings[, res2] == cluster2

            overlap <- sum(is_c1 & is_c2)

            s <- s + (overlap / (sum(is_c2) * length(clusters) ** 2))
        }
    }

    s <- s / ncol(clusterings)

    return(s)
}
