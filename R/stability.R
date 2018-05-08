calc_sc3_stability <- function(clusterings) {

    stabilities <- lapply(seq_len(ncol(clusterings)), function(k) {
        clusters <- sort(unique(clusterings[, k]))
        cluster_ss <- sapply(clusters, function(i) {
            s <- calc_sc3_stability_single(clusterings, k, i)
            c(Resolution = k, Cluster = i, Stability = s)
        })
        t(cluster_ss)
    })

    stabilities <- do.call("rbind", stabilities)

    return(stabilities)
}

calc_sc3_stability_single <- function(clusterings, k, i) {

    is_c1 <- clusterings[, k] == i

    s <- 0

    for (l in seq_len(ncol(clusterings))) {
        if (l == k) {
            next
        }

        clusters <- unique(clusterings[is_c1, l])

        for (j in clusters) {
            is_c2 <- clusterings[, l] == j

            overlap <- sum(is_c1 & is_c2)

            s <- s + (overlap / (sum(is_c2) * length(clusters) ** 2))
        }
    }

    s <- s / ncol(clusterings)

    return(s)
}
