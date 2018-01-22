clustree <- function (x, ...) {
    UseMethod("clustree", x)
}

clustree.data.frame <- function(x, prefix, ...) {

    checkmate::assert_character(prefix, len = 1)

    clust_cols <- grepl(prefix, colnames(x))

    clusterings <- as.matrix(x[, clust_cols])
    colnames(clusterings) <- gsub(prefix, "", colnames(clusterings))
    clusterings <- clusterings[, sort(as.numeric(colnames(clusterings)))]

    if (sum(!clust_cols) > 0) {
        metadata <- x[, !clust_cols]
    } else {
        metadata <- NULL
    }

    clustree(clusterings, meta = metadata, ...)
}

clustree.matrix <- function(x, ...) {
    print("MATRIX!")
    print(head(x))
}