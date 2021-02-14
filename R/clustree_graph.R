#' Data structure for manipulating clustering trees
#'
#' The `clustree_graph` class is an extension to the `tbl_graph` class which
#' adds a table for storing metadata about individual samples. Unlike most
#' graphs where is node is a sample, nodes in a clustering tree represent
#' clusters of multiple samples. Storing metadata about the original samples
#' makes it possible to calculate additional node (or edge) level summaries as
#' required rather than having to compute these when the graph is created.
#'
#' @param x Object to convert to a `clustree_graph`
#' @param ... Arguments passed to [tidygraph::tbl_graph()] or conversion
#' functions
#' @param metadata A `data.frame` providing information about individual
#' samples. If a column named `.clustree_idx` is not already present it will
#' be created using the row names. If `NULL` an empty `tibble` is created.
#'
#' @details
#' A `clustree_graph` object should usually be created using the
#' [build_clustree_graph()] function. If using [clustree_graph()] directly then
#' both the `nodes` and `edges` tables must include a column named
#' "`.clustree_indices`" which is a list of vectors giving the indices in
#' the `.clustree_idx` column of `metadata` matching the samples associated
#' with each node/edge.
#'
#' The metadata table is stored as a graph attribute accessible with
#' [igraph::graph_attr()].
#'
#' @return A `clustree_graph` object
#' @export
#'
#' @seealso [tidygraph::tbl_graph()] for the `tbl_graph` object and
#' [build_clustree_graph()] for building `clustree_graph` objects from various
#' inputs
#'
#' @examples
#' nodes <- tibble::tibble(
#'     node = c("A", "B"),
#'     .clustree_indices = list(c("1", "2"), c("1"))
#' )
#'
#' edges <- tibble::tibble(
#'     from = "A",
#'     to   = "B",
#'     .clustree_indices = list(c("1"))
#' )
#'
#' metadata <- data.frame(
#'     .clustree_idx = c("1", "2"),
#'     value = c("Example1", "Example2")
#' )
#'
#' clustree_graph(nodes = nodes, edges = edges, metadata = metadata)
clustree_graph <- function(..., metadata = NULL) {

    abort_data_frame(metadata, null.ok = TRUE)

    tbl_graph <- tidygraph::tbl_graph(...)

    as_clustree_graph(tbl_graph, metadata = metadata)
}

#' @rdname clustree_graph
#' @export
as_clustree_graph <- function(x, ..., metadata = NULL) {

    abort_data_frame(metadata, null.ok = TRUE)

    UseMethod("as_clustree_graph")
}

#' @describeIn clustree_graph Default method. Tries to call
#' [tidygraph::as_tbl_graph()] on the input before converting.
#' @export
as_clustree_graph.default <- function(x, ..., metadata = NULL) {
    tryCatch({
        as_clustree_graph(tidygraph::as_tbl_graph(x, ...), metadata = metadata)
    }, error = function(err) {
        abort(paste0("No support for ", class(x)[1], " objects'"))
    })
}

#' @describeIn clustree_graph Method for `tbl_graph` objects. Stores `metadata`
#' and appends the class.
#' @export
as_clustree_graph.tbl_graph <- function(x, ..., metadata = NULL) {

    x <- tidygraph::activate(x, "edges")
    if (!(".clustree_indices" %in% colnames(tibble::as_tibble(x)))) {
        abort("edges do not contain a '.clustree_indices' column")
    }

    x <- tidygraph::activate(x, "nodes")
    if (!(".clustree_indices" %in% colnames(tibble::as_tibble(x)))) {
        abort("nodes do not contain a '.clustree_indices' column")
    }

    if (!is.null(metadata) && !(".clustree_idx") %in% colnames(metadata)) {
        inform("Adding '.clustree_idx' column to metadata using row names")
        metadata[, ".clustree_idx"] <- rownames(metadata)
    }

    if (is.null(metadata)) {
        metadata <- tibble::tibble(.clustree_index = character())
    } else {
        metadata <- tibble::as_tibble(metadata)
    }

    x <- igraph::set_graph_attr(x, ".clustree_metadata", metadata)

    class(x) <- c("clustree_graph", "tbl_graph", "igraph")

    return(x)
}

#' @rdname clustree_graph
#' @export
is.clustree_graph <- function(x) {
    inherits(x, "clustree_graph")
}

#' @importFrom utils modifyList
#' @export
print.clustree_graph <- function(x, ...) {
    arg_list <- list(...)

    cat_subtle("# A clustree_graph:\n")
    cat_subtle("#\n")

    cat_subtle("# --------------\n")
    cat_subtle("#\n")
    NextMethod()
    cat_subtle("#\n")
    cat_subtle("# --------------\n")

    metadata <- igraph::graph_attr(x, ".clustree_metadata")
    trunc <- do.call(
        tibble::trunc_mat,
        modifyList(arg_list, list(x = metadata, n = 6))
    )
    names(trunc$summary)[1] <- "Metadata"

    cat_subtle("#\n")
    print(trunc)

    invisible(x)
}
