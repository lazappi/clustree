#' Assert node aesthetics
#'
#' Raise error if an incorrect set of node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation function associated with the node aesthetic
#'
#' @importFrom utils head
assert_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                            node_aes_aggr) {

    allowed <- c(prefix, "cluster", "size", "sc3_stability")

    checkmate::assert_character(node_aes, len = 1, .var.name = node_aes_name)

    if (!is.null(metadata)) {
        if (!(node_aes %in% c(colnames(metadata), allowed))) {
            stop(node_aes_name, " must be one of: ",
                 paste(allowed, collapse = ", "),
                 ", or a column in metadata: ",
                 paste(head(colnames(metadata)), collapse = ", "), "...",
                 call. = FALSE)
        }
    } else {
        if (!(node_aes %in% allowed)) {
            stop("If metadata is not supplied ", node_aes_name,
                 "can only be one of: ", paste(allowed, collapse = ", "),
                 call. = FALSE)
        }
    }

    if (!(node_aes %in% allowed)) {
        checkmate::assert_character(node_aes_aggr, len = 1, any.missing = FALSE,
                                    .var.name = paste0(node_aes_name, "_aggr"))
        if (!is.null(node_aes_aggr)) {
            node_aes_aggr_fun <- match.fun(node_aes_aggr)
            checkmate::assert_function(node_aes_aggr_fun,
                                       .var.name = paste0(node_aes_name,
                                                          "_aggr"))
        }
    }
}


#' Assert numeric node aesthetics
#'
#' Raise error if an incorrect set of numeric node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation function associated with the node aesthetic
#' @param min minimum numeric value allowed
#' @param max maximum numeric value allowed
assert_numeric_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                                    node_aes_aggr, min, max) {

    num_chk <- checkmate::check_number(node_aes)

    if (!(num_chk == TRUE)) {
        assert_node_aes(node_aes_name, prefix, metadata, node_aes,
                        node_aes_aggr)
    } else {
        checkmate::assert_number(node_aes, lower = min, upper = max,
                                 .var.name = node_aes_name)
    }

}


#' Assert colour node aesthetics
#'
#' Raise error if an incorrect set of colour node parameters has been supplied.
#'
#' @param node_aes_name name of the node aesthetic to check
#' @param prefix string indicating columns containing clustering information
#' @param metadata data.frame containing metadata on each sample that can be
#' used as node aesthetics
#' @param node_aes value of the node aesthetic to check
#' @param node_aes_aggr aggregation function associated with the node aesthetic
#' @param min minimum numeric value allowed
#' @param max maximum numeric value allowed
#'
#' @importFrom grDevices col2rgb
assert_colour_node_aes <- function(node_aes_name, prefix, metadata, node_aes,
                                   node_aes_aggr, min, max) {

    num_chk <- checkmate::check_number(node_aes)
    allowed <- c(prefix, "cluster", "size", "sc3_stability", colnames(metadata))

    if (!(num_chk == TRUE)) {
        if (node_aes %in% allowed) {
            assert_node_aes(node_aes_name, prefix, metadata, node_aes,
                            node_aes_aggr)
        } else {
            tryCatch(col2rgb(node_aes),
                     error = function(e) {
                         stop(node_aes_name, " is set to '", node_aes, "' ",
                              "which is not a valid colour name. Other options",
                              " include a number or the name of a metadata ",
                              "column.", call. = FALSE)
                     })
        }
    } else {
        checkmate::assert_number(node_aes, lower = 0, .var.name = node_aes_name)
    }

}


#' Check node aes list
#'
#' Warn if node aesthetic names are incorrect
#'
#' @param node_aes_list List of node aesthetics
#'
#' @return Corrected node aesthetics list
check_node_aes_list <- function(node_aes_list) {
    for (aes in names(node_aes_list)) {
        aes_value <- node_aes_list[[aes]]$value
        if (is.character(aes_value)) {
            aes_name <- make.names(aes_value)
            if (aes_value != aes_name) {
                warning(
                    "node_", aes, " will be converted from ", aes_value, " to ",
                    aes_name,
                    call. = FALSE
                )
                node_aes_list[[aes]]$value <- aes_name
            }
        }
    }

    return(node_aes_list)
}
