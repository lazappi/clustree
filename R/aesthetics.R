set_default_aes <- function(aes, default_aes, edge = FALSE) {

    abort_class(aes, "uneval", null.ok = TRUE)
    abort_class(default_aes, "uneval")

    if (is.null(aes)) {
        return(default_aes)
    }

    if (any(names(aes) == "color")) {
        names(aes)[names(aes) == "color"] <- "colour"
    }

    if (edge) {
        if (any(names(aes) == "edge_color")) {
            names(aes)[names(aes) == "edge_color"] <- "edge_colour"
        }

        aes <- expand_edge_aes(aes)
    }

    aes <- c(as.list(aes), default_aes[!(names(default_aes) %in% names(aes))])

    class(aes) <- "uneval"

    return(aes)
}

expand_edge_aes <- function(x) {
    is_short_names <- names(x) %in% c(
        "colour", "color", "fill", "linetype", "shape", "size", "width", "alpha"
    )

    names(x)[is_short_names] <- paste0(
        "edge_",
        names(x)[is_short_names]
    )

    return(x)
}

set_default_params <- function(params, default_params, aes) {

    for (param in names(default_params)) {
        if (!(param %in% c(names(params), names(aes)))) {
            params[[param]] <- default_params[[param]]
        }
    }

    return(params)
}
