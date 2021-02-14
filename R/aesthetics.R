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

        is_short_names <- names(aes) %in%
            c("colour", "fill", "linetype", "shape", "size", "width", "alpha")

        names(aes)[is_short_names] <- paste0(
            "edge_",
            names(aes)[is_short_names]
        )
    }

    aes <- c(as.list(aes), default_aes[!(names(default_aes) %in% names(aes))])

    class(aes) <- "uneval"

    return(aes)
}
