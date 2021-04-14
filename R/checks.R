abort_data_frame <- function(...) {
    rlang::with_abort(checkmate::assert_data_frame(...))
}

abort_character <- function(...) {
    rlang::with_abort(checkmate::assert_character(...))
}

abort_matrix <- function(...) {
    rlang::with_abort(checkmate::assert_matrix(...))
}

abort_class <- function(...) {
    rlang::with_abort(checkmate::assert_class(...))
}

abort_number <- function(...) {
    rlang::with_abort(checkmate::assert_number(...))
}

abort_integerish <- function(...) {
    rlang::with_abort(checkmate::assert_integerish(...))
}
