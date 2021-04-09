aes <- ggplot2::aes(color = .data$colour, shape = .data$shape)
default_aes <- ggplot2::aes(x = .data$x, y = .data$y)

test_that("set_default_aes checks work", {
    expect_error(
        set_default_aes(NULL, list()),
        "Must inherit from class 'uneval'"
    )
    expect_error(
        set_default_aes(list(), default_aes),
        "Must inherit from class 'uneval'"
    )
})

test_that("set_default_aes works", {
    set_aes <- set_default_aes(aes, default_aes)
    expect_identical(names(set_aes), c("colour", "shape", "x", "y"))
})

test_that("set_default_aes works for edges", {
    set_aes <- set_default_aes(aes, default_aes, edge = TRUE)
    expect_identical(names(set_aes), c("edge_colour", "edge_shape", "x", "y"))
})

test_that("expand_edge_aes works", {
    edge_aes <- ggplot2::aes(
        color    = .data$color,
        fill     = .data$fill,
        linetype = .data$linetype,
        shape    = .data$shape,
        size     = .data$size,
        width    = .data$width,
        alpha    = .data$alpha
    )
    expanded_aes <- expand_edge_aes(edge_aes)
    expect_identical(
        names(expanded_aes),
        c("edge_colour", "edge_fill", "edge_linetype", "edge_shape",
          "edge_size", "edge_width", "edge_alpha")
    )
})

test_that("set_default_params works", {
    params <- list(a = 1, b = 2)
    default_params <- list(c = 3, colour = "red")
    set_params <- set_default_params(params, default_params, aes)
    expect_identical(names(set_params), c("a", "b", "c"))
})
