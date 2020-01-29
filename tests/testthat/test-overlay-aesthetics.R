data("iris_clusts")

test_that("Colour, Size, Alpha node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = "size",
                         node_colour = "size", node_alpha = "size"),
        c("gg", "ggplot")
    )
})

test_that("Colour, Alpha node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = 1,
                         node_colour = "size", node_alpha = "size"),
        c("gg", "ggplot")
    )
})

test_that("Colour node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = 1,
                         node_colour = "size", node_alpha = 1),
        c("gg", "ggplot")
    )
})

test_that("Size, alpha node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = "size",
                         node_colour = 1, node_alpha = "size"),
        c("gg", "ggplot")
    )
})

test_that("Size node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = "size",
                         node_colour = 1, node_alpha = 1),
        c("gg", "ggplot")
    )
})

test_that("Alpha node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = 1,
                         node_colour = 1, node_alpha = "size"),
        c("gg", "ggplot")
    )
})

test_that("None node aesthetics work", {
    expect_is(
        clustree_overlay(iris_clusts, prefix = "K", x_value = "PC1",
                         y_value = "PC2", node_size = 1,
                         node_colour = 1, node_alpha = 1),
        c("gg", "ggplot")
    )
})