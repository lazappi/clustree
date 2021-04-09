test_that("geom_clustree_point return GeomClustreePoint", {
    geom <- geom_clustree_point()
    expect_s3_class(geom, "LayerInstance")
    expect_s3_class(geom$geom, "GeomClustreePoint")
})

test_that("geom_clustree_text return GeomClustreeText", {
    geom <- geom_clustree_text()
    expect_s3_class(geom, "LayerInstance")
    expect_s3_class(geom$geom, "GeomClustreeText")
})

test_that("geom_clustree_edge return GeomClustreeEdge", {
    geom <- geom_clustree_edge()
    expect_s3_class(geom, "LayerInstance")
    expect_s3_class(geom$geom, "GeomClustreeEdge")
})
