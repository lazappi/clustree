test_that("loading sc_example works", {
    data("sc_example", package = "clustree", envir = environment())
    expect_true(is.list(sc_example))
})

test_that("loading sc_example as SCE works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_s4_class(sce_example(), "SingleCellExperiment")
})
