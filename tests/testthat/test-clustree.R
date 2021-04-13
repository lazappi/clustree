test_that("clustree works with data.frame", {
    expect_s3_class(
        suppressMessages(clustree(nba_clusts, prefix = "K")),
        "ggraph"
    )
})

test_that("clustree works with SingleCellExperiment", {
    expect_s3_class(
        suppressMessages(
            clustree(sce_example(), prefix = "sc3_", suffix = "_clusters")
        ),
        "ggraph"
    )
})

test_that("clustree works with Seurat", {
    expect_s3_class(
        suppressMessages(clustree(seurat_example(), prefix = "res.")),
        "ggraph"
    )
})

test_that("clustree_overlay works with data.frame", {
    expect_s3_class(
        suppressMessages(
            clustree_overlay(nba_clusts, prefix = "K", x_value = "PC1",
                             y_value = "PC2")
        ),
        "ggraph"
    )
})

# test_that("clustree works with SingleCellExperiment", {
#     expect_s3_class(
#         suppressMessages(
#             clustree_overlay(
#                 sce_example(), prefix = "sc3_", suffix = "_clusters",
#                 x_value = "TSNE1", y_value = "TSNE2"
#             )
#         ),
#         "ggraph"
#     )
# })
#
# test_that("clustree works with Seurat", {
#     expect_s3_class(
#         suppressMessages(
#             clustree_overlay(seurat_example(), prefix = "res.",
#                              x_value = "TSNE1", y_value = "TSNE2")
#         ),
#         "ggraph"
#     )
# })
