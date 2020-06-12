context("clustree")

data("nba_clusts")
data("sc_example")

# Add gene name with "-" for some tests
rownames(sc_example$counts)[1] <- "A-Gene"

nba_clusts2 <- nba_clusts
nba_clusts2[["A-1"]] <- nba_clusts2$ReboundPct

nba_clusts3 <- nba_clusts
nba_clusts3$K1 <- "A"

nba_clusts4 <- nba_clusts
nba_clusts4$L1 <- nba_clusts4$K1
nba_clusts4$L2 <- nba_clusts4$K2

nba_clusts6 <- nba_clusts
nba_clusts6$KX <- "X"

seurat_clusters2 <- sc_example$seurat_clusters
seurat_clusters2$resX <- "X"

if (requireNamespace("Seurat", quietly = TRUE) &&
    packageVersion(pkg = "Seurat") < package_version(x = "3.0.0")) {
    library("Seurat")
    seurat <- CreateSeuratObject(sc_example$counts,
                                 meta.data = sc_example$seurat_clusters)
    seurat <- SetDimReduction(seurat, "TSNE", "cell.embeddings",
                              sc_example$tsne)
}

if (requireNamespace("Seurat", quietly = TRUE) &&
    packageVersion(pkg = "Seurat") >= package_version(x = "3.0.0")) {
    library("Seurat")
    seurat <- CreateSeuratObject(counts = sc_example$counts,
                                 meta.data = sc_example$seurat_clusters)
    seurat[["TSNE"]] <- suppressWarnings(CreateDimReducObject(
        embeddings = sc_example$tsne,
        key = "tSNE_",
        assay = DefaultAssay(seurat)
    ))
}

if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    library("SingleCellExperiment")
    sce <- SingleCellExperiment(
        assays = list(counts = sc_example$counts,
                      logcounts = sc_example$logcounts),
        colData = sc_example$sc3_clusters,
        reducedDims = SimpleList(TSNE = sc_example$tsne))
}

test_that("data.frame interface works", {
    expect_is(clustree(nba_clusts, prefix = "K"), c("gg", "ggplot"))
})

test_that("SingleCellExperiment interface works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_is(clustree(sce, prefix = "sc3_", suffix = "_clusters"),
              c("gg", "ggplot"))
})

test_that("Seurat interface works", {
    skip_if_not_installed("Seurat")
    expect_is(clustree(seurat, prefix = "res."), c("gg", "ggplot"))
})

test_that("column number check works", {
    expect_error(clustree(nba_clusts[1:5], prefix = "K"),
                 "Less than two column names matched")
    expect_error(clustree(nba_clusts[1:6], prefix = "K"),
                 "Less than two column names matched")
})

test_that("metadata column name check works", {
    expect_warning(clustree(nba_clusts2, prefix = "K"),
                   "The following metadata column names will be converted")
})

test_that("aesthetics name check works", {
    expect_warning(clustree(nba_clusts2, prefix = "K", node_colour = "A-1",
                            node_colour_aggr = "mean"),
                   "node_colour will be converted from")
    expect_warning(clustree(nba_clusts2, prefix = "K", node_size = "A-1",
                            node_size_aggr = "mean"),
                   "node_size will be converted from")
    expect_warning(clustree(nba_clusts2, prefix = "K", node_alpha = "A-1",
                            node_alpha_aggr = "mean"),
                   "node_alpha will be converted from")
})

test_that("returning graph works", {
    expect_is(clustree(nba_clusts, prefix = "K", return = "graph"),
              c("tbl_graph"))
})

test_that("returning layout works", {
    expect_is(clustree(nba_clusts, prefix = "K", return = "layout"),
              c("layout_igraph", "layout_ggraph"))
})

test_that("show_axis works", {
    expect_is(clustree(nba_clusts, prefix = "K", show_axis = TRUE),
              c("gg", "ggplot"))
})

test_that("character cluster names work", {
    expect_is(clustree(nba_clusts3, prefix = "K"),
              c("gg", "ggplot"))
})

test_that("exact prefix selection works", {
    # Fails if matches additional columns
    expect_is(clustree(nba_clusts4, prefix = "L"), c("gg", "ggplot"))
})

test_that("prefix selection doesn't match wildcards", {
    expect_is(clustree(seurat_clusters2, prefix = "res."), c("gg", "ggplot"))
})

test_that("check for non-numeric resolution works", {
    expect_error(clustree(nba_clusts6, prefix = "K"),
                 "The X portion of your clustering column names could not be ")
})

test_that("node labels work", {
    expect_is(clustree(nba_clusts, prefix = "K", node_label = "cluster"),
              c("gg", "ggplot"))
})

test_that("node labels with fixed colour work", {
    expect_is(clustree(nba_clusts, prefix = "K", node_label = "cluster",
                       node_colour = "red"),
              c("gg", "ggplot"))
})

test_that("SCE aesthetics work", {
    skip_if_not_installed("SingleCellExperiment")
    expect_is(clustree(sce, prefix = "sc3_", suffix = "_clusters",
                       node_colour = "Gene2", node_colour_aggr = "mean"),
              c("gg", "ggplot"))
})

test_that("Seurat aesthetics work", {
    skip_if_not_installed("Seurat")
    expect_is(clustree(seurat, prefix = "res.",
                       node_colour = "Gene2", node_colour_aggr = "mean"),
              c("gg", "ggplot"))
})

test_that("SCE feature containing '-' works", {
    skip_if_not_installed("SingleCellExperiment")
    expect_warning(clustree(sce, prefix = "sc3_", suffix = "_clusters",
                            node_colour = "A-Gene", node_colour_aggr = "mean"),
              c("will be converted to"))
})

test_that("Seurat feature containing '-' works", {
    skip_if_not_installed("Seurat")
    expect_warning(clustree(seurat, prefix = "res.", node_colour = "A-Gene",
                            node_colour_aggr = "mean"),
              c("will be converted to"))
})

test_that("node text scaling works", {
    expect_is(clustree(nba_clusts, prefix = "K", node_size = "ReboundPct",
                       node_size_aggr = "mean", scale_node_text = TRUE),
              c("gg", "ggplot"))
})

test_that("non-arrow edges works", {
    expect_is(clustree(nba_clusts, prefix = "K", edge_arrow = FALSE),
              c("gg", "ggplot"))
})
