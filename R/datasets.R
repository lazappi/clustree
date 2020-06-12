#' Clustered nba dataset
#'
#' iris dataset clustered using k-means with a range of values of k
#'
#' @format `iris_clusts` is a data.frame containing the normal `iris` dataset
#' with additional columns holding k-means clusterings at different values of k
#' and the first two principal components
#'
#' @source
#' ```
#' set.seed(1)
#' iris_mat <- as.matrix(iris[1:4])
#' iris_km <- sapply(1:5, function(x) {
#'     km <- kmeans(iris_mat, centers = x, iter.max = 100, nstart = 10)
#'     km$cluster
#' })
#' colnames(iris_km) <- paste0("K", 1:5)
#' iris_clusts <- cbind(iris, iris_km)
#' iris_pca <- prcomp(iris_clusts[1:4])
#' iris_clusts$PC1 <- iris_pca$x[, 1]
#' iris_clusts$PC2 <- iris_pca$x[, 2]
#' ```
"iris_clusts"


#' Clustered NBA positions dataset
#'
#' NBA positions dataset clustered using k-means with a range of values of `k`
#'
#' @format `nba_clusts` is a data.frame containing the NBA positions dataset
#' with additional columns holding k-means clusterings at different values of
#' `k` and the first two principal components
#'
#' * **Position** - Player position
#' * **TurnoverPct** - Turnover percentage
#' * **ReboundPct** - Rebound percentage
#' * **AssistPct** - Assist percentage
#' * **FieldGoalPct** - Field goal percentage
#' * **K1 - K5** - Results of k-means clustering
#' * **PC1** - First principal component
#' * **PC2** - Second principal component
#'
#' @source
#' NBA positions downloaded from <https://github.com/lazappi/nba_positions>.
#'
#' The source dataset is available from Kaggle at
#' <https://www.kaggle.com/drgilermo/nba-players-stats/data?select=Seasons_Stats.csv>
#' and was originally scraped from [Basketball Reference](http://www.basketball-reference.com/).
#'
#' See <https://github.com/lazappi/clustree/blob/master/data-raw/nba_clusts.R>
#' for details of how clustering was performed.
"nba_clusts"


#' Simulated scRNA-seq dataset
#'
#' A simulated scRNA-seq dataset generated using the `splatter` package and
#' clustered using the `SC3` and `Seurat` packages.
#'
#' @format `sc_example` is a list holding a simulated scRNA-seq dataset. Items
#' in the list included the simulated counts, normalised log counts,
#' tSNE dimensionality reduction and cell assignments from `SC3` and `Seurat`
#' clustering.
#'
#' @source
#' ```
#' # Simulation
#' library("splatter") # Version 1.2.1
#'
#' sim <- splatSimulate(batchCells = 200, nGenes = 10000,
#'                      group.prob = c(0.4, 0.2, 0.2, 0.15, 0.05),
#'                      de.prob = c(0.1, 0.2, 0.05, 0.1, 0.05),
#'                      method = "groups", seed = 1)
#' sim_counts <- counts(sim)[1:1000, ]
#'
#' # SC3 Clustering
#' library("SC3") # Version 1.7.6
#' library("scater") # Version 1.6.2
#'
#' sim_sc3 <- SingleCellExperiment(assays = list(counts = sim_counts))
#' rowData(sim_sc3)$feature_symbol <- rownames(sim_counts)
#' sim_sc3 <- normalise(sim_sc3)
#' sim_sc3 <- sc3(sim_sc3, ks = 1:8, biology = FALSE, n_cores = 1)
#' sim_sc3 <- runTSNE(sim_sc3)
#'
#' # Seurat Clustering
#' library("Seurat") # Version 2.2.0
#'
#' sim_seurat <- CreateSeuratObject(sim_counts)
#' sim_seurat <- NormalizeData(sim_seurat, display.progress = FALSE)
#' sim_seurat <- FindVariableGenes(sim_seurat, do.plot = FALSE,
#'                                 display.progress = FALSE)
#' sim_seurat <- ScaleData(sim_seurat, display.progress = FALSE)
#' sim_seurat <- RunPCA(sim_seurat, do.print = FALSE)
#' sim_seurat <- FindClusters(sim_seurat, dims.use = 1:6,
#'                            resolution = seq(0, 1, 0.1),
#'                            print.output = FALSE)
#'
#' sc_example <- list(counts = counts(sim_sc3),
#'                    logcounts = logcounts(sim_sc3),
#'                    tsne = reducedDim(sim_sc3),
#'                    sc3_clusters = as.data.frame(colData(sim_sc3)),
#'                    seurat_clusters = sim_seurat@meta.data)
#' ```
"sc_example"
