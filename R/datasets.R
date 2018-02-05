#' Clustered Iris dataset
#'
#' Iris dataset clustered using k-means with a range of values of k
#'
#' @format `iris_clusts` is a data.frame containing the normal `iris` dataset
#' with additional columns holding k-means clusterings at different values of k
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
#' ```
"iris_clusts"


#' SC3 Clustered simulated scRNA-seq dataset
#'
#' A simulated scRNA-seq dataset generated using the `splatter` package and
#' clustered using `SC3`.
#'
#' @format `sim_sc3` is a [SingleCellExperiment::SingleCellExperiment] object
#' holding a simulated scRNA-seq dataset. The dataset has been clustered using
#' the `SC3` package with a range of values for k. The results of the clustering
#' are held in the `colData` slot with columns named `sc3_X_clusters` where `X`
#' is the value of `k`.
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
#' # Clustering
#' library("SC3") # Version 1.7.6
#' library("scater") # Version 1.6.2
#'
#' sim_sc3 <- SingleCellExperiment(assays = list(counts = sim_counts))
#' rowData(sim_sc3)$feature_symbol <- rownames(sim_counts)
#' sim_sc3 <- normalise(sim_sc3)
#' sim_sc3 <- sc3(sim_sc3, ks = 1:8, biology = FALSE, n_cores = 1)
#' ```
"sim_sc3"


#' Seurat Clustered simulated scRNA-seq dataset
#'
#' A simulated scRNA-seq dataset generated using the `splatter` package and
#' clustered using `Seurat`.
#'
#' @format `sim_seurat` is a [Seurat::seurat] object holding a simulated
#' scRNA-seq dataset. The dataset has been clustered using the `Seurat` package
#' with a range of values for the resolution parameter. The results of the
#' clustering are held in the `meta.data` slot with columns named `res.X` where
#' `X` is the value of the resolution parameter.
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
#' library("Seurat") # Version 2.2.0
#' sim_seurat <- CreateSeuratObject(sim_counts)
#' sim_seurat <- NormalizeData(sim_seurat, display.progress = FALSE)
#' sim_seurat <- FindVariableGenes(sim_seurat, do.plot = FALSE,
#'                                 display.progress = FALSE)
#' sim_seurat <- ScaleData(sim_seurat, display.progress = FALSE)
#' sim_seurat <- RunPCA(sim_seurat, do.print = FALSE)
#' sim_seurat <- FindClusters(sim_seurat, dims.use = 1:6,
#'                            resolution = seq(0, 1, 0.1),
#'                            print.output = FALSE)
#' ```
"sim_seurat"