data_url <- "https://github.com/lazappi/nba_positions/raw/master/nba_positions.tsv"

nba_positions <- read.delim(data_url, sep = "\t")

stats_mat <- nba_positions[, 2:5]

set.seed(8)
k_means <- sapply(1:5, function(x) {
    km <- kmeans(stats_mat, centers = x, iter.max = 100, nstart = 1000)
    km$cluster
})

colnames(k_means) <- paste0("K", 1:5)
nba_clusts <- cbind(nba_positions, k_means)
pca <- prcomp(stats_mat)
nba_clusts$PC1 <- pca$x[, 1]
nba_clusts$PC2 <- pca$x[, 2]

usethis::use_data(nba_clusts, overwrite = TRUE)
