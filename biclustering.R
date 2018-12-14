#------------------------------------------------------------------------------
# Title:  Biclustering Examples
# Author: Brandon Monier (brandon.monier@sdstate.edu)
# Date:   12.01.17
#------------------------------------------------------------------------------

# Preamble ----

## Load packages
require(QUBIC)
require(pheatmap)

## Make matrix - binary
samp2 <- sample(x = c(1, -1), size = 2500, replace = TRUE)
mat2 <- matrix(data = samp2, nrow = 50, ncol = 50)

## Make matrix - pentary
samp5 <- sample(x = c(1, 0.5, 0, -0.5, -1), size = 2500, replace = TRUE)
mat5 <- matrix(data = samp5, nrow = 50, ncol = 50) 


# Visualize ----

## No clustering (Binary)
colors2 <- c("#ffffff", "#333333")
clust.1 <- pheatmap(
  main = paste0(
    "Test matrix (", nrow(mat), " x ", ncol(mat), ") - no clustering"
  ),
  mat = mat2, 
  color = colors2, 
  cluster_rows = FALSE, 
  cluster_cols = FALSE,
  legend = FALSE,
  border_color = NA,
  treeheight_row = 0,
  treeheight_col = 0
)

## Clustering (Binary)
clust.2 <- pheatmap(
  main = paste0(
    "Test matrix (", nrow(mat), " x ", ncol(mat), ") - clustering"
  ),
  mat = mat2, 
  color = colors2, 
  cluster_rows = TRUE, 
  cluster_cols = TRUE,
  legend = FALSE,
  border_color = NA, 
  treeheight_row = 0, 
  treeheight_col = 0
)

## No clustering (Pentary)
colors5 <- c("#f7f7f7","#cccccc", "#969696", "#636363", "#252525")
clust.3 <- pheatmap(
  main = paste0(
    "Test matrix (", nrow(mat), " x ", ncol(mat), ") - no clustering"
  ),
  mat = mat5, 
  color = colors5, 
  cluster_rows = FALSE, 
  cluster_cols = FALSE,
  legend = FALSE,
  border_color = NA,
  treeheight_row = 0,
  treeheight_col = 0
)

## Clustering (Pentary)
clust.4 <- pheatmap(
  main = paste0(
    "Test matrix (", nrow(mat), " x ", ncol(mat), ") - clustering"
  ),
  mat = mat5, 
  color = colors5, 
  cluster_rows = TRUE, 
  cluster_cols = TRUE,
  legend = FALSE,
  border_color = NA, 
  treeheight_row = 0, 
  treeheight_col = 0
)
