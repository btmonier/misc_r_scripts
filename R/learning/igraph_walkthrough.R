#!/usr/bin/env Rscript

#------------------------------------------------------------------------------
# Title:   igraph Walkthrough
# Author:  Brandon Monier
# Created: 2019-02-25
#
# Provide a simple walkthrough for igraph construction
#------------------------------------------------------------------------------

# === Preamble ======================================================

## Load data (set `data_dir` to the folder with the example CSV files)
data_dir <- "path/to/graph_data_tests"
links <- readr::read_csv(
    file = file.path(data_dir, "Dataset1-Media-Example-EDGES.csv")
)
nodes <- readr::read_csv(
    file = file.path(data_dir, "Dataset1-Media-Example-NODES.csv")
)


# === Graph construction ============================================

## Make graph object
net <- igraph::graph_from_data_frame(
    d = links,
    vertices = nodes,
    directed = TRUE
)

## Plot initial object
plot(
    net,
    edge.arrow.size = 0.4,
    vertex.label = NA
)

## Simplify graph object
net <- igraph::simplify(net, remove.multiple = FALSE, remove.loops = TRUE)
