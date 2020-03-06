#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   igraph_walkthrough.R
# Description:   Scratchpad
# Author:        Brandon Monier
# Created:       2019-02-25 at 15:43:12
# Last Modified: 2019-02-25 at 18:00:36
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to provide a simple
#    walkthrough for igraph construction
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load data
links <- readr::read_csv(
    file = "~/Development/storage/graph_data_tests/Dataset1-Media-Example-EDGES.csv"
)
nodes <- readr::read_csv(
    file = "~/Development/storage/graph_data_tests/Dataset1-Media-Example-NODES.csv"
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



