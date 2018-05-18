#' Network Manipulation Functions
#'
#'@name network_statistics
#'
NULL


#' Computes global efficiency
#'
#' @param Net Input Net
#'
#' @return global efficiency value
#' @export
#'
#' @rdname network_statistics
global_efficiency <- function(Net){

  net <- igraph::graph.adjacency(Net@net, mode = "undirected", weighted = T, diag = F)
  globEff <- brainGraph::efficiency(net,type="global")
    return(globEff)
}

#' Computes modularity with a given partition
#'
#' @param community.variable character name of the node variable that represents the partition.
#'
#' @return modularity value
#' @export
#'
#' @rdname network_statistics
modularity <- function(Net, community.variable){

  net = igraph::graph.adjacency(Net@net, mode = "undirected", weighted = T, diag = F)
  mod = igraph::modularity(net, as.numeric(as.factor(Net@node.variables[[community.variable]])), weights = igraph::E(net)$weight)
  return(mod)
}


