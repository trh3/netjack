#' Network Statistic Functions
#'
#' These functions compute a variety of network statistics on single \code{Net} objects.
#'
#'@name network_statistics
#'
#'@references
#'\insertAllCited{}
#'
#'@examples
#' data(GroupA)
#' GroupA1_Net = as_Net(GroupA[[1]], "1", list(community = c(rep(1, 10), rep(2,10))))
#' global_efficiency(GroupA1_Net)
#' modularity(GroupA1_Net, "community")
NULL



#' \code{global_efficiency} - Computes global efficiency. Uses brainGraph implementation \insertCite{brainGraph}{netjack}. Global efficiency originally derived by \insertCite{globEff;textual}{netjack}
#'
#' @param Net Input \code{Net} object
#'
#' @return Network statistic value
#' @export
#'
#' @rdname network_statistics
global_efficiency <- function(Net){

  net <- igraph::graph.adjacency(Net@net, mode = "undirected", weighted = T, diag = F)
  globEff <- brainGraph::efficiency(net,type="global")
    return(globEff)
}

#' \code{modularity} - Computes modularity with a given community partition. Uses \code{igraph} implementation \insertCite{igraph}{netjack}. Modularity originally derived by \insertCite{mod;textual}{netjack}
#'
#' @param community.variable character name of the node variable that represents the partition.
#'
#' @export
#'
#' @rdname network_statistics
modularity <- function(Net, community.variable){

  net = igraph::graph.adjacency(Net@net, mode = "undirected", weighted = T, diag = F)
  mod = igraph::modularity(net, as.numeric(as.factor(Net@node.variables[[community.variable]])), weights = igraph::E(net)$weight)
  return(mod)
}


