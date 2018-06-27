#' Network Manipulation Functions
#'
#'These functions take a \code{Net} object, manipulate the network in some way, and return a \code{list} of modified \code{Net} objects.
#'
#'@name network_functions
#' @examples
#' data(GroupA)
#' GroupA1_Net = as_Net(GroupA[[1]], "1", list(community = c(rep(1, 10), rep(2,10))))
#' node_jackknife(GroupA1_Net)
#' network_jackknife(GroupA1_Net, "community")
NULL



#'
#'
#'\code{node_jacknife}. Removes each node in turn from the \code{Net}, and returns a list of jackknifed \code{Net} objects.
#'
#' @param Net Network to jackknife
#'
#' @return A \code{list} of \code{Net} objects
#' @export
#'
#'@rdname network_functions
node_jackknife = function(Net){

  toReturn = list()

  for(i in 1:dim(Net@net)[[1]]){
    toReturn[[i]] = methods::new("Net", net = Net@net[-i,-i],
                        net.name = Net@net.name,
                        node.variables = lapply(Net@node.variables, function(x, i){
                          return(x[-i])
                        }, i = i))
  }
  names(toReturn) = as.character(1:dim(Net@net)[[1]])
  return(toReturn)
}

#' \code{network_jackknife} - Removes each subnetwork in turn from the \code{Net}, and returns a list of jackknifed \code{Net} objects
#'
#' @param network.variable Character name of node variable containing network labels
#' @export
#'
#'@rdname network_functions
network_jackknife = function(Net, network.variable){

  toReturn = list()

  networks = unlist(Net@node.variables[network.variable])
  uniNets = unique(names(table(networks)))
  for(i in 1:length(uniNets)){
    toReturn[[i]] = methods::new("Net", net = Net@net[-which(networks == uniNets[i]),-which(networks == uniNets[i])],
                        net.name = Net@net.name,
                        node.variables = lapply(Net@node.variables, function(x, i){
                          return(x[-i])
                        }, i = which(networks == uniNets[i])))
  }
  names(toReturn) = as.character(uniNets)
  return(toReturn)
}


#' \code{absolute_threshold} - Applies a series of absolute thresholding to target network, returning a binary network.
#'
#' @param thresholds Vector of thresholds to use
#'
#' @export
#'
#'@rdname network_functions
absolute_threshold = function(Net, thresholds){

  toReturn = list()


  toReturn = lapply(thresholds, function(x, network){
    print(x)
    temp = (Net@net > x)*1
    return(methods::new("Net", net = temp, net.name = Net@net.name, node.variables = Net@node.variables))
  }, network = Net@net)
  names(toReturn) = as.character(thresholds)
  return(toReturn)
}


#' \code{relative_threshold} -  Applies a relative threshold based on target density to target network
#'
#' @param percentiles Vector of densities to threshold at
#'
#' @export
#'
#' @rdname network_functions
relative_threshold = function(Net, percentiles){

  toReturn = list()
  toggle = isSymmetric(Net@net)
  tempOrg = Net@net
  diag(tempOrg) = NA
  toReturn = lapply(percentiles, function(x, network){
    if(toggle){
    target = stats::quantile(as.numeric(network[upper.tri(network)]), probs = x)
    }else{
      target = stats::quantile(as.numeric(network), probs = x, na.rm = T)
    }
    temp = (network > target)*1
    return(methods::new("Net", net = temp, net.name = Net@net.name, node.variables = Net@node.variables))
  }, network = tempOrg)
  names(toReturn) = as.character(percentiles)
  return(toReturn)
}


