#' Network Manipulation Functions
#'
#'@name network_functions
#'
NULL



#' Node level jackknife function
#'
#' Removes each node in turn from the Net, and returns a list of jackknifed Net objects
#'
#' @param Net Network to jackknife
#'
#' @return A list of Net objects
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

#' Network level jackknife function
#'
#' Removes each subnetwork in turn from the Net, and returns a list of jackknifed Net objects
#'
#' @param network.variable character name of node variable containing network labels
#' @return A list of Net objects
#' @export
#'
#'@rdname network_functions
network_jackknife = function(Net, network.variable){

  toReturn = list()

  networks = unlist(Net@node.variables[network.variable])
  uniNets = unique(names(table(networks)))
  print(uniNets)
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


#' Absolute Thresholding
#'
#' Applies an absolute thresholding to target network, returning a binary network.
#'
#' @param thresholds Vector of thresholds to use
#'
#' @return A list of Net objects, named at threshold level.
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

#' Relative Thresholding
#'
#' Applies a relative threshold based on target density to target network
#'
#' @param percentiles Vector of densities to threshold at
#'
#' @return List of net objects with named at the density threshold.
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


