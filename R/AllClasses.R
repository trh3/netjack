#' Format and print an argument list.
#'
#' Internal function for pretty printing argument lists.
#'
#' @param arg.list list of arguments to format.
#'
#' @return Formatted argument list
#' @export
#' @keywords internal
#'
pretty_print_arg_list = function(arg.list){
  temp = "\n"
  for(i in 1:(length(arg.list)-1)){
  temp = c(temp,paste("\t",names(arg.list)[i], " = ", arg.list[[i]], "\n", sep = ""))
  }
  return(c(temp,paste("\t",names(arg.list)[length(arg.list)], " = ", arg.list[[length(arg.list)]], sep = "")))
}

#' An S4 class to represent a single network.
#'
#'This class represents a single observation of a network, with associated node level variables.
#'
#'For constructor see: \code{\link{as_Net}}
#'
#' @slot net matrix. The network represented as an adjacency matrix.
#' @slot net.name character. The name of the network (e.g. subject ID, school name)
#' @slot node.variables list. A named list of node variables, in the same order as the adjacency matrix.
#'
#' @export
#'
setClass("Net",slots =c(
  net = "matrix",
  net.name = "character",
  node.variables = "list"))

#' An S4 class to represent a sample of networks
#'
#' This class represents a collection of networks, with associated network level variables.
#'
#' For constructor see: \code{\link{as_NetSample}}
#'
#' @slot nets list. A list of Net objects
#' @slot net.names character. A character vector representing the names of the net objects
#' @slot sample.variables list. A named list of network level variables.
#'
#' @export
#'
setClass("NetSample",
                      slots = c(
                        nets = "list",
                        net.names = "character",
                        sample.variables = "list"
                      ))


#' An S4 class representing a single network with a network permutation function applied to it.
#'
#' This class represents the results of applying a network permutation function, such as a jackknife, or rewiring algorithm, to a single network.
#'
#' For constructor see: For constructor see: \code{\link{net_apply}}
#'
#' @slot fun.name character. The name of the network permutation function applied
#' @slot fun function. The permutation function applied
#' @slot fun.args list. The arguments supplied to the permutation function
#' @slot orig.net Net. The original network
#' @slot orig.net.name character. The name of the original network
#' @slot nets list. A list of Net objects, each corresponding to a instance of the manipulated original network
#' @slot nets.names character. The names of the manipulated networks.
#' @slot node.variables list. Node variables of the original network
#' @slot iter logical. A flag to indicate that the permutation function was repeated with the same arguements. Currently unused.
#'
#' @export
#'
setClass("NetSet",slots =c(
         fun.name = "character",
         fun = "function",
         fun.args = "list",
         orig.net = "Net",
         orig.net.name = "character",
         nets = "list",
         nets.names = "character",
         node.variables = "list",
         iter = "logical"))


#' An S4 class representing a sample of networks with a network permutation function applied to it.
#'
#' This class represents the results of applying a network permutation function, such as a jackknife, or rewiring algorithm, to a sample of networks.
#'
#' @slot net.sets list. A list of NetSet objects, reach representing a network, and the results of applying the permutation function.
#' @slot net.names character. A character vector representing the names of the original network in the sample
#' @slot sample.variables list. A list representing sample level variables.
#'
#' @inheritParams NetSet
#' @export
#'
setClass("NetSampleSet",slots =c(
  fun.name = "character",
  fun = "function",
  fun.args = "list",
  net.sets = "list",
  net.names = "character",
  sample.variables = "list"
))




#'@rdname show-Net
setMethod("show", "NetSet",
          function(object){

            cat("Net Set","\n",
                "Applied Function: ", object@fun.name, "\n",
                "Function Arguments: ", pretty_print_arg_list(object@fun.args), "\n",
                "Original Network Name: ",object@orig.net.name, "\n",
                "Contains " ,length(object@nets), " networks.", sep = ""
                )
          })

#'@rdname show-Net
setMethod("names", "NetSet",
          function(x){
            return(x@nets.names)
          })



#' An S4 class representing the results of applying a network statistic function to a single NetSet object.
#'
#' This class represents the results of applying a network statistic function to a single NetSet object. This class contains the results for the original network, as well as for each instance of the permuted/manipulated networks.
#'
#' @slot stat.fun function. The network statistic function applied
#' @slot stat.fun.name character. The name of the network statistic function
#' @slot stat.fun.args list. Additional arguments the network statistic function took
#' @slot orig.net.name character. The name of the original network.
#' @slot orig.net.stat numeric. The value of the network statistic calculated on the original network.
#' @slot nets.stat list. A list of values of the network statistic applied to the manipulated networks
#' @slot nets.names character. Names of the manipulated networks.
#' @inheritParams NetSet
#' @export
#'
setClass("NetStatSet", slots =c(fun = "function",
         fun.name = "character",
         fun.args = "list",
         stat.fun = "function",
         stat.fun.name = "character",
         stat.fun.args = "list",
         orig.net.name = "character",
         orig.net.stat = "numeric",
         nets.stat = "list",
         nets.names = "character"))


#' An S4 class representing the results of applying a network statistic function to a  NetSampSet object.
#'
#' This class represents the results of applying a network statistic function to a NetSampSet object. This class contains the results for the original networks, as well as for each instance of the permuted/manipulated networks.
#'
#' @slot stat.fun function. The network statistic function applied
#' @slot stat.fun.name character. The name of the network statistic function
#' @slot stat.fun.args list. Additional arguments the network statistic function took
#' @slot orig.net.name character. The name of the original network.
#' @slot orig.net.stat numeric. The value of the network statistic calculated on the original network.
#' @slot nets.stat list. A list of values of the network statistic applied to the manipulated networks
#' @slot nets.names character. Names of the manipulated networks.
#' @slot sample.variables list. A list of sample level variables.
#' @inheritParams NetSet
#' @export
#'
setClass("NetSampleStatSet", slots =c(fun = "function",
                                              fun.name = "character",
                                              fun.args = "list",
                                              stat.fun = "function",
                                              stat.fun.name = "character",
                                              stat.fun.args = "list",
                                              nets.stat = "list",
                                              net.names = "character",
                                              sample.variables = "list"
                                              ))


#'@rdname show-Net
setMethod("show", "NetStatSet",
          function(object){
            cat("Net Statistic Set","\n",
                "Applied Function: ", object@fun.name, "\n",
                "Function Arguments: ", do.call(cat,list(object@fun.args, sep = "\n")), "\n",
                "Applied Statistic Function: ", object@stat.fun.name, "\n",
                "Statistic Function Arguments: ", object@stat.fun.args, "\n",
                "Original Network Name: ",object@orig.net.name, "\n"
            )
          })


#' Convenience Functions for printing information about Net-type objects
#'@param object,x Object to print
#'@name show-Net
#'
NULL



