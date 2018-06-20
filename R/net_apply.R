
#' Constructor for single Net object
#'
#' This function takes a single network, as an adjacency matrix, and returns a
#' \code{Net} object.
#'
#' @param matrix Network represented as an adjacency matrix
#' @param net.name Name of the network (optional)
#' @param node.variables Node level variables (optional)
#'
#' @return A \code{Net} object
#' @export
#'
as_Net <- function(matrix, net.name, node.variables){
  if(missing(node.variables)){
    node.variables = list(index = 1:dim(matrix)[[1]])
  }else{
    if(class(node.variables) == "data.frame"){
      node.variables = as.list(node.variables)
    }
    node.variables = c(list(index = 1:dim(matrix)[[1]]), node.variables)
  }
  if(missing(net.name)){
    net.name = ""
  }
  return(methods::new("Net", net =matrix, net.name=net.name, node.variables = node.variables))
}

#' Constructor for a \code{NetSample} object
#'
#' This function takes a list of adjacency matrices, and returns a \code{NetSample}
#' object.
#'
#' @param matrixList A list of adjacency matrices
#' @param net.names A character vector of network names
#' @param node.variables A list of node level variables to be associated with
#'   every network in the sample.
#' @param sample.variables A list of network level variables.
#'
#' @return A \code{NetSample} instance.
#' @export
#'
as_NetSample <- function(matrixList, net.names, node.variables, sample.variables){
  temp = list()
  if(missing(sample.variables)){
    sample.variables = list()
  }
  if(class(sample.variables) == "data.frame"){
    sample.variables = as.list(sample.variables)
  }
    sample.variables$orig.net = net.names
  for(i in 1:length(matrixList)){
    temp[[i]] = as_Net(matrixList[[i]], net.names[i], node.variables)
  }
  toReturn = methods::new("NetSample", nets = temp, net.names = net.names, sample.variables = sample.variables)
  return(toReturn)
}

#' Apply a network manipulation function to a single network, or to a sample of
#' networks
#'
#' This function applies a network manipulation function to a single network or
#' sample of networks, and returns a NetSet, or NetSampleSet containing the
#' results.
#'
#' @param network An \code{Net} object or a \code{NetSample} object
#' @param net.function A network manipulation function (reference or character)
#' @param net.function.args A labeled list containing arguments to the
#'   net.function
#' @param orig.net.name The original network name, when applying net_apply to a
#'   \code{Net}
#'
#' @return A \code{NetSet} or \code{NetSampleSet} object
#' @export
#'
setGeneric("net_apply", function(network, net.function, net.function.args, orig.net.name) {
  standardGeneric("net_apply")
})


#' @describeIn net_apply net_apply for Net
setMethod("net_apply", signature = c(network = "Net", net.function = "ANY",
                                     net.function.args = "ANY",
                                     orig.net.name = "ANY"),
          function(network, net.function, net.function.args, orig.net.name){
            net.function.true = match.fun(net.function)
            if(missing(net.function.args)){
              net.function.args = list()
              results = do.call(net.function.true, list(network))
            }else{
              results = do.call(net.function.true, c(list(network), net.function.args))
            }
            if(missing(orig.net.name)){
              orig.net.name = deparse(substitute(network))
            }

            toReturn = methods::new("NetSet",fun.name = deparse(substitute(net.function)),
                           fun = net.function.true,
                           fun.args = net.function.args,
                           orig.net = network,
                           orig.net.name = orig.net.name,
                           nets = results,
                           nets.names = names(results),
                           iter = FALSE)
            return(toReturn)
          })


#' @describeIn net_apply net_apply for NetSample
setMethod("net_apply", signature = c(network = "NetSample", net.function = "ANY",
                                     net.function.args = "ANY",
                                     orig.net.name = "missing"),
          function(network, net.function, net.function.args, orig.net.name){
            if(missing(net.function.args)){
              net.function.args = list()
            }
            net.function.true = match.fun(net.function)
            results = mapply(FUN = function(x, name){
              return(net_apply(x, net.function = net.function, net.function.args = net.function.args,
                               orig.net.name = name))
              },
              network@nets, network@net.names)

            toReturn = methods::new("NetSampleSet",
                           fun.name = deparse(substitute(net.function)),
                           fun = net.function.true,
                           fun.args = net.function.args,
                           net.sets = results,
                           net.names = network@net.names,
                           sample.variables = network@sample.variables)

            return(toReturn)
          })

#'Apply a network statistic function to a \code{NetSet} or \code{NetSampleSet} object.
#'
#'This function applies a network statistic function to a \code{NetSet} or \code{NetSampleSet}
#'object, and returns the calculated network statistics.
#'
#'@param netSet A \code{NetSet} or \code{NetSampleSet} object.
#'@param net.stat.fun The network statistic function
#'@param net.stat.fun.args A list of additional arguments to the network
#'  statistic function
#'@param net.stat.name A descriptive name for the network statistic (defaults to
#'  deparsed name of statistic function)
#'
#'@return A \code{NetStatSet} or \code{NetSampleStatSet}
#'@export
#'
setGeneric("net_stat_apply", function(netSet, net.stat.fun, net.stat.fun.args, net.stat.name) {
  standardGeneric("net_stat_apply")
})

#' @describeIn net_stat_apply net_stat_apply for NetSet
setMethod("net_stat_apply", signature = c(netSet = "NetSet", net.stat.fun = "ANY",
                                          net.stat.fun.args = "ANY", net.stat.name = "ANY"),
          function(netSet, net.stat.fun, net.stat.fun.args, net.stat.name){
            if(missing(net.stat.name)){
              net.stat.name = deparse(substitute(net.stat.fun))
            }
            net.stat.fun = match.fun(net.stat.fun)
            if(missing(net.stat.fun.args)){
              net.stat.fun.args = list()
            }
            orig.stat = do.call(net.stat.fun, c(net.stat.fun.args, netSet@orig.net))
            nets.stat = lapply(netSet@nets, function(x){
              return(do.call(net.stat.fun, c(net.stat.fun.args, x)))
            })
            toReturn = methods::new("NetStatSet",fun = netSet@fun,
                           fun.name = netSet@fun.name,
                           fun.args = netSet@fun.args,
                           stat.fun = net.stat.fun,
                           stat.fun.name = net.stat.name,
                           stat.fun.args = net.stat.fun.args,
                           orig.net.name = netSet@orig.net.name,
                           orig.net.stat = orig.stat,
                           nets.stat = nets.stat,
                           nets.names = netSet@nets.names)
            return(toReturn)
          })


#' @describeIn net_stat_apply Converter for NetSampleSet
setMethod("net_stat_apply", signature = c(netSet = "NetSampleSet", net.stat.fun = "ANY",
                                          net.stat.fun.args = "ANY", net.stat.name = "ANY"),
          function(netSet, net.stat.fun, net.stat.fun.args, net.stat.name){
             if(missing(net.stat.name)){
              net.stat.name = deparse(substitute(net.stat.fun))
            }
            net.stat.fun.name = deparse(substitute(net.stat.fun))
            net.stat.fun = match.fun(net.stat.fun)
            if(missing(net.stat.fun.args)){
              net.stat.fun.args = list()
            }
            nets.stat = lapply(netSet@net.sets, FUN = net_stat_apply, net.stat.fun = net.stat.fun,
                               net.stat.fun.args = net.stat.fun.args, net.stat.name = net.stat.name)

            toReturn = methods::new("NetSampleStatSet",fun = netSet@fun,
                           fun.name = netSet@fun.name,
                           fun.args = netSet@fun.args,
                           stat.fun = net.stat.fun,
                           stat.fun.name = net.stat.name,
                           stat.fun.args = net.stat.fun.args,
                           nets.stat = nets.stat,
                           net.names = netSet@net.names,
                           sample.variables = netSet@sample.variables)
            return(toReturn)
          })

#' Network statistics to long format dataframe
#'
#' This function converts a \code{NetStatSet} or \code{NetSampleStatSet} into a long format
#' dataframe
#' @param netStatSet A \code{NetStatSet} or \code{NetSampleStatSet} object
#'
#' @return A long format dataframe containing the name of the original network,
#'   the original network network statistic, the name of the manipulated
#'   network, the manipulated network network statistic and the name of the
#'   network statistic.
#' @export
#'
setGeneric("to_data_frame", function(netStatSet) {
  standardGeneric("to_data_frame")
})

#' @describeIn to_data_frame Converter for NetSampleStatSet
setMethod("to_data_frame", signature = c(netStatSet = "NetStatSet"),
          function(netStatSet){
            toReturn = data.frame(orig.net = netStatSet@orig.net.name,
                                  orig.stat = netStatSet@orig.net.stat,
                                  net.names = netStatSet@nets.names,
                                  nets.stat = as.numeric(netStatSet@nets.stat),
                                  stat.name = netStatSet@stat.fun.name)
            return(toReturn)
          })

#' @describeIn to_data_frame Converter for NetSampleStatSet
setMethod("to_data_frame", signature = c(netStatSet = "NetSampleStatSet"),
          function(netStatSet){
            toReturn = lapply(netStatSet@nets.stat, to_data_frame)
            toReturn = do.call("rbind", toReturn)

            if(length(netStatSet@sample.variables)> 0){
              temp = as.data.frame(netStatSet@sample.variables)
              toReturn = merge(toReturn, temp, by = "orig.net")
            }
            return(toReturn)
          })


#' Test for differences from original statistic
#'
#' This function tests for significant differences from the original network
#' statistic as a result of the network manipulation. If non-parametric is
#' chosen, this is done using the Wilcox test, otherwise, Welch's t-test.
#'
#' @param netSampleStatSet Input \code{NetSampleStatSet}
#' @param p.adjust character string for requested multiple comparisons
#'   adjustment. Defaults to Benjamani-Hochberg
#' @param non.parametric Logical. if true, test is performed using Wilcox test.
#'   If false, Welch's t-test. Defaults to false.
#'
#' @return A data frame containing original and adjusted p.values, as well as
#'   differences, labeled with manipulation name.
#' @export
#'
diff_test = function(netSampleStatSet, p.adjust = "BH", non.parametric = F){

  toPlot = to_data_frame(netSampleStatSet)

  net.names = names(table(toPlot$net.names))

  results = lapply(net.names,FUN = function(name, toPlot){
    sub = toPlot[which(toPlot$net.names == name),]
    subDiff = sub$nets.stat-sub$orig.stat

    if(non.parametric){
      diffTest = stats::wilcox.test(sub$nets.stats, sub$orig.stat, paired = T)
    }else{
      diffTest = stats::t.test(subDiff)
    }
    return(data.frame(net.names = name,diff = diffTest$estimate, p = diffTest$p.value))
  }, toPlot = toPlot)

  results <- as.data.frame(do.call("rbind", results))
  results$adjusted.p = stats::p.adjust(results$p, method = p.adjust)
  return(results)
}

#' Group difference test
#'
#' This function implements the group difference test on a network statistic.
#' This test assesses if the change in the network statistic due to the network
#' manipulation is significantly different between groups.
#'
#' If the sample has 2 groups, this test is performed using Welch's t-test or
#' Wilcox test. If the sample has 3 or more groups, the test is performed using
#' a 1-way ANOVA, or Kruskal-Wallis test. Differences are tested at each network
#' manipulation.
#'
#' @param netSampleStatSet Input \code{NetSampleStatSet}
#' @param grouping.variable character name of sample level grouping variable
#' @param p.adjust character string for requested multiple comparisons
#'   adjustment. Defaults to Benjamani-Hochberg
#' @param non.parametric Logical. if true, test is performed using Wilcox test.
#'   If false, Welch's t-test. Defaults to false.
#'
#' @return A data frame containing original and adjusted p.values.
#' @export
#'
 group_diff_test = function(netSampleStatSet,grouping.variable, p.adjust = "BY", non.parametric = F){

  toPlot = to_data_frame(netSampleStatSet)

  net.names = names(table(toPlot$net.names))
  form = paste0("subDiff", "~", grouping.variable)
  results = lapply(net.names,FUN = function(name, toPlot){
    sub = toPlot[which(toPlot$net.names == name),]
    sub$subDiff = sub$nets.stat-sub$orig.stat
    form = stats::as.formula(form)
    if(non.parametric){
      groupTest <- stats::kruskal.test(form, data = sub)
      return(data.frame(net.names = name, p = groupTest$p.value))
    }else{
      groupTest <- stats::anova(stats::lm(form, data = sub))
      return(data.frame(net.names = name, p = groupTest$"Pr(>F)"[1]))
    }
  }, toPlot = toPlot)

  results <- as.data.frame(do.call("rbind", results))
  results$adjusted.p = stats::p.adjust(results$p, method = p.adjust)
  return(results)
}

 #' Group test
 #'
 #' This function implements the group test on a network statistic. This test
 #' assesses if the network statistic is significantly different between groups,
 #' at each network manipulation.
 #'
 #' If the sample has 2 groups, this test is performed using Welch's t-test or
 #' Wilcox test. If the sample has 3 or more groups, the test is performed using
 #' a 1-way ANOVA, or Kruskal-Wallis test. Differences are tested at each
 #' network manipulation.
 #'
 #' @param netSampleStatSet Input \code{NetSampleStatSet}
 #' @param grouping.variable character name of sample level grouping variable
 #' @param p.adjust character string for requested multiple comparisons
 #'   adjustment. Defaults to Benjamani-Hochberg
 #' @param non.parametric Logical. if true, test is performed using Wilcox test.
 #'   If false, Welch's t-test. Defaults to false.
 #'
 #' @return A data frame containing original and adjusted p.values.
 #' @export
group_test = function(netSampleStatSet, grouping.variable, p.adjust = "BY", non.parametric = F){
  toPlot <- to_data_frame(netSampleStatSet)
  form = paste0("nets.stat", "~", grouping.variable)
  form = stats::as.formula(form)
  net.names = names(table(toPlot$net.names))
  results = lapply(net.names,FUN = function(name, toPlot){

    sub = toPlot[which(toPlot$net.names == name),]
    if(non.parametric){
      groupTest <- stats::kruskal.test(form, sub)
      return(data.frame(net.names = name, p = groupTest$p.value))
    }else{
      groupTest <- stats::anova(stats::lm(form, data = sub))
      return(data.frame(net.names = name, p = groupTest$"Pr(>F)"[1]))
    }
  }, toPlot = toPlot)

  results <- as.data.frame(do.call("rbind", results))
  results["adjusted.p"] = stats::p.adjust(results$p, method = p.adjust)
  return(results)
}



#' Difference Test Plots
#'
#' This function performs the difference test and generates a ggplot object representing the results.
#'
#' @inheritParams diff_test
#' @param labels ggplot2 labs object. Labels for the plot
#' @param sort one of "alpha", "mean", "median". "alpha" sorts in alpha numeric order, while mean and median sort by decreasing values.
#' @param p.threshold Numeric. Threshold by which to highlight results. Defaults to .05
#' @param hide.non.sig Logical. If true, non significant (as defined by p.threshold) are not plotted.
#'
#' @return A ggplot object
#' @export
#'
net_ggPlot = function(netSampleStatSet, labels, sort = "alpha", p.threshold = .05, p.adjust = "BY", hide.non.sig = F,non.parametric = F){

toPlot = to_data_frame(netSampleStatSet)
  testtoPlot <- diff_test(netSampleStatSet, p.adjust = p.adjust, non.parametric = non.parametric)
  toPlot = merge(toPlot, testtoPlot, by = "net.names")
  toPlot$diff = toPlot$nets.stat - toPlot$orig.stat
  if(sort == "alpha"){
    if(!any(is.na(as.numeric(toPlot$net.names)))){
        toPlot$net.names = factor(toPlot$net.names, level = sort(unique(as.numeric(levels(toPlot$net.names)[as.numeric(toPlot$net.names)]))))
      }
  }
  if(sort == "mean"){
    agg <- stats::aggregate(toPlot$diff, by = toPlot["net.names"], mean, na.rm = T)
    ord = order(agg$x)
    toPlot$net.names = factor(toPlot$net.names, level = agg$net.names[ord])
  }

  if(sort == "median"){
    agg <- stats::aggregate(toPlot$diff, by = toPlot["net.names"], stats::median, na.rm = T)
    ord = order(agg$x)
    toPlot$net.names = factor(toPlot$net.names, level = agg$net.names[ord])
  }

  if(!hide.non.sig){
    toPlot$p.thres = T
  }else{
    toPlot$p.thres = FALSE
    toPlot$p.thres[which(toPlot$adjusted.p < p.threshold)] = TRUE
  }

  p = ggplot2::ggplot(toPlot[toPlot$p.thres,], ggplot2::aes(x = as.factor(net.names), y = diff, color = (adjusted.p < p.threshold)))+
    ggplot2::geom_boxplot() + ggplot2::coord_flip() + ggplot2::scale_color_manual(values = c("FALSE" ="black", "TRUE" ="red"),name = "Significant\nDifferences", labels = c(paste0("p > ", p.threshold),
                                                                                                       paste0("p < ", p.threshold))) +ggplot2::theme_classic()

  if(missing(labels)){
    p = p+ ggplot2::labs(x = "Change Name", y = paste0("Difference from Original ", toPlot$stat.name[1]))
  }else{
    p = p + labels
  }
  return(p)

}

#' Group Test Plots
#'
#' This function performs the group test and generates a ggplot object representing the results.
#'
#' @inheritParams group_test
#' @param labels ggplot2 labs object. Labels for the plot
#' @param sort one of "alpha", "mag"; "alpha" sorts in alpha numeric order, while "mag" sorts in order of decreasing effect size
#' @param p.threshold Numeric. Threshold by which to highlight results. Defaults to .05
#' @param hide.non.sig Logical. If true, non significant (as defined by p.threshold) are not plotted.
#'
#' @return A ggplot object
#' @export
#'
netGroup_ggPlot = function(netSampleStatSet, grouping.variable, labels, sort = "alpha", p.threshold = .05, p.adjust = "BY", hide.non.sig = F, non.parametric = F){

  toPlot = to_data_frame(netSampleStatSet)
  testtoPlot <- group_test(netSampleStatSet,grouping.variable = grouping.variable, p.adjust = p.adjust, non.parametric = non.parametric)
  toPlot = merge(toPlot, testtoPlot, by = "net.names")

  if(sort == "alpha"){
    if(!any(is.na(as.numeric(toPlot$net.names)))){
        toPlot$net.names = factor(toPlot$net.names, level = sort(unique(as.numeric(levels(toPlot$net.names)[as.numeric(toPlot$net.names)]))))
      }
  }
  if(sort == "mag"){
    agg <- stats::aggregate(toPlot$adjusted.p, by = toPlot["net.names"], mean, na.rm = T)
    ord = order(agg$x, decreasing = T)
    toPlot$net.names = factor(toPlot$net.names, level = agg$net.names[ord])
  }

  if(!hide.non.sig){
    toPlot$p.thres = T
  }else{
    toPlot$p.thres = FALSE
    toPlot$p.thres[which(toPlot$adjusted.p < p.threshold)] = TRUE
  }
  group = as.factor(toPlot[,grouping.variable])

  p = ggplot2::ggplot(toPlot[toPlot$p.thres,], ggplot2::aes(x = as.factor(net.names), y = nets.stat, color = (adjusted.p < p.threshold), fill = group))+
    ggplot2::geom_boxplot() + ggplot2::coord_flip() + ggplot2::scale_color_manual(values = c("FALSE" ="black", "TRUE" ="red"),name = "Significant\nDifferences", labels = c(paste0("p > ", p.threshold),
                                                                                                       paste0("p < ", p.threshold)))+
    ggplot2::scale_fill_discrete(name = "Group")+ggplot2::theme_classic()

  if(missing(labels)){
    p = p+ ggplot2::labs(x = "Change Name", y = paste0(toPlot$stat.name[1]))
  }else{
    p = p + labels
  }
  return(p)
}

#' Group Difference Plots
#'
#' This function performs the group difference test and generates a ggplot object representing the results.
#'
#' @inheritParams group_diff_test
#' @param labels ggplot2 labs object. Labels for the plot
#' @param sort one of "alpha", "mag"; "alpha" sorts in alpha numeric order, while "mag" sorts in order of decreasing effect size
#' @param p.threshold Numeric. Threshold by which to highlight results. Defaults to .05
#' @param hide.non.sig Logical. If true, non significant (as defined by p.threshold) are not plotted.
#'
#' @return A ggplot object
#' @export
netGroupDiff_ggPlot = function(netSampleStatSet, grouping.variable, labels, sort = "alpha", p.threshold = .05, p.adjust = "BY", hide.non.sig = F, non.parametric = F){

  toPlot = to_data_frame(netSampleStatSet)
  testtoPlot <- group_diff_test(netSampleStatSet,grouping.variable = grouping.variable, p.adjust = p.adjust, non.parametric = non.parametric)
  toPlot = merge(toPlot, testtoPlot, by = "net.names")

  if(sort == "alpha"){
    if(!any(is.na(as.numeric(toPlot$net.names)))){
      toPlot$net.names = factor(toPlot$net.names, level = sort(unique(as.numeric(levels(toPlot$net.names)[as.numeric(toPlot$net.names)]))))
    }
  }
  if(sort == "mag"){
    agg <- stats::aggregate(toPlot$adjusted.p, by = toPlot["net.names"], mean, na.rm = T)
    ord = order(agg$x, decreasing = T)
    toPlot$net.names = factor(toPlot$net.names, level = agg$net.names[ord])
  }

  if(!hide.non.sig){
    toPlot$p.thres = T
  }else{
    toPlot$p.thres = FALSE
    toPlot$p.thres[which(toPlot$adjusted.p < p.threshold)] = TRUE
  }
  group = as.factor(toPlot[,grouping.variable])
  toPlot$diff = toPlot$nets.stat - toPlot$orig.stat
  p = ggplot2::ggplot(toPlot[toPlot$p.thres,], ggplot2::aes(x = as.factor(net.names), y = diff, color = (adjusted.p < p.threshold), fill = group))+
    ggplot2::geom_boxplot() + ggplot2::coord_flip() + ggplot2::scale_color_manual(values = c("FALSE" ="black", "TRUE" ="red"),name = "Significant\nDifferences", labels = c(paste0("p > ", p.threshold),
                                                                                                                                                 paste0("p < ", p.threshold)))+
    ggplot2::scale_fill_discrete(name = "Group")+ggplot2::theme_classic()

  if(missing(labels)){
    p = p+ ggplot2::labs(x = "Change Name", y = paste0("Difference from Original ", toPlot$stat.name[1]))
  }else{
    p = p + labels
  }
  return(p)
}

