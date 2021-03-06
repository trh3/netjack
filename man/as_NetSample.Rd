% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_apply.R
\name{as_NetSample}
\alias{as_NetSample}
\title{Constructor for a \code{NetSample} object}
\usage{
as_NetSample(matrixList, net.names, node.variables, sample.variables)
}
\arguments{
\item{matrixList}{A list of adjacency matrices}

\item{net.names}{A character vector of network names}

\item{node.variables}{A list of node level variables to be associated with
every network in the sample.}

\item{sample.variables}{A list of network level variables.}
}
\value{
A \code{NetSample} instance.
}
\description{
This function takes a list of adjacency matrices, and returns a \code{NetSample}
object.
}
\examples{
data(GroupA)
GroupA_Net = as_NetSample(GroupA, 1:20, node.variables = list(community = c(rep(1, 10), rep(2,10))),
 sample.variables = list(group = c(rep(1, 10), rep(2,10))))
}
