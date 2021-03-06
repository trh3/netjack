% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_apply.R
\name{group_diff_test}
\alias{group_diff_test}
\title{Group difference test}
\usage{
group_diff_test(netSampleStatSet, grouping.variable, p.adjust = "BH",
  non.parametric = F)
}
\arguments{
\item{netSampleStatSet}{Input \code{NetSampleStatSet}}

\item{grouping.variable}{character name of sample level grouping variable}

\item{p.adjust}{character string for requested multiple comparisons
adjustment. Defaults to Benjamani-Hochberg}

\item{non.parametric}{Logical. if true, test is performed using Wilcox test.
If false, t-test. Defaults to false.}
}
\value{
A data frame containing original and adjusted p.values.
}
\description{
This function implements the group difference test on a network statistic.
This test assesses if the change in the network statistic due to the network
manipulation is significantly different between groups.
}
\details{
If the sample has 2 groups, this test is performed using a t-test or
Wilcox test. If the sample has 3 or more groups, the test is performed using
a 1-way ANOVA, or Kruskal-Wallis test. Differences are tested at each network
manipulation.
}
\examples{
data(GroupA)
GroupA_Net = as_NetSample(GroupA, 1:20, node.variables = list(community = c(rep(1, 10), rep(2,10))),
  sample.variables = list(group = c(rep(1, 10), rep(2,10))))
Jackknife_GroupA_Net = net_apply(GroupA_Net, node_jackknife)
GlobEff_GroupA_Net = net_stat_apply(Jackknife_GroupA_Net, global_efficiency)
group_diff_test(GlobEff_GroupA_Net, grouping.variable = "group")
}
