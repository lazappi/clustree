% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{store_node_aes}
\alias{store_node_aes}
\title{Store node aesthetics}
\usage{
store_node_aes(graph, node_aes_list, metadata)
}
\arguments{
\item{graph}{graph to store attributes in}

\item{node_aes_list}{nested list containing node aesthetics}

\item{metadata}{data.frame containing metadata that can be used as aesthetics}
}
\value{
graph with additional attributes
}
\description{
Store the names of node attributes to use as aesthetics as graph attributes
}
\keyword{internal}
