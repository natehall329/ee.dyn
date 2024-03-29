% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gimme_graph_attrs.R
\name{get_gimme_graph_attrs}
\alias{get_gimme_graph_attrs}
\title{Extract Graph Attributes from GIMME Output}
\usage{
get_gimme_graph_attrs(
  betas,
  extract_attributes = "all",
  outliers = NULL,
  keep_igraph = TRUE
)
}
\arguments{
\item{betas}{A matrix or data frame of betas resulting from a GIMME analysis.
These betas represent the coefficients for the edges in the graph.}

\item{outliers}{A list specifying the method and parameters for outlier
adjustment. The default method is "winsorize" with limits set to the 2.5th
and 97.5th percentiles, and grouping by subject membership.}

\item{keep_igraph}{A logical indicating whether to keep the igraph object in
the output. Default is TRUE.}

\item{attributes}{A character string specifying which graph attributes to
extract. Options are "all", "ar1" for self-edges, "contemporaneous" for
contemporaneous relationships, and "cross_lagged" for cross-lagged
relationships. Default is "all".}
}
\value{
A list containing the extracted graph attributes based on the specified
criteria. Each attribute is stored in its respective named element within the
list.
}
\description{
This function extracts EE parameters (betas) of interest from raw GIMME. It can extract
contemporaneous (lag-0) and cross-lagged relationships, as well as self-edges (i.e.,
the AR1 component of each nodal time series). It allows for the extraction and storage
of different graph properties by disentangling these components. Additionally,
outlier adjustment is supported through various methods, such as winsorizing.
}
\examples{
# Assuming `betas` is a matrix of beta coefficients from GIMME
graph_attrs <- get_gimme_graph_attrs(betas)
graph_attrs_ar1 <- get_gimme_graph_attrs(betas, attributes = "ar1")

}
\author{
Nate Hall
}
