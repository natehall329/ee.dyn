#' Extract Graph Properties from GIMME Analysis
#'
#' Extracts and computes graph properties from GIMME beta coefficients, allowing
#' the selection between contemporaneous and cross-lagged edges. The function can
#' handle outlier adjustment and offers options to retain the raw igraph objects
#' for further custom analyses. This functionality facilitates in-depth exploration
#' of the network characteristics derived from GIMME outputs, providing insights
#' into the dynamics of the system under study.
#'
#' @param betas A dataframe containing the beta coefficients from a GIMME analysis.
#' This dataframe should include, at a minimum, columns for the left-hand side
#' (lhs), the right-hand side (rhs), and the beta values.
#' @param lagged Logical indicating whether to create a graph from lagged paths
#' only (`TRUE`) or contemporaneous paths only (`FALSE`). Defaults to `FALSE`.
#' @param outliers An optional list specifying parameters for outlier adjustment
#' within the graph properties analysis. The list can include methods for outlier
#' handling and limits for identification, among other settings.
#' @param edge_values_from Character string specifying the source of edge values.
#' Options are "beta" for beta coefficients or "z" for z-scores. Default is "beta".
#' @param silent Logical indicating whether to suppress messages during function
#' execution. Defaults to `TRUE`.
#' @param keep_igraph Logical indicating whether to include the raw igraph objects
#' in the function's output. Defaults to `FALSE`.
#'
#' @return A list containing the extracted graph properties. If `keep_igraph` is
#' `TRUE`, the list includes three elements: `edges`, which contains the dataframe
#' of edges with potential outlier adjustments; `graph_metrics`, a list of dataframes
#' with graph metrics for each file; and `graphs`, a list of igraph objects for
#' each file. If `keep_igraph` is `FALSE`, the `graphs` element is omitted.
#'
#' @examples
#' # Assuming `betas` is a dataframe of beta coefficients from GIMME analysis
#' graph_props <- extract_gimme_graph_properties(betas, lagged = FALSE, keep_igraph = TRUE)
#'
#' @author Nate Hall
#' @export

extract_gimme_graph_properties <- function(betas,
                                           lagged = FALSE,  # TRUE creates a graph from lagged paths only
                                           outliers = NULL,
                                           edge_values_from = "beta", # beta or z. Stick with beta for now.
                                           # rm_subj_colmods = NULL,
                                           silent = TRUE,
                                           keep_igraph = FALSE
){
  # debug
  # -----
  # lagged = FALSE
  # outliers = list(method = "winsorize",
  #                 limit_class = "prob",
  #                 limits = c(.025, .975))
  # edge_values_from = "beta"
  # silent = FALSE
  # keep_igraph = TRUE
  # -----

  if(lagged) {
    assoc <- "cross-lagged"
    edges <- betas |> filter(grepl("lag", rhs), (lhs != sub("lag", "", rhs))) |> mutate(assoc = "cross_lagged", rhs = sub("lag", "", rhs)) ### remove the lag label so nodes are identified as the same
  } else{
    assoc <- "contemporaneous"
    edges <- betas |> filter(!grepl("lag", rhs)) |> mutate(assoc = "contemporaneous")
  }


  ## N.B. this includes cleaning outliers for group and subgroup edges based on specification setup in outliers.
  if(!silent) {cat("-------------------------\nExtracting GIMME coeffecients amongst", toupper(assoc), "associations\n-------------------------\n")}
  edf <- setup_edge_df(lagged = lagged,
                       edges = edges,
                       outliers = outliers,
                       edge_values_from = edge_values_from)

# browser()

  if(!silent) cat("-------------------------\nComputing graph metrics for", toupper(assoc), "associations\n-------------------------\n")
  ######## Generate igraph and calculate a few nodal centrality metrics. Done on a file-by-file basis
  sub_graphs <- list()
  sub_metrics <- list()
  for(fil in unique(edf$file)){
    if(!silent) cat("Generating graphs from file: ", fil, "\n")
    # fil <- 1000
    if("sub_membership" %in% colnames(betas)){
      subgroup <- betas |> filter(file == fil) |> select(sub_membership) |> distinct() |> pull(sub_membership)
    }
    f.betas <- edges |> filter(file == fil) |> rename(from = rhs,
                                                        to = lhs,
                                                        weight = beta) |> select(-op)

    # reorder name so from and to are accurately denoted as vertices
    name_reorder <- c("from", "to", colnames(f.betas)[which(!colnames(f.betas) %in% c("from", "to"))])
    g <- f.betas |> select(all_of(name_reorder)) |> igraph::graph_from_data_frame(directed = TRUE)

    # calculate vertex-level graph metrics: in-strength, out-strength
    igraph::V(g)$in_strength <- igraph::strength(g, mode = "in")
    igraph::V(g)$out_strength <- igraph::strength(g, mode = "out")
    # V(g)$betweenness <- betweenness(g, weights =ifelse(edge_attr(g)$weight < 0, 0, edge_attr(g)$weight), directed = TRUE) # conceptually fraught if there are negative edges. Can return to this later if there is a good reason to do so.

    #### N.B. can consider additional vertex, edge, or global graph attributes, though think carefully about their meaning in the context of your network.

    sub_graphs[[as.character(fil)]] <- g

    sub_metrics[[as.character(fil)]] <- bind_cols(igraph::vertex_attr(g)) |> mutate(file = fil, sub_membership = subgroup) |> select(file,name, sub_membership, in_strength, out_strength)
  }

  ### Set NAs edges to 0
  edf[is.na(edf)] <- 0

  if(keep_igraph){
    out <- list(edges = edf, graph_metrics = sub_metrics, graphs = sub_graphs)
  } else {
    out <- list(edges = edf, graph_metrics = sub_metrics)
  }

  return(out)
}
