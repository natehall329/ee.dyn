#' Setup Edge Dataframe for Graph Analysis
#'
#' Prepares an edge dataframe for graph analysis by organizing edges based on
#' their level (group, individual, or subgroup) and handling outliers as specified.
#' This function is a crucial step in processing edges for network analysis,
#' ensuring that data is appropriately structured and cleaned for subsequent
#' graph metrics computation. The flexibility in handling outliers and the option
#' to retain cleaned data are key features of this function.
#'
#' @param lagged Logical indicating whether the edges are lagged (`TRUE`) or
#' contemporaneous (`FALSE`). This parameter primarily affects internal processing
#' and labeling within the function.
#' @param edges A dataframe of edges to be processed. This should include
#' columns for file identifiers, subgroup membership (if any), and edge
#' coefficients, among possibly others.
#' @param outliers An optional list specifying parameters for outlier adjustment
#' within the edge dataframe. This list can include methods for outlier handling,
#' limits for identification, and grouping variables, among other settings.
#' @param edge_values_from Character string specifying the source of edge values
#' within the `edges` dataframe. Typically, this will be "beta" for beta
#' coefficients or "z" for z-scores.
#' @param keep_clean Logical indicating whether to keep the cleaned data within
#' the output dataframe. If `TRUE`, original columns corresponding to outliers
#' will be removed, and only cleaned data will be retained.
#'
#' @return A dataframe with edges organized and potentially cleaned for further
#' graph analysis. The structure of the output dataframe is tailored for easy
#' integration with graph generation and analysis functions, supporting both
#' group-level and individual-level analysis.
#'
#' @examples
#' # Assuming `edges` is a dataframe containing edges from GIMME analysis and
#' # `outliers` specifies how outliers should be handled
#' edge_df <- setup_edge_df(lagged = FALSE, edges = edges,
#'                          outliers = list(method = "winsorize",
#'                                          limit_class = "prob",
#'                                          limits = c(.025, .975)),
#'                          edge_values_from = "beta",
#'                          keep_clean = TRUE)
#'
#' @author Nate Hall
#' @export


setup_edge_df <- function(lagged,
                          edges,
                          outliers,
                          edge_values_from,
                          keep_clean = TRUE){


  # debug
  # -----
  # lagged = FALSE
  # outliers = list(method = "winsorize",
  #                 limit_class = "prob",
  #                 limits = c(.025, .975))
  # edge_values_from = "beta"
  # silent = FALSE
  # keep_igraph = TRUE
  # keep_clean = TRUE
  # -----




  edf <- edges |> mutate(level = case_when(level == "group" ~ "G",
                                            level == "ind" ~ "I",
                                            level == "sub" ~ "S"),
                          edge = paste0(rhs, "->", lhs, "_", level)) |>
    pivot_wider(names_from = edge, id_cols = c(file, sub_membership), values_from = all_of(edge_values_from)) |>
    select(file, sub_membership, ends_with("_G"), ends_with("_S"), ends_with("_I"))

  ### Handle edge outliers by group. Important to do this before calculating strength or other graph metrics to keep inputs to these steps on the rails.
  ## N.B. Only perform on group and subgroup edges, since many individual paths may not have enough strength from the sample to trust proportional thresholds.

  clean_these_edges <- colnames(edf)[c(which(grepl("_G", colnames(edf))), which(grepl("_S", colnames(edf))))]


  if(!is.null(outliers)){
    edf_clean <- handle_outliers_bygroup(edf,
                                         columns = clean_these_edges,
                                         method = outliers$method,
                                         limit_class = outliers$limit_class,
                                         limits = outliers$limits,
                                         groupBy = outliers$groupBy)

    if(keep_clean) {
      # remove original columns
      edf_out <- edf_clean |> select(-clean_these_edges) #|> rename_at(paste0(clean_these_edges, "_clean"), funclean_these_edges)
      # remove "clean" naming
      colnames(edf_out)[which(grepl("clean", colnames(edf_out)))] <- sub("_clean", "", colnames(edf_out)[which(grepl("clean", colnames(edf_out)))])
      #rearrange
      edf_clean <- edf_out |> select(file, columns_modified, sub_membership, ends_with("_G"), ends_with("_S"), ends_with("_I"))
    }
  } else{ edf_clean <- edf}

  # xx <- reshape2::melt(select(edf_clean,!ends_with("_I")), id.vars = c("file", "sub_membership")) #|>
  # ggplot(xx, aes(x= value, color = sub_membership, fill = sub_membership)) + geom_histogram() + facet_wrap(~variable, scales = "free")

  return(edf_clean)
}
