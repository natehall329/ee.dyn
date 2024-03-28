#' Extract Graph Attributes from GIMME Output
#'
#' This function extracts EE parameters (betas) of interest from raw GIMME. It can extract
#' contemporaneous (lag-0) and cross-lagged relationships, as well as self-edges (i.e.,
#' the AR1 component of each nodal time series). It allows for the extraction and storage
#' of different graph properties by disentangling these components. Additionally,
#' outlier adjustment is supported through various methods, such as winsorizing.
#'
#' @param betas A matrix or data frame of betas resulting from a GIMME analysis.
#' These betas represent the coefficients for the edges in the graph.
#' @param attributes A character string specifying which graph attributes to
#' extract. Options are "all", "ar1" for self-edges, "contemporaneous" for
#' contemporaneous relationships, and "cross_lagged" for cross-lagged
#' relationships. Default is "all".
#' @param outliers A list specifying the method and parameters for outlier
#' adjustment. The default method is "winsorize" with limits set to the 2.5th
#' and 97.5th percentiles, and grouping by subject membership.
#' @param keep_igraph A logical indicating whether to keep the igraph object in
#' the output. Default is TRUE.
#'
#' @return A list containing the extracted graph attributes based on the specified
#' criteria. Each attribute is stored in its respective named element within the
#' list.
#'
#' @examples
#' # Assuming `betas` is a matrix of beta coefficients from GIMME
#' graph_attrs <- get_gimme_graph_attrs(betas)
#' graph_attrs_ar1 <- get_gimme_graph_attrs(betas, attributes = "ar1")
#'
#' @author Nate Hall
#'
#' @export

get_gimme_graph_attrs <- function(betas,
                                  extract_attributes = "all",
                                  outliers = NULL,
                                  keep_igraph = TRUE
){

  # ensure outlier handling is properly specified or empty
  stopifnot(is.null(outliers) | is.list(outliers))
  # Example list:
  # -------------
  # outliers = list(method = "winsorize",   # can be "winsorize" or "exclude" (NA-substitution)
  #                 limit_class = "prob",   # can be "prob" (proportion) or "fixed" (a priori value). must be length 1 or 2. If length 2, first element will be applied to the lower limit and second will be applied to upper limit
  #                 limits = c(.025, .975), # values to be used alongside limit_class (either probabilities or a priori values)
  #                 groupBy = "sub_membership" # optional character with column name of variable to group outlier handling by (e.g. proportional thresholds applied at the within-group rather than sample level. hopefully this distinction shouldnt matter much!)
  #                 )
  if(!is.null(outliers)){
    # user wants outlier handling if they've changed default, add defaults for empty fields
    if(is.null(outliers$method)) outliers$method <- "exclude"
    if(is.null(outliers$limit_class)) outliers$limit_class <- "prob"
    if(is.null(outliers$limits)){
      # Could either be probabilities or values
      if(outliers$limit_class == "prob"){
        outliers$limits <- c(.025, .975)
      } else if (outliers$limit_class == "fixed") {
        outliers$limits <- c(NA, 1) # do nothing on low end, trim to 1 on high end.
      } else{
        stop("outliers$limit_class must be either 'prob' or 'fixed'")
      }
    }
    # confirm user didnt mess up naming conventions
    stopifnot(all(outliers$method %in% c("exclude", "winsorize")))
    stopifnot(length(outliers$method) <= 2)
    stopifnot(all(outliers$limit_class %in% c("prob", "fixed")))
    stopifnot(length(outliers$limit_class) <= 2)
    stopifnot(length(outliers$limits) == 2)
  }

  # check betas structure aligns with gimme output conventions
  stopifnot(colnames(betas) %in% c("file", "lhs", "op", "rhs", "beta", "se", "z", "pval", "level", "sub_membership"))

  # extract options
  stopifnot(extract_attributes %in% c("all", "ar1", "contemporaneous", "cross_lagged"))


  gimme_vars <- list()

  # AR1 component
  if(extract_attributes %in% c("ar1", "all")) gimme_vars[["ar1"]] <- extract_gimme_ar1(betas)

  # Graph properties constructed on contemporaneous edges
  if(extract_attributes %in% c("contemporanous", "all")) gimme_vars[["contemporaneous"]] <- extract_gimme_graph_properties(betas, lagged = FALSE, edge_values_from = "beta", outliers = outliers, keep_igraph = keep_igraph )

  # Graph properties constructed on cross-lagged edges
  if(extract_attributes %in% c("cross_lagged", "all")) gimme_vars[["cross_lagged"]] <- extract_gimme_graph_properties(betas, lagged = TRUE, edge_values_from = "beta", outliers = outliers, keep_igraph = keep_igraph)

  return(gimme_vars)

}
