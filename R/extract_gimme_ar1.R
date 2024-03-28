extract_gimme_ar1 <- function(betas, calc_avg_ar1 = FALSE){
  ######## capture AR1 (lagged) components for each nodal timeseries
  ar1s <- betas |> dplyr::filter(paste0(lhs,"lag") == rhs) |> pivot_wider(names_from = lhs, id_cols = file, values_from = c(beta))
  colnames(ar1s)[-1] <- paste0(colnames(ar1s)[-1], "_ar1")

  ######## calculate average AR1 component per file
  if(calc_avg_ar1){
    ar1s$avg_ar1 <- ar1s |> select(-file) |> rowMeans()
  }

  ######## tag with subgroup labels if applicable
  if("sub_membership" %in% names(betas)){
    head(betas)
    ar1s <- ar1s |> left_join(distinct(select(betas, file, sub_membership)), by = "file")
  }

  return(ar1s)
}
