#' One-sided Outlier Handling by Group
#'
#' Handles outliers in specified columns of a dataframe on one side (lower or
#' higher) based on a given method (exclude or winsorize), limit classification
#' (probability or fixed value), and an optional grouping. This function can
#' set outliers to NA (for exclusion) or adjust their values (for winsorization).
#'
#' @param df A dataframe containing the data to be processed.
#' @param columns A character vector specifying the columns within `df` to be
#' checked for outliers.
#' @param side A character string indicating the side of the outlier. Valid
#' options are "lo" for lower side or "hi" for higher side.
#' @param method A character string specifying the method to handle outliers.
#' Valid options are "exclude" (default) or "winsorize".
#' @param limit_class A character string indicating the type of limit to use.
#' Valid options are "prob" for probabilistic limits based on quantiles or
#' "fixed" for fixed value limits. Default is "prob".
#' @param limit A numeric value specifying the limit for outlier detection.
#' When `limit_class` is "prob", `limit` represents a quantile (e.g., 0.025 for
#' lower 2.5%); when `limit_class` is "fixed", it represents the actual value
#' threshold.
#' @param groupBy An optional character vector specifying columns to group by
#' before performing outlier handling. If NULL (default), outlier handling is
#' applied column-wise across the entire dataset.
#'
#' @return A dataframe with outliers handled according to the specified method
#' and parameters. If the method is "exclude", outliers are set to NA. If the
#' method is "winsorize", outlier values are adjusted.
#'
#' @examples
#' # Assuming `df` is your dataframe, and you want to handle lower outliers
#' # in the "weight" column by exclusion, based on the bottom 2.5% quantile
#' cleaned_df <- handle_outliers_bygroup_oneside(df, columns = "weight",
#'                                               side = "lo", method = "exclude",
#'                                               limit_class = "prob", limit = .025)
#'
#' @author Nate Hall
#' @export


handle_outliers_bygroup_oneside <- function(df,
                                            columns,
                                            side = "lo", # lo or high
                                            method = "exclude",
                                            limit_class = "prob",
                                            limit = .025,
                                            groupBy = NULL
) {
# browser()
  exclude_quantile_onesided <- function(x, prob, side, ...){
    val <- as.numeric(quantile(x,prob, na.rm = TRUE))
    if(side == "lo") x[which(x < val)] <- NA_real_
    if(side == "hi") x[which(x > val)] <- NA_real_
    return(x)
  }

  exclude_value_onesided <- function(x, val, side, ...){
    if(side == "lo") x[which(x < val)] <- NA_real_
    if(side == "hi") x[which(x > val)] <- NA_real_
    return(x)
  }

  if(method == "exclude"){
    ##### METHOD(EXCLUDE): Set values satisfying conditions to NA
    if(limit_class == "fixed"){
      ### fixed (a priori) values will not differ by group. FEATURE WISH LIST: could add the ability to pass a df of a priori upper and lower limits, stratified by grouping levels. Seems like a very rare case that we would need this.
      df <- df |> mutate_at(.funs = list(clean = exclude_value_onesided), .vars= vars(columns), val = limit, side = side, na.rm = TRUE)
    } else if(limit_class == "prob") {
      ### if quantile-based exclusion is desired
      if(is.null(groupBy)){
        #### column-wise/non-grouped, quantile exclusion
        df <- df |> mutate_at(.funs = list(clean = exclude_quantile_onesided), .vars= vars(columns), prob = limit, side = side, na.rm = TRUE)
      }  else{
        #### grouped column-wise, quantile exclusion
        df <- df |> group_by_at(groupBy) |> mutate_at(.funs = list(clean = exclude_quantile_onesided), .vars= vars(columns), prob = limit, side = side, na.rm = TRUE)
      }
    }
  } else if (method == "winsorize") {
    if(limit_class == "fixed"){
      #### fixed winsorization. Do not differ these by group. Can add with feature wish list from above.
      if(side == "lo") df <- df |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), minval = limit, probs = c(0, 1), na.rm = TRUE)
      if(side == "hi") df <- df |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), maxval = limit, probs = c(0, 1), na.rm = TRUE)
    } else if(limit_class == "prob") {
      ### if quantile-based winsorization is desired
      if(is.null(groupBy)){
        #### column-wise/non-grouped, quantile winsorization
        if(side == "lo") df <- df |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), probs = c(limit, 1), na.rm = TRUE)
        if(side == "hi") df <- df |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), probs = c(0, limit), na.rm = TRUE)
      }  else{
        #### grouped column-wise, quantile winsorization
        if(side == "lo") df <- df |> group_by_at(groupBy) |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), probs = c(limit, 1), na.rm = TRUE)
        if(side == "hi") df <- df |> group_by_at(groupBy) |> mutate_at(.funs = list(clean = DescTools::Winsorize), .vars= vars(columns), probs = c(0, limit), na.rm = TRUE)
      }
    }
  }
  return(df)
}


