#' Exclude or Winsorize Outliers in a DataFrame by Group
#'
#' This function allows for the exclusion or winsorization of outliers within specified
#' columns of a dataframe, potentially within groupings. Outliers can be defined either
#' through probabilistic quantiles or fixed values, with the ability to apply different
#' methods for lower and upper bounds of the data. The function can operate on grouped
#' data if a grouping variable is provided, allowing for more tailored outlier handling.
#'
#' @param df A dataframe containing the data to be processed.
#' @param columns A vector of column names within `df` that should be checked for outliers.
#' @param method A character vector specifying the method(s) for handling outliers: "exclude"
#' to set outliers to `NA_real_` or "winsorize" to adjust extreme values to the nearest
#' specified limit. If a vector of length 2 is passed, the first and second elements will
#' be applied to the lower and upper bounds, respectively.
#' @param limit_class A character vector indicating whether limits are based on probability
#' ("prob" for quantiles) or fixed values ("fixed"). If a vector of length 2 is passed,
#' it specifies options for lower and upper bounds; otherwise, it sets both bounds
#' equally.
#' @param limits Numeric vector of length 2 specifying the upper and lower limits for outlier
#' detection and handling, depending on `limit_class`. Use `NA` for one side to apply
#' handling to only the floor or ceiling.
#' @param groupBy A list containing vectors of grouping variables for lower and upper
#' bounds, or `NULL` for no grouping. This allows for differentiated handling by group.
#' @param id A string specifying the column identifier for a unique subject or observational
#' unit. This is used to track and report the number of changes made per subject, as subjects
#' with a large number of changes may have encountered estimation problems or singular fits.
#'
#' @return Returns a dataframe with outliers handled according to the specified methods
#' and parameters, alongside an additional column indicating the number of columns modified
#' for each subject. This output facilitates further analysis and tracking of data adjustments.
#'
#' @examples
#' # Assuming `df` is your dataframe, and you want to handle outliers in the "weight"
#' # and "height" columns, both lower and upper, by exclusion, based on quantile limits
#' cleaned_df <- handle_outliers_bygroup(df, columns = c("weight", "height"),
#'                                       method = c("exclude", "exclude"),
#'                                       limit_class = "prob",
#'                                       limits = c(.025, .975))
#'
#' @author Nate Hall
#' @export

handle_outliers_bygroup <- function(df,
                                    columns,
                                    method = "exclude",
                                    limit_class = "prob",
                                    limits = c(.025, .975),
                                    groupBy = NULL,
                                    id = "file"
){
  # debug
  # -----
  # df <- edf
  # columns <- colnames(edf)[-c(1:2)]
  # # columns <- colnames(edf)[-1]
  # method = c("exclude", "winsorize")
  # limit_class = c("fixed", "prob")
  # limits = c(0, .975)
  # groupBy = list(lo = "sub_membership", hi = NULL)
  # -----

  # browser()

  ### Can implement some checks on the input.
  stopifnot(length(limits) %in% c(1,2)) # need upper and lower limits.
  stopifnot(length(limit_class) %in% c(1,2))
  stopifnot(length(limits) == 2) # must have high and low with NA if dont want to apply.

  ### parse arguments based on lo vs hi. there is probably a more elegant way of doing this.
  method_lo <- method[1]
  method_hi <- ifelse(length(method) == 2, method[2], method)

  limit_class_lo <- limit_class[1]
  limit_class_hi <- ifelse(length(limit_class) == 2, limit_class[2], limit_class)

  limit_lo <- limits[1]
  limit_hi <- ifelse(length(limits) == 2, limits[2], limits)

  groupBy_lo <- ifelse(is_list(groupBy), ifelse(is.null(groupBy[["lo"]]), "none", groupBy[["lo"]]), groupBy)
  if("none" %in% groupBy_lo) groupBy_lo <- NULL
  groupBy_hi <- ifelse(is_list(groupBy), ifelse(is.null(groupBy[["hi"]]), "none", groupBy[["hi"]]), groupBy)
  if("none" %in% groupBy_hi) groupBy_hi <- NULL

  columns_unchanged <- colnames(df)[which(!colnames(df) %in% columns)]



  ########### perform specific hi and lo outlier handling.
  dflo <- handle_outliers_bygroup_oneside(df, columns, side = "lo", method = method_lo, limit_class = limit_class_lo, limit = limit_lo, groupBy = groupBy_lo)
  dfhi <- handle_outliers_bygroup_oneside(df, columns, side = "hi", method = method_hi, limit_class = limit_class_hi, limit = limit_hi, groupBy = groupBy_hi)

  ########### merge different outlier dfs into one by capitalizing on differences between original and cleaned
  cleanlo <- dflo |> ungroup() |> select(contains("_clean"))
  cleanhi <- dfhi |> ungroup() |> select(contains("_clean"))
  origdat <- dfhi |> ungroup() |> select(-contains("_clean"), -all_of(columns_unchanged))

  refmat <- (as.matrix(cleanhi) != as.matrix(origdat)) # will be TRUE for values that have been changed due to upper limit outlier procedure
  mh <- as.matrix(cleanhi); ml <- as.matrix(cleanlo)
  arind <- which(refmat, arr.ind = TRUE)
  ml[arind] <- mh[arind]

  clean_all <- cbind(df,as.data.frame(ml)) |> tibble()

  ### flag subjects who had values changed. Could flag as altered or unaltered as binary, but would be better to provide dimensional variation in the number of columns that have been edited and let the practitioner decide what is "too much"
  clean_all <- clean_all |> mutate(columns_modified = 0, .after = file)
  for(i in unique(clean_all |> pull(file))){
    filei <- clean_all |> filter_at(vars(file), any_vars(. == i))
    for(j in columns){
      if(!is.na(filei |> pull(j))){
        columns_mod_current <- clean_all[which(clean_all["file"] == i), "columns_modified"] |> pull()
        orig_col <- pull(clean_all[which(clean_all["file"] == i), j])
        clean_col <- pull(clean_all[which(clean_all["file"] == i), paste0(j, "_clean")])
        ## update count
        clean_all[which(clean_all["file"] == i), "columns_modified"] <- columns_mod_current + ifelse(all(is.na(orig_col), is.na(clean_col)) | orig_col == clean_col, 0, 1)
      }
    }
  }

  return(clean_all)
}






# unique(clean_all$columns_modified)
#
# xx <- clean_all |> dplyr::select(file, contains("neutral->disgusted")) |> print(n = 500)
# hist(xx$`neutral->disgusted_G`)
#
