#' Interpolate Missing Values
#'
#' This function performs interpolation on missing values within time series
#' data, followed by range adjustments if requested to ensure EE columns remain within
#' their valid ranges (0-1 for all EEs other than valence, which is adjusted to [-1, 1].
#'
#' @param dat A data frame containing time series data, with the first column
#'            assumed to be a time/index variable.
#' @param maxgap Maximum gap size (number of consecutive NAs) allowed
#'                  for interpolation. Defaults to 30 (for FaceReader data collected at 30 Hz, equivalent to smoothing over 1 second).
#' @param interp_option The interpolation option to use from the `imputeTS`
#'                      package. Can take values, 'linear', 'spline', 'stine'.
#'                      Defaults to "stine".
#' @param range_restrict Logical. When TRUE (Default), perform range restriction on EE timeseries
#'
#' @return A data frame with interpolated values and (if requested) adjusted ranges. If range_restrict is TRUE, the range-adjusted columns will be added with _adjusted appended to the column names.
#'
#' @examples
#' dat <- ee.dyn::ee_read("dev/data/pt1.txt", id = 1)
#' result <- ts_interp(dat)
#'
#' @author Nate Hall
#'
#' @importFrom imputeTS na_interpolation
#' @importFrom dplyr mutate
#'
#' @export
#'

ts_interp <- function(dat, maxgap = 30, interp_option = "stine", range_restrict = TRUE) {
  # Impute missing values using interpolation
  # dat <- ee_read("dev/data/pt2.txt")

  interp_cols <- which(!colnames(dat) %in% c("id", "time"))

  #. N.B.
  # the way na_interpolation works is that any gaps <= maxgap get interpolated over, including the very beginning of the timeseries
  # - in my tests one subject had 29 NA rows in the beginning of their timeseries, and the interpolation essentially copied over the first measurement for the first 29 rows.
  # - Instead, i prefer to drop the NA columns at the beginning and end of the timeseries, to ensure that interpolation only goes along runs of NAs with a reliable anchor at each end.

  na_edges <- get_na_edge_rows(dat)

  if(!length(na_edges) == 0){
    # NA rows identified should be identical across columns. this simple function can help us make sure that is the case
    check_identical_elements <- function(lst) {
      reference_element <- lst[[1]] # Take the first element as reference
      for (i in 2:length(lst)) { # Compare each element to the reference
        if (!isTRUE(all.equal(reference_element, lst[[i]]))) {
          return(FALSE) # Return FALSE if any element is not identical
        }
      }
      return(TRUE) # Return TRUE if all elements are identical
    }

    if(!check_identical_elements(na_edges)){
      warning("Be advised, across the columns checked, NA values at the beginning and end of the timeseries are different. \nThis can lead to unintended consequences during NA interpolation!")
    }

    remove_rows <- unlist(unique(na_edges))

    cat("Removing NAs at the beginning | end of this subject's time series: ", remove_rows)
    dat <- dat[-remove_rows,]
  }

  dat[,interp_cols] <- apply(dat[,interp_cols], 2, function(x) imputeTS::na_interpolation(x, option = interp_option, maxgap = maxgap))

  if(range_restrict){
    # Range adjustment (0-1 columns, excluding 'valence')
    range_cols <- setdiff(names(dat), c("time", "id", "valence"))
    dat <- dat |>
      mutate(across(range_cols, ~ ifelse(.x < 0, 0, ifelse(.x > 1, 1, .x)), .names = "{col}_adjusted"),
             valence_adjusted = ifelse(valence < -1, -1, ifelse(valence > 1, 1, valence)))
  }

  return(dat)
}

