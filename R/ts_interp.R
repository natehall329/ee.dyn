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

ts_interp <- function(dat, maxgap = 30, interp_option = "stine", range_restrict = TRUE) {
  # Impute missing values using interpolation
  dat[,-1] <- apply(dat[,-1], 2, function(x) imputeTS::na_interpolation(x, option = interp_option, maxgap = maxgap))

  if(range_restrict){
    # Range adjustment (0-1 columns, excluding 'valence')
    range_cols <- setdiff(names(dat), c("time", "valence"))
    dat <- dat |>
      mutate(across(range_cols, ~ ifelse(.x < 0, 0, ifelse(.x > 1, 1, .x)), .names = "{col}_adjusted"),
             valence_adjusted = ifelse(valence < -1, -1, ifelse(valence > 1, 1, valence)))
  }

  return(dat)
}


