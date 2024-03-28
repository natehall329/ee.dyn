#' Identify NA Rows at the Edges of Time Series Columns
#'
#' This function iterates over specified columns in a data frame, identifying rows
#' that contain NA values at the beginning and end of the time series. It's particularly
#' useful for preprocessing steps before interpolation, allowing users to handle edge
#' cases where NA values may not be suitable for interpolation without a valid anchor.
#'
#' @param dat A data frame containing the time series data.
#' @param cols A character vector specifying the names of the columns in `dat` to check
#' for NA rows at the edges. Typically, this excludes identifier and time columns.
#'
#' @return A list where each element corresponds to a column specified in `cols`. Each
#' list element contains a numeric vector of row numbers that are NA at the beginning
#' or end of the time series for that column. If a column has no NAs at its edges, it
#' will not appear in the list.
#'
#' @examples
#' dat <- data.frame(id = 1:10, time = 1:10, measure1 = c(NA, NA, 3:8, NA, NA), measure2 = c(1:4, NA, NA, NA, 7:10))
#' interp_cols <- setdiff(colnames(dat), c("id", "time"))
#' na_edge_rows <- get_na_edge_rows(dat, interp_cols)
#' # Check the output to see the rows with NAs at the edges for 'measure1' and 'measure2'
#'
#' @author Nate Hall
#'
#' @export
#'

get_na_edge_rows <- function(dat, cols = c("neutral", "happy", "sad", "angry", "surprised", "scared", "disgusted", "valence", "arousal")) {
  na_edge_rows <- list()

  for (col in cols) {
    col_data <- dat[[col]]
    na_rows <- which(is.na(col_data))

    if (length(na_rows) == 0) {
      next # Skip if no NAs in the column
    }

    # Check beginning
    if (na_rows[1] == 1) { # If the first row is NA
      for (i in na_rows) {
        if (i == max(na_rows) || !is.na(col_data[i + 1])) {
          na_edge_rows[[col]] <- c(na_edge_rows[[col]], 1:i)
          break
        }
      }
    }

    # Check end
    if (na_rows[length(na_rows)] == nrow(dat)) { # If the last row is NA
      for (i in rev(na_rows)) {
        if (i == min(na_rows) || !is.na(col_data[i - 1])) {
          na_edge_rows[[col]] <- c(na_edge_rows[[col]], i:nrow(dat))
          break
        }
      }
    }
  }

  return(na_edge_rows)
}
