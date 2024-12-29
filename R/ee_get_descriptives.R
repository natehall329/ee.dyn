#' Compute Descriptive Statistics for Emotion Expression Time Series Data
#'
#' This function takes a dataframe with time series data and computes descriptive
#' statistics for each EE variable present in the dataset. It reshapes the data,
#' calculates the number of missing and complete cases, the percentage of complete cases,
#' and standard descriptive statistics (mean, median, standard deviation, 5th percentile,
#' 95th percentile, minimum, and maximum) for each emotion.
#'
#' @param dat A dataframe containing the time series data. The dataframe must
#' include a "time" column and one or more variables representing different
#' emotions or signals to analyze.
#'
#' @return A dataframe (tibble) containing the descriptive statistics for each
#' emotion variable: number of missing values, number of complete cases,
#' percentage of complete cases, mean, median, standard deviation, 5th percentile,
#' 95th percentile, minimum, and maximum values.
#'
#' @import dplyr
#' @importFrom reshape2 melt
#' @examples
#' \dontrun{
#' # Assumes `ee_read` function is used to read time series data
#' dat <- ee_read("inst/extdata/pt1.txt")
#' descriptives <- ee_get_descriptives(dat)
#' print(descriptives)
#'}
#'
#' @author Nate Hall
#'
#' @export



ee_get_descriptives <- function(dat){

  dmelt <- reshape2::melt(dat, id.vars = "time")  |> rename(`emotion` = `variable`,
                                                             `signal` = `value`)
  descriptives <- dmelt |> add_count(emotion) |> group_by(emotion) |>
    dplyr::summarise(missing = sum(is.na(signal)),
                     complete = unique(n) - missing,
                     perc_complete = complete/unique(n),
                     mean = mean(signal, na.rm = TRUE),
                     median = median(signal, na.rm = TRUE),
                     sd = sd(signal, na.rm = TRUE),
                     q05 = quantile(signal, .05, na.rm = TRUE),
                     q95 = quantile(signal, .95, na.rm = TRUE),
                     min = min(signal, na.rm =TRUE),
                     max = max(signal, na.rm = TRUE))

  return(descriptives)
}

