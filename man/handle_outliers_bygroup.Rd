% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_outliers_bygroup.R
\name{handle_outliers_bygroup}
\alias{handle_outliers_bygroup}
\title{Exclude or Winsorize Outliers in a DataFrame by Group}
\usage{
handle_outliers_bygroup(
  df,
  columns,
  method = "exclude",
  limit_class = "prob",
  limits = c(0.025, 0.975),
  groupBy = NULL,
  id = "file"
)
}
\arguments{
\item{df}{A dataframe containing the data to be processed.}

\item{columns}{A vector of column names within `df` that should be checked for outliers.}

\item{method}{A character vector specifying the method(s) for handling outliers: "exclude"
to set outliers to `NA_real_` or "winsorize" to adjust extreme values to the nearest
specified limit. If a vector of length 2 is passed, the first and second elements will
be applied to the lower and upper bounds, respectively.}

\item{limit_class}{A character vector indicating whether limits are based on probability
("prob" for quantiles) or fixed values ("fixed"). If a vector of length 2 is passed,
it specifies options for lower and upper bounds; otherwise, it sets both bounds
equally.}

\item{limits}{Numeric vector of length 2 specifying the upper and lower limits for outlier
detection and handling, depending on `limit_class`. Use `NA` for one side to apply
handling to only the floor or ceiling.}

\item{groupBy}{A list containing vectors of grouping variables for lower and upper
bounds, or `NULL` for no grouping. This allows for differentiated handling by group.}

\item{id}{A string specifying the column identifier for a unique subject or observational
unit. This is used to track and report the number of changes made per subject, as subjects
with a large number of changes may have encountered estimation problems or singular fits.}
}
\value{
Returns a dataframe with outliers handled according to the specified methods
and parameters, alongside an additional column indicating the number of columns modified
for each subject. This output facilitates further analysis and tracking of data adjustments.
}
\description{
This function allows for the exclusion or winsorization of outliers within specified
columns of a dataframe, potentially within groupings. Outliers can be defined either
through probabilistic quantiles or fixed values, with the ability to apply different
methods for lower and upper bounds of the data. The function can operate on grouped
data if a grouping variable is provided, allowing for more tailored outlier handling.
}
\examples{
# Assuming `df` is your dataframe, and you want to handle outliers in the "weight"
# and "height" columns, both lower and upper, by exclusion, based on quantile limits
cleaned_df <- handle_outliers_bygroup(df, columns = c("weight", "height"),
                                      method = c("exclude", "exclude"),
                                      limit_class = "prob",
                                      limits = c(.025, .975))

}
\author{
Nate Hall
}
