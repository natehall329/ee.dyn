#' @title Read and Tidy Noldus FaceReader Data File
#'
#' @description  Reads in emotion expression data from Noldus FaceReader
#' V 8.0 output files (.txt format) and performs basic setup of data object.
#'
#' @param fpath Path to the Noldus FaceReader output file (.txt).
#' @param id (Optional) Participant ID. If provided, will be added as a column.
#'
#' @return  A tibble (data frame) containing:
#' \itemize{
#'  \item \code{id}: Participant ID (if provided).
#'  \item \code{time}: Time point in seconds.
#'  \item \code{neutral}, \code{happy}, \code{sad}, \code{angry},
#'        \code{surprised}, \code{scared}, \code{disgusted}:
#'        Emotion expression intensities (likely ranging from 0 to 1).
#'  \item \code{valence}: Valence of emotion expression (positive/negative).
#'  \item \code{arousal}: Arousal level of emotion expression.
#' }
#'
#' @examples
#' # Read data from a file for test participant 1
#' my_data <- ee_read("data/pt1.txt", id = 1)
#'
#' @importFrom data.table fread
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @export


ee_read <- function(fpath, id = NULL){
  cat("Reading and basic wrangling from: ", fpath, "\n")

  raw <- data.table::fread(fpath, skip="Video Time", header=T) %>% tibble()

  tidy <- raw %>% mutate(time = seq(0, .0333333333333333333*(nrow(.)-1), by =  .0333333333333333333)) %>%
    select(time, Neutral, Happy, Sad, Angry, Surprised, Scared, Disgusted, Valence, Arousal)

  colnames(tidy) <- tolower(colnames(tidy))

  # returns all as numeric and sets fit failure messages to NA
  tidy <- suppressWarnings(apply(tidy, 2, as.numeric)) %>% data.frame () %>% tibble()

  if(!is.null(id)){
    tidy %>% mutate(id = id, .before = everything())
  }


  # cat("    DONE\n")
  return(tidy)
}
