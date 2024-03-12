#' @title Read and Tidy Noldus FaceReader Data Files in a Specified directory
#'
#' @description This function reads emotion expression data files exported from Noldus FaceReader V 8.0,
#' efficiently organizing the data into a tibble. It can utilize both single-core and
#' parallel processing for faster reading of multiple files.
#'
#' @param dir_path Character string specifying the directory path containing the data files.
#' @param ncores Integer specifying the number of cores for parallel processing.
#'               Default is 1 (single-core).
#' @param rm_duplicates Logical. Whether or not to remove duplicate EE data if detected
#'                      Default is TRUE.
#'
#' @return A tibble with columns:
#'   * **id:** Participant ID (assuming this is extractable from the filename).
#'   * **fpath:** Full file path.
#'   * **dat:** The emotion expression data (structure from `ee_read` function).
#'   * **duplicate** Logical on whether dat is duplicated elsewhere in the data.frame
#'   * **rows** Number of rows in dat
#'
#' @examples
#' # Example usage with a Noldus FaceReader data directory
#' face_data_dir <- "/path/to/facereader/data"
#' emotion_data <- ee_read_dir(face_data_dir, ncores = 2)
#'
#' @importFrom tibble tibble
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr arrange
#'
#' @export

ee_read_dir <- function(dir_path, ncores = 1, rm_duplicates = TRUE){

  file_names <- list.files(dir_path, full.names = TRUE)

  stopifnot(all(grepl("^.*/pt\\d+\\.txt$", file_names)))

  uber_df <- tibble(id = as.numeric(  gsub("pt", "", tools::file_path_sans_ext(list.files(dir_path)))),
                    fpath =  file_names) %>% arrange(id)

  if(ncores == 1){
    uber_df$dat <- lapply(uber_df$fpath, ee_read)
  } else if(ncores >= 2){
    # parallel computation
    library(parallel)
    cl <- makeCluster(ncores) # Create a cluster
    uber_df$dat <- parLapply(cl, uber_df$fpath, ee_read)
    stopCluster(cl)
  }

  # Remove any duplicates if they exist
  uber_df$duplicate <- duplicated(uber_df$dat)
  if(rm_duplicates) uber_df <- uber_df %>% filter(!duplicate)

  ## seems like there are a couple that this did not capture that have the exact same number of rows, which seems very unlikely.
  uber_df$rows <- sapply(uber_df$dat, nrow)

  return(uber_df)

}


