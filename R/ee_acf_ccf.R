#' Compute and Plot Autocorrelations and Cross-Correlations for Emotion Variables
#'
#' This function computes autocorrelations (ACF) and cross-correlations (CCF) for a given set of emotional variables across subjects.
#' It allows for visualization of the results in a grid of plots, with options to save the plots to a PDF file.
#'
#' @param data A data frame containing time-series data for emotional variables, as well as an `id` column for subjects.
#' @param emo_vars A character vector of column names representing the emotional variables to analyze.
#' @param lag.max Integer. The maximum lag to compute for ACF and CCF. Default is 20.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is 1.
#' @param plot_path Character. Path to save the resulting PDF file containing the grid of ACF/CCF plots. If `NULL`, no plot is saved. Default is `NULL`.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{\code{Var1}, \code{Var2}}{Variable pairs for which ACF/CCF was computed.}
#'   \item{\code{lab}}{Formatted label for the variable pair.}
#'   \item{\code{accf_results}}{A list-column containing data frames of lags and average ACF/CCF values.}
#'   \item{\code{accf_plot}}{A list-column containing ggplot objects for each ACF/CCF visualization.}
#'   \item{\code{acf}}{A character column indicating whether the plot is an autocorrelation (\code{"acf"}) or a cross-correlation (\code{"ccf"}).}
#' }
#'
#' @details
#' - For each subject, ACF is computed for individual variables and CCF is computed for variable pairs.
#' - Plots are generated for each variable pair and optionally saved as a PDF in a grid layout.
#' - The function uses parallel processing to speed up computation for large datasets.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' data <- data.frame(
#'   id = rep(1:10, each = 100),
#'   Angry = rnorm(1000),
#'   Sad = rnorm(1000),
#'   Happy = rnorm(1000)
#' )
#'
#' # Define emotional variables
#' emo_vars <- c("Angry", "Sad", "Happy")
#'
#' # Compute and plot ACF/CCF
#' ee_acf_ccf(data, emo_vars, lag.max = 20, cores = 2, plot_path = "acf_ccf_plots.pdf")
#' }
#'
#' @import nate.utils
#' @import psych
#' @import gridExtra
#'
#' @export


ee_acf_ccf <- function(data, emo_vars, lag.max = 20, cores = 1, plot_path = NULL){

  # Fit static bivariate correlations
  corr_bivar <- corr.test(data[, emo_vars])$r
  corr_bivar[lower.tri(corr_bivar)] <- NA

  # # Prepare data for ACF/CCF computation
  # tri_ccf <- corr_bivar %>%
  #   as.data.frame() %>%
  #   rownames_to_column("Var1") %>%
  #   pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  #   dplyr::select(Var1, Var2) %>%
  #   arrange_all()

  # Generate unique pairs (lower triangle)
  unique_pairs <- combn(emo_vars, 2, simplify = FALSE)
  # Add diagonal pairs
  unique_pairs <- c(unique_pairs, lapply(emo_vars, function(x) rep(x, 2)))

  # Create the data frame
  mvars <- data.frame(
    Var1 = sapply(unique_pairs, `[`, 1),
    Var2 = sapply(unique_pairs, `[`, 2)
  )

  # Merge with subject data and arrange
  mvars <- data %>%
    dplyr::select(id) %>%
    distinct() %>%
    merge(mvars) %>%
    arrange(id, factor(Var1, levels = emo_vars), factor(Var2, levels = emo_vars))

  # Function to fit multivariate ACFs
  fit_multivar_acfs <- function(id, Var1, Var2, ...){
    # id <- 1002
    # Var1 <- "Angry"
    # Var2 <- "Angry.1"


    dat <- data %>% dplyr::filter(id == id) %>% select(all_of(c(Var1, Var2)))

    if(Var1 == Var2){
      x <- pull(dat, Var1)
      out <- acf(x, lag.max = lag.max, na.action = na.pass, plot = FALSE)
    } else{
      x <- pull(dat, Var1)
      y <- pull(dat, Var2)
      out <- ccf(x, y, lag.max = lag.max, na.action = na.pass, plot = FALSE)
    }

    return(list(out))
  }

  # Compute ACFs/CCFs for all subjects
  acfs_all <- nate.utils::gmcmapply(mvars, fit_multivar_acfs, mc.cores = cores)

  acfs_all$acf_df <- lapply(acfs_all$fun_out, function(tt){
    acf_df <- data.frame(lag = tt$lag[,,1],
                         acf = tt$acf[,,1])
    acf_df
  })

  # Function to graph CCFs
  graph_ccfs <- function(Var1, Var2, lab, ...){
    #debug
    # Var1 <- "Angry"
    # Var2 <- "Angry.1"
    # lab <- "Angry~Angry.1"
    # theme = "fancy"

    tt <- acfs_all %>% dplyr::filter(Var1 == !!Var1 & Var2 == !!Var2)

    names(tt$acf_df) <- tt$id

    ttt <- bind_rows(tt$acf_df, .id = "id")

    ttt_spread <- ttt %>% spread(key = "id", value = "acf")
    acf_avgs <- data.frame(lag = ttt_spread$lag, `A.CCF` = dplyr::select(ttt_spread, -lag) %>% rowMeans())

    a.ccf_plot <-
      ggplot(acf_avgs, aes(x = lag, y = A.CCF)) +
      geom_col(just = .5, width = 1, color = "black", fill = "black", alpha = .9) +
      geom_line(linewidth = 2.5, color = "darkgrey") +
      ggtitle(lab) +
      see::theme_radar_dark() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "grey", size = 0.25), # Major grid lines visible under data
        panel.grid.minor = element_line(color = "darkgrey", size = .1), # Minor grid lines visible under data
        panel.background = element_rect(fill = "transparent", color = NA), # Transparent background
        panel.ontop = TRUE # Ensures grid lines are beneath the data
      ) +
      geom_vline(xintercept = 0, color = "red", linewidth = 1.5)


    return(list(lab = lab, accf_plot = a.ccf_plot, accf_results = acf_avgs))
  }

  # Generate and arrange plots
  mvars_distinct <-
    mvars %>%
    select(-id) %>%
    distinct() %>%
    mutate(lab = paste(Var1, Var2, sep = "~"))

  accf_plots <- nate.utils::gmcmapply(mvars_distinct, graph_ccfs, mc.cores = cores, SIMPLIFY = FALSE)


  tidy_accf_plots <- lapply(accf_plots, function(x) {
    tibble(lab = x$lab,
           accf_results = list(x$accf_results),
           accf_plot = list(x$accf_plot))
  }) %>% bind_rows()

  final_df <- as.tibble(mvars_distinct) %>%
    left_join(tidy_accf_plots, by = "lab") %>%
    mutate(acf = ifelse(Var1 == Var2, "acf", "ccf"), .after = lab)




  # x <- map(accf_plots, "accf_plot")
  # y <- map(accf_plots, "accf_results")

  # x %>% length()
  # names(x) <- mvars_distinct$lab

  m <- matrix(NA, length(emo_vars), length(emo_vars))
  m[lower.tri(m, diag = T)] <- 1:length(final_df$lab)

  # m <- matrix(NA, 9,9)
  # m[lower.tri(m, diag = T)] <- 1:45


  pdf(plot_path, width = 50, height = 40)
  accf_grid <- gridExtra::grid.arrange(grobs = final_df$accf_plot, layout_matrix = m)
  dev.off()

}
