#' Plot Emotion Expression Time Series
#'
#' This function visualizes the time series of facial emotion expression data from FaceReader. It supports the plotting
#' of raw emotion intensities over time and can distinguish between discrete emotions
#' and circumplex model (valence and arousal). Users can specify the type of plot desired:
#' combined (default), discrete emotions, or circumplex model only.
#'
#' @param sub_ts A data frame containing the subject's emotion time series data.
#' @param include_raw Logical, indicating whether to include raw emotion data in the plot.
#'        Default is `TRUE`.
#' @param id Optional; the subject's identifier. If provided, it will be used in the plot title.
#' @param desc Optional; a data frame returned by `ee_get_descriptives` containing descriptive
#'        statistics of the subject's data. If provided, the percentage of readable frames
#'        is included in the plot subtitle.
#' @param plot_type Character string specifying the type of plot: "combined" (default),
#'        "discrete", or "circumplex". Determines whether to plot all data combined,
#'        just the discrete emotions, or just the circumplex model (valence and arousal).
#'
#' @return A `ggplot` object representing the emotion time series plot as specified
#'         by the `plot_type` parameter.
#'
#' @examples
#' plot_ee_timeseries(sub_ts = my_data, id = "Subject1", desc = descriptives,
#'                    plot_type = "combined")
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth labs coord_cartesian
#' @importFrom cowplot plot_grid
#' @importFrom see theme_blackboard
#' @importFrom ggpubr get_legend
#' @import ggplot2 cowplot see ggpubr
#'
#' @author Nate Hall
#'
#' @export

plot_ee_timeseries <- function(sub_ts, include_raw = TRUE, id = NULL, desc = NULL, plot_type = "combined"){
  # desc is output from ee_get_descriptives

  assertthat::assert_that(plot_type %in% c("combined", "discrete", "circumplex"))

  ts_melt <- reshape2::melt(select(sub_ts, -id), id.vars = "time") |> mutate(ts_class = case_when(variable %in% c("valence", "arousal") ~ "circumplex",
                                                                                                    TRUE ~ "discrete")) |> rename(`Emotion` = `variable`) |> mutate(Emotion = str_to_title(Emotion,))

  # unique(ts_melt$Emotion)
  # str(ts_melt)
  # unique(ts_melt$ts_class)

  # missing_times <- ts_melt$time[is.na(ts_melt$value)] |> unique()
  cols <- c("Neutral" = "darkgrey", "Happy" = "yellow", "Sad" = "blue", "Angry" = "red", "Surprised" = "violet", "Scared" = "darkgreen", "Disgusted" = "darkorange") # colors that kinda match the emotions
  if(!is.null(id) & !is.null(desc)){
    title <- paste0("Subject: ", id)
    desc <- desc |> filter(emotion != "id")
    subtitle <- paste0("Percentage of Readable Frames: ", round(unique(desc$perc_complete) * 100,2), "%")
  } else{title <- ""; subtitle <- ""}

  # ts_melt

  # plot of discrete emotions
  discrete <- ggplot(filter(ts_melt, ts_class == "discrete"), aes(x = time, y = value, color = Emotion)) +
    geom_line(alpha = .175) +
    geom_smooth() +
    see::theme_blackboard() +
    scale_color_manual(values = cols) +
    labs(x = "Time (sec)", y = "Emotion Expression Intensity", title = title, subtitle = subtitle) +
    coord_cartesian(ylim = c(0, 1))

  if (plot_type == "discrete") {
    return(discrete)
  }

  ##-----------------------
  ##  Valence and Arousal
  ##-----------------------

  legend <- ggpubr::get_legend(discrete); discrete <- discrete + theme(legend.position = "none")

  ## Valence
  valence <- ggplot(filter(ts_melt, Emotion == "Valence"), aes(x = time, y = value)) + #geom_line(alpha = .175, color = "darkgrey") +
   geom_hline(yintercept = 0, color = "white", linetype = "dashed") +
     geom_line(alpha = .25, color = "white") +
    geom_smooth() +
    see::theme_blackboard() +
    labs(x = "Time", y = "Valence") +
    coord_cartesian(ylim = c(-1, 1))

  # ## Arousal
  arousal <- ggplot(filter(ts_melt, Emotion == "Arousal"), aes(x = time, y = value)) +
    geom_line(alpha = .25, color = "white") +
    geom_smooth() +
    see::theme_blackboard() +
    labs(x = "Time", y = "Arousal") +
    coord_cartesian(ylim = c(0, 1))

  va <- cowplot::plot_grid(valence, arousal, ncol = 1)

  if(plot_type == "circumplex"){
    # Just affective circumplex
    return(va)
  }

 comb <- cowplot::plot_grid(discrete, va, ncol = 1, rel_heights = c(.6, .4))


  return(comb)
}
