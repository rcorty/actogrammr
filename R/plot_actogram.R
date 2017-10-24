#' @title plot_actogram
#'
#' @param data the activity data to plot
#' @param start_time the start time
#' @param end_time the end time
#'
#' @return the plot
#'
#' @export
#'
plot_actogram <- function(data,
                          start_time = min(data$date),
                          end_time = max(data$date)) {

  data %>%
    dplyr::filter(date > start_time, date < end_time) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = interaction(bin, hour),
                                           y = interaction(date, file_name),
                                           fill = bin_act)) +
    ggplot2::geom_tile()
}

