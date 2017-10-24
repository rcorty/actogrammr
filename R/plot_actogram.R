#' @title plot_actogram
#'
#' @param data the activity data to plot
#' @param start_date the start time
#' @param end_date the end time
#'
#' @return the plot
#'
#' @description plots an actogram
#'
#' @export
#'
plot_actogram <- function(data,
                          start_date = min(data$date),
                          end_date = max(data$date)) {

  bin <- hour <- file_name <- bin_act <- 'fake_global_for_CRAN'

  data %>%
    dplyr::filter(date > start_date, date < end_date) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = interaction(bin, hour),
                                           y = interaction(date, file_name),
                                           fill = bin_act)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_continuous(low = 'white', high = 'darkblue')
}

