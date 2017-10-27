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
#' @examples
#' f <- file.path(system.file(package = 'actogrammr'), 'testdata')
#' d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])
#' b <- bin_data(data = d, minutes_per_bin = 6)
#' \dontrun{
#' plot_actogram(data = b, start_date = '2010-01-01')
#' }

plot_actogram <- function(data,
                          start_date = min(data$date),
                          end_date = max(data$date)) {

  bin <- hour <- file_name <- bin_act <- bin_light <- 'fake_global_for_CRAN'

  data %>%
    dplyr::filter(date > start_date, date < end_date) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(xmin = as.numeric(interaction(hour, bin, lex.order = TRUE)),
                                           xmax = as.numeric(interaction(hour, bin, lex.order = TRUE)) + 1,
                                           ymin = 0)) +
    ggplot2::geom_rect(mapping = ggplot2::aes(ymax = max(bin_act, na.rm = TRUE), fill = bin_light), alpha = 0.6) +
    ggplot2::geom_rect(mapping = ggplot2::aes(ymax = bin_act)) +
    ggplot2::facet_grid(facets = date~.) +
    ggplot2::scale_fill_gradient(low = 'white', high = 'lightblue', guide = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(x = 0, units = 'mm'),
                   strip.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(x = c(0, 0, 0, 0), units = 'mm')) +
    ggplot2::labs(x = NULL, y = NULL)
}
