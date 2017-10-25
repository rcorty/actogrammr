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
#' d <- read_clock_lab_files(file_names = list.files(path = f, full.names = TRUE))
#' b <- bin_data(data = d, minutes_per_bin = 6)
#' \dontrun{
#' plot_actogram(data = b, start_date = '2010-01-01')
#' }

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

