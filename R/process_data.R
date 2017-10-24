#' @title bin_data
#'
#' @param data the activity data to bin
#' @param minutes_per_bin number of minutes per bin
#'
#' @return the data, after binning
#'
#' @description function to bin data time-wise
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' f <- file.path(system.file(package = 'actogrammr'), 'testdata')
#' d <- read_clock_lab_files(file_names = list.files(path = f, full.names = TRUE))
#' b <- bin_data(data = d, minutes_per_bin = 6)

bin_data <- function(data,
                     minutes_per_bin) {

  stopifnot(minutes_per_bin %in% c(2, 3, 4, 5, 6, 10, 12, 15, 20, 30))

  file_name <- hour <- bin <- act <- light <- 'fake_global_for_CRAN'

  data %>%
    dplyr::mutate(bin = (min - 1) %/% minutes_per_bin) %>%
    dplyr::group_by(file_name, date, hour, bin) %>%
    dplyr::summarise(bin_act = sum(act),
                     bin_light = sum(light)) %>%
    dplyr::ungroup() %>%
    return()

}