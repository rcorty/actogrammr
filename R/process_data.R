#' @title bin_data
#'
#' @param data the activity data to bin
#' @param minutes_per_bin number of minutes per bin
#'
#' @return the data, after binning
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
bin_data <- function(data,
                     minutes_per_bin) {

  stopifnot(minutes_per_bin %in% c(2, 3, 4, 5, 6, 10, 12, 15, 20, 30))

  data %>%
    dplyr::mutate(bin = (min - 1) %/% minutes_per_bin) %>%
    dpyr::group_by(file_name, date, hour, bin) %>%
    dplyr::summarise(bin_act = sum(act),
                     bin_light = sum(light)) %>%
    dplyr::ungroup() %>%
    return()

}