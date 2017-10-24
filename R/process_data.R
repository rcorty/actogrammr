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
                     minutes_per_bin = c('1', '2', '3', '4', '5', '6',
                                         '10', '12', '15', '20', '30')) {

  minutes_per_bin <- as.numeric(match.arg(arg = minutes_per_bin))

  if (minutes_per_bin == 6) {

    data$act_bin1  = rowSums(data %>% dplyr::select(act_min1:act_min6))
    data$act_bin2  = rowSums(data %>% dplyr::select(act_min7:act_min12))
    data$act_bin3  = rowSums(data %>% dplyr::select(act_min13:act_min18))
    data$act_bin4  = rowSums(data %>% dplyr::select(act_min19:act_min24))
    data$act_bin5  = rowSums(data %>% dplyr::select(act_min25:act_min30))
    data$act_bin6  = rowSums(data %>% dplyr::select(act_min31:act_min36))
    data$act_bin7  = rowSums(data %>% dplyr::select(act_min37:act_min42))
    data$act_bin8  = rowSums(data %>% dplyr::select(act_min43:act_min48))
    data$act_bin9  = rowSums(data %>% dplyr::select(act_min49:act_min54))
    data$act_bin10 = rowSums(data %>% dplyr::select(act_min55:act_min60))

    data$light_bin1  = rowSums(data %>% dplyr::select(light_min1:light_min6))
    data$light_bin2  = rowSums(data %>% dplyr::select(light_min7:light_min12))
    data$light_bin3  = rowSums(data %>% dplyr::select(light_min13:light_min18))
    data$light_bin4  = rowSums(data %>% dplyr::select(light_min19:light_min24))
    data$light_bin5  = rowSums(data %>% dplyr::select(light_min25:light_min30))
    data$light_bin6  = rowSums(data %>% dplyr::select(light_min31:light_min36))
    data$light_bin7  = rowSums(data %>% dplyr::select(light_min37:light_min42))
    data$light_bin8  = rowSums(data %>% dplyr::select(light_min43:light_min48))
    data$light_bin9  = rowSums(data %>% dplyr::select(light_min49:light_min54))
    data$light_bin10 = rowSums(data %>% dplyr::select(light_min55:light_min60))

    data <- data %>%
      dplyr::select(-(act_min1:light_min60))

  } else {

    stop("Haven't set up binning by anything other than six min bins.")
  }

  return(data)

}