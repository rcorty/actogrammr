#' @title read_clock_lab_files
#'
#' @param file_names the names of the files to read.
#'   Should be the result of a call to list.files(..., full.names = TRUE)
#'
#' @return a big data.frame
#' @export
#'
read_clock_lab_files <- function(file_names) {

  mouse_data <- list()
  for (file_name in file_names) {

    mouse_data[[file_name]] <- read_clock_lab_file(file_name = file_name)

  }


  return(do.call(what = rbind,
                 args = mouse_data))
}




read_clock_lab_file <- function(file_name) {

  file_name_byte_length <- 4
  bytes_per_hours_data <- 172

  file_name_offset_start <- 4
  file_name_offset_end <- 23

  date_offset_start <- 28
  date_offset_end <- 37

  hour_offset <- 42

  counts_offset_start <- 48
  counts_offset_end <- 107

  light_offset_start <- 112
  light_offset_end <- 171

  data <- readr::read_file_raw(file = file_name)

  data_length <- readBin(con = data[1:4],
                         what = 'integer',
                         size = file_name_byte_length,
                         endian = 'big')

  num_hours <- num_hours <- (data_length - file_name_byte_length)/bytes_per_hours_data

  filenames <- dates <- hours <- acts <- lights <- list()

  for (hour in 1:num_hours) {

    hour_start <- (hour - 1)*bytes_per_hours_data + 5

    filenames[[hour]] <- readBin(con = data[hour_start + (file_name_offset_start:file_name_offset_end)],
                                 what = 'character')

    dates[[hour]] <- readBin(con = data[hour_start + (date_offset_start:date_offset_end)],
                             what = 'character')

    hours[[hour]] <- readBin(con = data[hour_start + hour_offset],
                             what = 'integer',
                             n = 1,
                             size = 1)

    acts[[hour]] <- readBin(con = data[hour_start + (counts_offset_start:counts_offset_end)],
                            what = 'integer',
                            n = 60,
                            size = 1)

    lights[[hour]] <- readBin(con = data[hour_start + (light_offset_start:light_offset_end)],
                              what = 'integer',
                              n = 60,
                              size = 1)
  }

  stopifnot(all(is.character(unlist(x = filenames))))
  stopifnot(length(unique(unlist(x = filenames))) == 1)

  acts <- do.call(what = rbind, args = acts)
  colnames(acts) <- paste0('act_', 1:60)

  lights <- do.call(what = rbind, args = lights)
  colnames(lights) <- paste0('light_', 1:60)

  wide_data <- cbind(data.frame(file_name = file_name,
                                date = lubridate::mdy(unlist(dates)),
                                hour = unlist(hours)),
                     acts,
                     lights)

  long_act <- wide_data %>%
    dplyr::select(-dplyr::matches('light')) %>%
    tidyr::gather(key = min,
                  value = act,
                  act_1:act_60)

  long_light <- wide_data %>%
    dplyr::select(-dplyr::matches('act')) %>%
    tidyr::gather(key = min,
                  value = light,
                  light_1:light_60)

  long_act %>%
    dplyr::as_data_frame() %>%
    tidyr::separate(col = min, into = c('trash', 'min')) %>%
    dplyr::select(-trash) %>%
    dplyr::mutate(min = as.integer(min),
                  light = long_light$light) %>%
    return()

}


