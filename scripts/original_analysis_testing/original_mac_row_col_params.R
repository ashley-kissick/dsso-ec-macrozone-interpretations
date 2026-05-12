#' mac_row_col_params
#' @usage Constants for plotting parameters for macrozone interpretations
#' @format A list with elements for each macrozone underlining feature.
#' 'param 1' is the number of rows. 'param_2' is the number of columns
#' @export


avg_max_temp_param <- list()
avg_max_temp_param$param_1 <- 2
avg_max_temp_param$param_2 <- 3

avg_temp_range_param <- list()
avg_temp_range_param$param_1 <- 2
avg_temp_range_param$param_2 <- 3

precip_param <- list()
precip_param$param_1 <- 4
precip_param$param_2 <- 3

daylength_param <- list()
daylength_param$param_1 <- 2
daylength_param$param_2 <- 3

mac_row_col_params <- list()
mac_row_col_params$avg_max_temp <- avg_max_temp_param
mac_row_col_params$avg_temp_range <- avg_temp_range_param
mac_row_col_params$precip_ <- precip_param
mac_row_col_params$daylength <- daylength_param

usethis::use_data(mac_row_col_params, overwrite = TRUE)
