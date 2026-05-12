#mac_row_col_params

mac_row_col_params <- list()
mac_row_col_params$max_temperature$param_1 <- 3
mac_row_col_params$max_temperature$param_2 <- 2

mac_row_col_params$temp_range$param_1 <- 4
mac_row_col_params$temp_range$param_2 <- 2

mac_row_col_params$total_precipitation$param_1 <- 3
mac_row_col_params$total_precipitation$param_2 <- 2

mac_row_col_params$daylength$param_1 <- 4
mac_row_col_params$daylength$param_2 <- 2

usethis::use_data(mac_row_col_params, overwrite = TRUE)
