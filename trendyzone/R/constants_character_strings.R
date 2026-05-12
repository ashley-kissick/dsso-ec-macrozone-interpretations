
#character constants


#Months

#' months
#' @name months

months <- c(1:12)
usethis::use_data(months, overwrite = TRUE)


#' month_names
#' @name month_names

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct", "Nov", "Dec")
usethis::use_data(month_names, overwrite = TRUE)


#############################################

# general_colors



twentysix_colors <- colorRampPalette(brewer.pal(12, "Paired"))(26)
#pie(rep(1, 26), col = twentysix_colors)
#remove #24, it is too pale
colors_macrozones <- twentysix_colors[-24]
#pie(rep(1, 25), col = mac_cols)
usethis::use_data(colors_macrozones, overwrite = TRUE)





seq_255 <- seq(0, 255, by = 15)



my_gradient_func <- colorRampPalette(c("tomato1", "darkgoldenrod1", "palegreen", "royalblue"))
custom_colors <- my_gradient_func(15)
#scales::show_col(custom_colors)
usethis::use_data(custom_colors, overwrite = TRUE)


#' blues
#' @name blues
blues <- rgb(0, 128, seq_255, maxColorValue = 255)
blues <- adjustcolor(blues, alpha.f = 0.5)
#scales::show_col(blues)
usethis::use_data(blues, overwrite = TRUE)







#' viridis_modified
#' @name viridis_modified

#library(viridisLite)
#scales::show_col(viridis)


#my_palette <- colorRampPalette(c("red", "white", "blue"))
#gradient_colors <- my_palette(100)







