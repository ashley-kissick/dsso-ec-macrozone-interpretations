#' Colors for product segment visualizations
#' i.e., maps, barplots, boxplots, 2D visualizations of clusters

#' color_list
#' @usage The colors used for product segmentation visualization for reports
#' @format A list with two elements for summer and safrinha.
#' @export

color_list <- list()

color_list$summer_colors <- c("#d1495b", "#ED8141", "gold", "#99CC66", "steelblue", "#99CCFF", "azure3", "#9590FF")
color_list$safrinha_colors <- c("#d1495b", "#ED8141", "gold", "#99CC66", "steelblue",
                                "#99CCFF", "#9590FF", "bisque2",  "azure3",  "palevioletred2")
usethis::use_data(color_list, overwrite = TRUE)

#Visualize:
slice_values <- rep(1/length(color_list$safrinha_colors), length = length(color_list$safrinha_colors))
#pie(slice_values, labels = color_list$safrinha_colors , col = color_list$safrinha_colors ,
#    main = "safrinha colors")


#' colors_macrozones
#' @usage the colors used in macrozone visualizations
#' @format A character vector
#' @export

#Generate some colors to use consistently with macrozones and their visual presentation
#library(colortools)
#darks <- wheel("steelblue")
#earths <- wheel("lightsalmon4")
#brights <- wheel("tan1")
#lights <- wheel("palegoldenrod")
#pales <- wheel("thistle")


#Manually removed colors that are too similar

#LIGHTS
#pastels <- c(lights, pales)
#slice_values <- rep(1/length(pastels ), length = length(pastels ))
#pie(slice_values, labels = pastels , col = pastels , main = "example colors")

pastels_keep <- c("#EEE8AA", "#D2EEAA", "#B0EEAA", "#AAEEE8", "#AAD2EE", "#AAB0EE", "#C6AAEE", "#E8AAEE",
                  "#EEAAB0", "#EEC6AA", "#D8BFBF", "#D8D8BF", "#BFD8BF", "#BFD8D8", "#BFBFD8")

#to visualize
#slice_values <- rep(1/length(pastels_keep), length = length(pastels_keep))
#pie(slice_values, labels = pastels_keep, col = pastels_keep, main = "pastel colors")


#DARKS
#deeps <- c(darks, earths)
#slice_values <- rep(1/length(deeps), length = length(deeps))
#pie(slice_values, labels = deeps, col = deeps, main = "example colors")

deeps_keep <- c("#4682B4", "#464BB4", "#AF46B4", "#B4464B", "#B47846", "#B4AF46",
                "#46B4AF", "#8B5742", "#518B42", "#42768B", "#57428B", "#8B4276")

#to visualize
#slice_values <- rep(1/length(deeps_keep), length = length(deeps_keep))
#pie(slice_values, labels = deeps_keep, col = deeps_keep, main = "example colors")


#COMBINE and CHECK
all_colors <- c(brights, deeps_keep, pastels_keep, "grey")

#to visualize
#slice_values <- rep(1/length(all_colors), length = length(all_colors))
#pie(slice_values, labels = all_colors, col = all_colors, main = "example colors")

#Randomize the colors:
set.seed(2020)
colors_macrozones <- sample(all_colors, 40)
usethis::use_data(colors_macrozones, overwrite = TRUE)

#to visualize
#slice_values <- rep(1/length(colors_macrozones), length = length(colors_macrozones))
#pie(slice_values, labels = colors_macrozones, col = colors_macrozones, main = "example colors")







#' colors_environment
#' @usage the colors used in macrozone environment visualizations
#' @format A character vector
#' @export

env_cols <- c("#EEE8AA", "#D2EEAA", "#B0EEAA", "lightcyan2", "#AAEEE8", "#AAD2EE",
          "#AAB0EE", "#C6AAEE", "#EEAAD2", "thistle1", "#EEAAB0", "#EEC6AA")
slice_values <- rep(1/length(env_cols), length = length(env_cols))
#pie(slice_values, labels = env_cols, col = env_cols, main = "example colors")
usethis::use_data(env_cols, overwrite = TRUE)






#' ftn_colors
#' @usage the colors used in temporal feature trends
#' @format A character vector
#' @export

ftn_colors <- c("#EEC6AA", "#EEE8AA", "#D2EEAA", "#B0EEAA", "lightcyan2", "#AAEEE8",
                "#AAD2EE", "#AAB0EE", "#C6AAEE", "#EEAAD2")
slice_values <- rep(1/length(ftn_colors), length = length(ftn_colors))
#pie(slice_values, labels = ftn_colors, col = ftn_colors, main = "example colors")
usethis::use_data(ftn_colors, overwrite = TRUE)

#######################################
#####  CODE FOR SELECTING COLOR SCHEMES FOR PRODUCT SEGMENTS

#POSSIBLE COLORS:
#Gold, grey, blue, orange, purple, teal, light-blue, brick

#LOOKING ONLINE:
#red:  #d1495b
#gold: #CF9400
#green: #66a182, "#99CC66, #85AD00
#blue:  "#99CCFF"
#orange:  "#FF9933" or "#FF9966", "#ED8141
#purple:  "#9966FF", "9590FF
#darkblue:  "#0066CC"
#teal:  "#00CC99" or "#33CC99"
#brick:  "#993333"
#light blue:  "#00BDD0, #00BBDB
#coral:  #F8766D

#Less favorite
#no_colors <- c("slateblue3", "#993333", "#F8766D", "orange", "#00BDD0", "#66a182", "#0066CC", "#FF9933", "#3355CC", "gray52")

#my_cols <- c("#d1495b", "#ED8141", "gold",    "#99CC66", "steelblue", "#99CCFF", "azure3", "#9590FF")
#slice_values <- rep(100/length(my_cols), length = length(my_cols))
#pie(slice_values, labels = my_cols, col = my_cols, main = "example colors")

# "#3366FF"  #ok better
# "#FF6600"  #ok
# "#FFCC99"
# "tan1"  I like
# "steelblue"
# "skyblue2"

#library(colortools)
#complementary("tan1")
#wheel("tan1")
#analogous("tan1")


#Old colors for reference ONLY:
#color_list <- list()
#color_list$summer_colors <- c("yellow", "blue", "deeppink2", "cadetblue1",
#                              "chartreuse1", "pink", "orange", "blueviolet")
#color_list$safrinha_colors <- c("deeppink2", "blueviolet", "chartreuse1", "cadetblue1",
#                                "orange", "yellow")






