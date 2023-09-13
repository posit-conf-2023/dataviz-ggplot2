#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                            Install Missing Packages                          #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) Workshop                           #
#                              September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#


## REQUIRED PACKAGES -----------------------------------------------------------

pkgs <- c("ggplot2", "dplyr", "tibble", "tidyr", "readr", "forcats", 
          "stringr", "lubridate", "purrr", "here", "scales", "ragg", 
          "systemfonts", "rcartocolor", "scico", "prismatic", "patchwork", 
          "ggtext", "ggforce", "ggrepel", "colorspace", "gapminder", "remotes")
install.packages(setdiff(pkgs, rownames(installed.packages())))

remotes::install_github("clauswilke/colorblindr")



## OPTIONAL PACKAGES -----------------------------------------------------------

pkgs_opt <- c("camcorder", "viridis", "RColorBrewer", "MetBrewer", 
              "ggthemes", "ggsci", "hrbrthemes", "tvthemes", 
              "ggannotate", "concavemen")
install.packages(setdiff(pkgs_opt, rownames(installed.packages())))

remotes::install_github("AllanCameron/geomtextpath")
