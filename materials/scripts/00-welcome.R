#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                               Preparation Steps                              #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) workshop                           #
#.                             September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#

## Required Packages
pkgs <- c("ggplot2", "dplyr", "tibble", "tidyr", "readr", "forcats", "stringr", 
          "lubridate", "here", "scales", "ragg", "systemfonts", "rcartocolor", 
          "scico", "prismatic", "patchwork", "ggtext", "ggforce", "ggrepel")
unavailable <- setdiff(pkgs, rownames(installed.packages()))
install.packages(unavailable)


## Optional Packages
pkgs_opt <- c("viridis", "RColorBrewer", "MetBrewer", "ggthemes", "ggsci", 
              "camcorder", "colorspace", "remotes")
unavailable <- setdiff(pkgs_opt, rownames(installed.packages()))
install.packages(unavailable)

remotes::install_github("clauswilke/colorblindr")