#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                               Working with Text                              #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) Workshop                           #
#                              September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#


## PREPARATION -----------------------------------------------------------------

pkgs <- c("ggplot2", "readr", "dplyr", "here", "RColorBrewer", "viridis", 
          "rcartocolor", "scico", "ggsci", "ggthemes", "MetBrewer", 
          "colorspace", "remotes")
install.packages(setdiff(pkgs, rownames(installed.packages())))

if (!"colorblindr" %in% rownames(installed.packages())) {
  remotes::install_github("clauswilke/colorblindr")
}



library(ggplot2)
library(dplyr)



bikes <- readr::read_csv(
  here::here("data", "london-bikes-custom.csv"), 
  col_types = "Dcfffilllddddc"
)



theme_set(theme_light(base_size = 14, base_family = "Asap SemiCondensed"))

theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot"
)



## DEFAULT COLOR PALETTES ------------------------------------------------------

ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot()



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7)



## PRE-DEFINED COLOR PALETTES --------------------------------------------------

ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_viridis_d()



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_viridis_d(
    begin = .3,
    end = .9
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_viridis_c()



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_viridis_c(
    option = "cividis"
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_viridis_c(
    option = "cividis",
    end = .95,
    direction = -1
  )



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_brewer()



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_distiller()



RColorBrewer::display.brewer.all()

RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_brewer(
    palette = "Set1"
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_distiller(
    palette = "YlOrRd",
    direction = 1
  )



## PALETTE PACKAGES ------------------------------------------------------------

ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  rcartocolor::scale_fill_carto_d()



rcartocolor::display_carto_all()

rcartocolor::display_carto_all(colorblind_friendly = TRUE)



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  rcartocolor::scale_fill_carto_d(
    palette = "Safe"
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scico::scale_color_scico()



scico::scico_palette_show()



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scico::scale_color_scico(
    palette = "brocO",
    direction = -1
  )



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  ggsci::scale_fill_npg()



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  ggthemes::scale_fill_gdocs()



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  MetBrewer::scale_fill_met_d(
    name = "Klimt"
  )



MetBrewer::display_all()

MetBrewer::display_all(colorblind_only = TRUE)



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  MetBrewer::scale_color_met_c(
    name = "Hiroshige" 
  )



## COLOR PALETTE QUALITY -------------------------------------------------------

colorspace::specplot(
  colorspace::diverging_hcl(
    n = 100, palette = "Blue-Red"
  )
)



colorspace::specplot(
  MetBrewer::met.brewer(
     n = 100, name = "Hiroshige"
  )
)



colorspace::specplot(
  MetBrewer::met.brewer(
     n = 100, name = "Cassatt2"
  )
)



colorspace::specplot(
  MetBrewer::met.brewer(
     n = 100, name = "Veronese"
  )
)



colorspace::specplot(
  viridis::viridis(
    n = 100, direction = -1
  )
)



colorspace::specplot(
  MetBrewer::met.brewer(
     n = 100, name = "Hokusai3"
  )
)



colorspace::specplot(
  rainbow(
    n = 100
  )
)



colorspace::specplot(
  viridis::turbo(
     n = 100, direction = -1
  )
)



## CUSTOMIZE PALETTES ----------------------------------------------------------

ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  rcartocolor::scale_fill_carto_d(
    palette = "Vivid" 
  )



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = rcartocolor::carto_pal(
      name = "Vivid", n = 4
    )
  )



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = rcartocolor::carto_pal(
      name = "Vivid", n = 5
    )[1:4]
  )



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = rcartocolor::carto_pal(
      name = "Vivid", n = 7
    )[c(2, 6, 1, 3)]
  )



carto_custom <- 
  rcartocolor::carto_pal(
    name = "Vivid", n = 7
  )[c(2, 6, 1, 3)]

ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = carto_custom
  )



library(prismatic)

carto_light <- clr_lighten(
  carto_custom, .7
)



ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = carto_light
  )



ggplot(bikes, 
       aes(x = day_night, y = count)) +
  geom_boxplot(
    aes(fill = season,
        fill = after_scale(
          clr_lighten(fill, .7)
    ))
  ) +
  scale_fill_manual(
    values = carto_custom
  )

ggplot(bikes, 
       aes(x = day_night, y = count)) +
  geom_boxplot(
    aes(fill = stage(
      season, 
      after_scale = clr_lighten(fill, .7)
    ))
  ) +
  scale_fill_manual(
    values = carto_custom
  )



ggplot(bikes, 
       aes(x = day_night, y = count)) +
  geom_boxplot(
    aes(color = season,
        fill = after_scale(
          clr_lighten(color, .7)
    ))
  ) +
  scale_color_manual(
    values = carto_custom
  )


ggplot(bikes,
       aes(x = day_night, y = count)) +
  geom_boxplot(
    aes(color = season,
        fill = after_scale(
          clr_lighten(color, .7)
    )),
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = season,
        color = after_scale(
          clr_darken(color, .4)
    )), 
    position = position_jitterdodge(
      dodge.width = .75, 
      jitter.width = .2
    ),
    alpha = .3, size = .6
  ) +
  scale_color_manual(
    values = carto_custom
  ) +
  theme(legend.position = "top")



## CREATE NEW PALETTES ---------------------------------------------------------

ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradient(
    low = "#1D785A",
    high = "#FFCE52"
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradient2(
    low = "#663399",
    high = "#993334",
    mid = "grey85"
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradient2(
    low = "#663399",
    high = "#993334",
    mid = "grey85",
    midpoint = mean(bikes$humidity)    
  )



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = temp)) +
  geom_point(alpha = .7) +
  scale_color_gradient2(
    low = "#663399",
    high = "#993334",
    mid = "grey85",
    midpoint = 10,
    limits = c(-10, 30)
  )



colorspace::specplot(
  colorRampPalette(
    c("#1D785A", "#FFCE52")
  )(100)
)



colorspace::specplot(
  colorRampPalette(
    c("#663399", "grey85", "#993334")
  )(100)
)



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradientn(
    colors = carto_custom  
  )



pal_c <- 
  colorRampPalette(
    c("#663399", "grey85", "#993334")
  )(5)

plot(color(pal_c))



ggplot(filter(bikes, is_weekend == TRUE), 
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradientn(
    colors = pal_c,
    values = c(0, .05, .2, .8, .95, 1)
  )



## BUILD YOUR OWN COLOR SCALES -------------------------------------------------

dubois_colors <- function(...) {
  
  # define colors
  dubois_cols <- c(
    `black`    = "#000000",
    `purple`   = "#582f6c",
    `violet`   = "#94679C",
    `pink`     = "#ef849f",
    `softred`  = "#f4b7a7",
    `iceblue`  = "#bccbf3",
    `palegrey` = "#e4e4e4"
  )

  # if no colors are specified, return all
  cols <- c(...)
  if (is.null(cols))  return (dubois_cols)

  # if colors are specified, return those
  dubois_cols[cols]
}



plot(color(dubois_colors()))

plot(color(dubois_colors("black", "violet", "softred", "iceblue", "palegrey")))



dubois_pal_d <- function(palette = "default", reverse = FALSE) {
  
  # nested function to return colors via `dubois_pal_d()(n)`
  function(n) {
    
    # check if number of colors is sufficient
    if(n > 5) stop('Palettes only contain 5 colors')
  
    # check arguments
    if (!palette %in% c("default", "dark", "light")) stop('palette should be "default", "dark" or "light".')
    if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')

    # define palette styles
    if (palette == "default") { pal <- dubois_colors("black", "violet", "softred", "iceblue", "palegrey")[1:n] }
    if (palette == "dark") { pal <- dubois_colors(1:5)[1:n] }
    if (palette == "light") { pal <- dubois_colors(3:7)[1:n] }
    
    # return unnamed vector of color codes
    pal <- unname(pal)

    # check reverse argument
    if (reverse) rev(pal) else pal
  }
}



plot(color(dubois_pal_d()(5)))

plot(color(dubois_pal_d(palette = "dark", reverse = TRUE)(5)))



scale_color_dubois_d <- function(palette = "default", reverse = FALSE, ...) {
  
  # check arguments
  if (!palette %in% c("default", "dark", "light")) stop('palette should be "default", "dark" or "light".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')

  # retrieve color set
  pal <- dubois_pal_d(palette = palette, reverse = reverse)

  # apply to discrete scale
  ggplot2::discrete_scale("colour", paste0("dubois_", palette), palette = pal, ...)
}



scale_fill_dubois_d <- function(palette = "default", reverse = FALSE, ...) {
  
  if (!palette %in% c("default", "dark", "light")) stop('palette should be "default", "dark" or "light".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')

  pal <- dubois_pal_d(palette = palette, reverse = reverse)

  ggplot2::discrete_scale("fill", paste0("dubois_", palette), palette = pal, ...)
}



ggplot(bikes, 
       aes(y = weather_type, 
           fill = season)) +
  geom_bar(position = "fill") +
  scale_fill_dubois_d(
    reverse = TRUE,
    name = NULL
  ) +
  theme(legend.position = "top")



dubois_pal_c <- function(palette = "dark", reverse = FALSE, ...) {
  
  # check arguments
  if (!palette %in% c("dark", "light")) stop('palette should be "dark" or "light".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')
  
  # define palette styles
  dubois_palettes <- list(
    `dark`    = dubois_colors("black", "purple", "violet", "pink"),
    `light`   = dubois_colors("purple", "violet", "pink", "palegrey")
  )

  # retrieve color set as unnamed vector
  pal <- dubois_palettes[[palette]]
  pal <- unname(pal)

  # check reverse argument
  if (reverse) pal <- rev(pal)

  # create a color gradient with n colors
  grDevices::colorRampPalette(pal, ...)
}



plot(color(dubois_pal_c()(50)))

plot(color(dubois_pal_c(palette = "light", reverse = TRUE)(7)))



scale_color_dubois_c <- function(palette = "dark", reverse = FALSE, ...) {
  
  # check function arguments
  if (!palette %in% c("dark", "light")) stop('Palette should be "dark" or "light".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric.')

  # apply color set to ggplot's gradientn scale
  pal <- dubois_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}



scale_fill_dubois_c <- function(palette = "dark", reverse = FALSE, ...) {
  
  if (!palette %in% c("dark", "light")) stop('Palette should be "dark" or "light".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric.')

  pal <- dubois_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_dubois_c()



ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_dubois_c(
    palette = "light",
    reverse = TRUE
  )

colorspace::specplot(
  dubois_pal_c()(100)
)



colorspace::specplot(
  dubois_pal_c(palette = "light")(100)
)



## DESIGN COLORBLIND-FRIENDLY GRAPHICS -----------------------------------------

deut <- 
  prismatic::clr_deutan(
    dubois_pal_c()(100)
  )

ggplot(filter(bikes, is_weekend == TRUE),
       aes(x = temp_feel, y = count, 
           color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_gradientn(
    colors = deut
  )



g1 <- 
  ggplot(filter(bikes, is_weekend == TRUE),
         aes(x = temp_feel, y = count, 
             color = humidity)) +
  geom_point(alpha = .7) +
  scale_color_dubois_c()

g1



colorblindr::cvd_grid(g1)



g2 <- 
  ggplot(bikes, 
       aes(x = day_night, y = count, 
           fill = season)) +
  geom_boxplot() +
  scale_fill_manual(
    values = carto_custom
  )

g2



colorblindr::cvd_grid(g2)



g3 <- 
  ggplot(bikes, 
         aes(x = temp_feel, y = count,
             color = season)) + 
  geom_point(
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    aes(group = day_night),
    method = "lm", color = "black"
  ) +
  scale_color_manual(
    values = c("#6681FE", "#1EC98D", "#F7B01B", "#A26E7C")
  )

g3



colorblindr::cvd_grid(g3)


##----------------------------------------------------------------------------##
## THAT'S IT FOLKS! ----------------------------------------------------------##
##----------------------------------------------------------------------------##