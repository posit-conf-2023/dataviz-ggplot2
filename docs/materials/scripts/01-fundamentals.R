#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                           Fundamentals & Workflows                           #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) workshop                           #
#.                             September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#

## PACKAGES --------------------------------------------------------------------

library(readr)
library(ggplot2)


## DATA IMPORT -----------------------------------------------------------------

bikes <-
  read_csv(
    here::here("data", "london-bikes.csv"),
    col_types = "Dcfffilllddddc"
  )

bikes


## A DEFAULT GGPLOT ------------------------------------------------------------

## scatter plot of plot bikes$count versus bikes$temp_feel
ggplot(data = bikes) +              ## initial call + data
  aes(x = temp_feel, y = count) +   ## aesthetics
  geom_point()                      ## geometric layer


## A WALK-THROUGH EXAMPLE ------------------------------------------------------

## create named color vector
colors <- c(
  `0` = "#1EC98D",
  `1` = "#F7B01B",
  `2` = "#A26E7C",
  `3` = "#6681FE"
)

## scatter plot of plot bikes$count versus bikes$temp_feel
ggplot(bikes, aes(x = temp_feel, y = count)) +
  ## add points
  geom_point(
    ## color mapping only applied to points
    aes(color = season),
    ## setting larger points with 50% opacity
    alpha = .5, size = 1.5
  ) +
  ## add a smoothing
  stat_smooth(  ## also: geom_smooth()
    ## use linear fitting + draw black smoothing lines
    method = "lm", color = "black"
  ) +
  ## small multiples
  facet_grid(
    day_night ~ year,  ## also: vars(day_night), vars(year)
    ## free y axis range
    scales = "free_y",
    ## scale heights proportionally
    space = "free_y"
  ) +
  ## x axis
  scale_x_continuous(
    ## add °C symbol
    labels = function(x) paste0(x, "°C"),
    ## use 5°C spacing
    breaks = -1:6*5  ## also: seq(-5, 30, by = 5)
  ) +
  ## y axis
  scale_y_continuous(
    ## add a thousand separator
    labels = scales::label_comma(),
    ## use consistent spacing across rows
    breaks = 0:5*10000
  ) +
  ## colors
  scale_color_manual(
    ## use a custom color palette
    values = colors,
    ## and overwrite legend keys
    labels = c("Winter", "Spring", "Summer", "Autumn"),
    ## adjust symbol size in legend size
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  labs(
    ## overwrite axis and legend titles
    x = "Average feels-like temperature", y = NULL, color = NULL,
    ## add plot title and caption
    title = "Trends of Reported Bike Rents versus Feels-Like Temperature in London",
    caption = "Data: TfL (Transport for London), Jan 2015–Dec 2016"
  ) +
  ## add theme with a custom font + larger element sizes
  theme_light(
    base_size = 15, base_family = "Spline Sans"
  ) +
  ## theme adjustments
  theme(
    plot.title.position = "plot", ## left-align title
    plot.caption.position = "plot", ## right-align caption
    legend.position = "top", ## place legend above plot
    plot.title = element_text(face = "bold", size = rel(1.4)), ## larger, bold title
    axis.text = element_text(family = "Spline Sans Mono"), ## monospaced font for axes
    axis.title.x = element_text( ## left-aligned, grey x axis label
      hjust = 0, color = "grey20", margin = margin(t = 12)
    ),
    legend.text = element_text(size = rel(1)), ## larger legend labels
    strip.text = element_text(face = "bold", size = rel(1.15)), ## larger, bold facet labels
    panel.grid.major.x = element_blank(), ## no vertical major lines
    panel.grid.minor = element_blank(), ## no minor grid lines
    panel.spacing.x = unit(20, "pt"), ## increase white space between panels
    panel.spacing.y = unit(10, "pt"), ## increase white space between panels
    plot.margin = margin(rep(15, 4)) ## adjust white space around plot
  )


## SAVING PLOTS -----------------------------------------------------------------

ggsave(filename = "my_plot.png", plot = g)

ggsave("my_plot.png")


ggsave("my_plot.png", width = 6, height = 5, dpi = 600)

ggsave("my_plot.png", width = 6*2.54, height = 5*2.54, unit = "cm", dpi = 600)


ggsave("my_plot.png", device = agg_png)

ggsave("my_plot.pdf", device = cairo_pdf)

ggsave("my_plot.svg")


camcorder::gg_record(
  dir = here::here("temp"),  ## path for plot files
  device = "png",            ## device to use
  width = 10,                ## figure width
  height = 5,                ## figure height
  dpi = 600                  ## plot resolution
)

g <- ggplot(bikes, aes(x = temp, y = count, color = day_night)) +
  geom_point(alpha = .3, size = 2) +
  scale_color_manual(values = c(day = "#FFA200", night = "#757BC7")) +
  theme_minimal(base_size = 18, base_family = "Asap SemiCondensed") +
  theme(panel.grid.minor = element_blank())

g

camcorder::gg_resize_film(width = 20) ## update figure width

g
