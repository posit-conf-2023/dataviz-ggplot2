#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                           Fundamentals & Workflows                           #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) Workshop                           #
#                              September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#


## PACKAGES --------------------------------------------------------------------

pkgs <- c("ggplot2", "readr", "dplyr", "stringr", "lubridate", "here", 
          "scales","camcorder", "purrr", "patchwork")
install.packages(setdiff(pkgs, rownames(installed.packages())))



library(readr)
library(ggplot2)



## DATA IMPORT -----------------------------------------------------------------

bikes <-
  read_csv(
    here::here("data", "london-bikes.csv"),
    col_types = "Dcfffilllddddc"
  )

bikes



## A WALK-THROUGH EXAMPLE ------------------------------------------------------

# scatter plot of plot bikes$count versus bikes$temp_feel
ggplot(data = bikes) +              # initial call + data
  aes(x = temp_feel, y = count) +   # aesthetics
  geom_point()                      # geometric layer

# scatter plot of plot bikes$count versus bikes$temp_feel
ggplot(bikes, aes(x = temp_feel, y = count)) +
  geom_point()



ggplot(bikes, aes(x = temp_feel, y = count)) +
  geom_point() + 
  # add a GAM smoothing
  stat_smooth() # also: geom_smooth()



ggplot(bikes, aes(x = temp_feel, y = count, color = day_night)) + 
  geom_point() + 
  stat_smooth()



ggplot(bikes, aes(x = temp_feel, y = count)) + 
  # color mapping only applied to points
  geom_point(aes(color = day_night)) + 
  # invisible grouping to create two trend lines
  stat_smooth(aes(group = day_night))



ggplot(bikes, aes(x = temp_feel, y = count)) + 
  geom_point(
    aes(color = day_night), 
    # setting larger points with 50% opacity
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    aes(group = day_night), 
    # use linear fitting + draw black smoothing lines
    method = "lm", color = "black"
  )



ggplot(bikes, aes(x = temp_feel, y = count)) + 
  geom_point(
    aes(color = day_night), 
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    method = "lm", color = "black"
  ) +
  # small multiples
  facet_wrap(facets = vars(day_night)) # also: ~ day_night



ggplot(bikes, aes(x = temp_feel, y = count)) + 
  geom_point(
    aes(color = season), 
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    method = "lm", color = "black"
  ) +
  # small multiples
  facet_grid(
    rows = vars(day_night), cols = vars(year) # also: day_night ~ year
  )



ggplot(bikes, aes(x = temp_feel, y = count)) + 
  geom_point(
    aes(color = season), 
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    method = "lm", color = "black"
  ) +
  facet_grid(
    day_night ~ year, 
    # free y axis range
    scales = "free_y", 
    # scale heights proportionally
    space = "free_y"
  )



g1 <- 
  ggplot(bikes, aes(x = temp_feel, y = count)) + 
  geom_point(
    aes(color = season), 
    alpha = .5, size = 1.5
  ) + 
  stat_smooth(
    method = "lm", color = "black"
  ) +
  facet_grid(
    day_night ~ year, 
    scales = "free_y", 
    space = "free_y"
  )



g2 <- g1 +
  # x axis
  scale_x_continuous(
    # add °C symbol
    labels = function(x) paste0(x, "°C"), 
    # use 5°C spacing
    breaks = -1:6*5  # also: seq(-5, 30, by = 5)
  ) +
  # y axis
  scale_y_continuous(
    # add a thousand separator
    labels = scales::label_comma(), 
    # use consistent spacing across rows
    breaks = 0:5*10000
  )

g2



g2 +
  # use a custom color palette for season colors
  scale_color_manual(
    values = c("#6681FE", "#1EC98D", "#F7B01B", "#A26E7C")
  )



# use a named vector for explicit matching
colors <- c(
  `0` = "#1EC98D",
  `1` = "#F7B01B",
  `2` = "#A26E7C",
  `3` = "#6681FE"
)

g2 +
  scale_color_manual(
    values = colors
  )



g2 +
  scale_color_manual(
    values = colors,
    # overwrite legend keys
    labels = c("Winter", "Spring", "Summer", "Autumn")
  )


g3 <- g2 +
  scale_color_manual(
    values = colors,
    labels = c("Winter", "Spring", "Summer", "Autumn")
  ) +
  labs(
    # overwrite axis and legend titles
    x = "Average feels-like temperature", y = NULL, color = NULL,
    # add plot title and caption
    title = "Trends of Reported Bike Rents versus Feels-Like Temperature in London",
    caption = "Data: TfL (Transport for London), Jan 2015–Dec 2016"
  )

g3



g3 +
  # add theme with a custom font + larger element sizes
  theme_light(
    base_size = 15, base_family = "Spline Sans"
  )



g4 <- g3 +
  theme_light(base_size = 15, base_family = "Spline Sans") +
  # theme adjustments
  theme(
    plot.title.position = "plot", # left-align title 
    plot.caption.position = "plot", # right-align caption
    legend.position = "top", # place legend above plot
    plot.title = element_text(face = "bold", size = rel(1.4)), # larger, bold title
    axis.text = element_text(family = "Spline Sans Mono"), # monospaced font for axes
    axis.title.x = element_text( # left-aligned, grey x axis label
      hjust = 0, color = "grey20", margin = margin(t = 12)
    ),
    legend.text = element_text(size = rel(1)), # larger legend labels
    strip.text = element_text(face = "bold", size = rel(1.15)), # larger, bold facet labels
    panel.grid.major.x = element_blank(), # no vertical major lines
    panel.grid.minor = element_blank(), # no minor grid lines
    panel.spacing.x = unit(20, "pt"), # increase white space between panels
    panel.spacing.y = unit(10, "pt"), # increase white space between panels
    plot.margin = margin(rep(15, 4)) # adjust white space around plot
  )

g4



g4 +
  # adjust symbol size in legend
  guides(
    color = guide_legend(override.aes = list(size = 4))
  )



g4 +
  scale_color_manual(
    values = colors,
    labels = c("Winter", "Spring", "Summer", "Autumn"),
    # adjust symbol size in legend size
    guide = guide_legend(override.aes = list(size = 4))
  )



## SAVING PLOTS ---------------------------------------------------------------

ggsave(filename = "my_plot.png", plot = g)

ggsave("my_plot.png")

ggsave("my_plot.png", width = 6, height = 5, dpi = 600)

ggsave("my_plot.png", width = 6*2.54, height = 5*2.54, unit = "cm", dpi = 600)

ggsave("my_plot.png", device = agg_png)

ggsave("my_plot.pdf", device = cairo_pdf)

ggsave("my_plot.svg")



## WORKING WITH ASPECT RATIOS --------------------------------------------------

camcorder::gg_record(
  dir = here::here("temp"),  # path for plot files
  device = "png",            # device to use
  width = 10,                # figure width
  height = 5,                # figure height
  dpi = 600                  # plot resolution
)

g <- ggplot(bikes, aes(x = temp, y = count, color = day_night)) +
  geom_point(alpha = .3, size = 2) +
  scale_color_manual(values = c(day = "#FFA200", night = "#757BC7")) +
  theme_minimal(base_size = 14, base_family = "Asap SemiCondensed") +
  theme(panel.grid.minor = element_blank())

g

camcorder::gg_resize_film(width = 20) # update figure width

g



## SET THEME GLOBALLY ---------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "Asap SemiCondensed"))
theme_update(panel.grid.minor = element_blank())



## PROGRAMMING WITH GGPLOT2 ----------------------------------------------------

smooth <- TRUE

ggplot(bikes, aes(x = temp, y = humidity)) +
  { if(smooth) geom_smooth(color = "red") } +
  geom_point(alpha = .5)



smooth <- FALSE

ggplot(bikes, aes(x = temp, y = humidity)) +
  { if(smooth) geom_smooth(color = "red") } +
  geom_point(alpha = .5)



draw_scatter <- function(smooth = TRUE) {
  ggplot(bikes, aes(x = temp, y = humidity)) +
    { if(smooth) geom_smooth(color = "red") } +
    geom_point(alpha = .5)
}

draw_scatter()

draw_scatter(smooth = FALSE)



geom_scatterfit <- function(pointsize = 1, pointalpha = 1, 
                            method = "lm", linecolor = "red", ...) {
  list(
    geom_point(size = pointsize, alpha = pointalpha, ...),
    geom_smooth(method = method, color = linecolor, ...)
  )
}

ggplot(bikes,
       aes(x = humidity, y = count)) +
  geom_scatterfit()

ggplot(bikes,
       aes(x = humidity, y = count)) +
  geom_scatterfit(
    color = "#28A87D", 
    linewidth = 3
  )

ggplot(diamonds, 
       aes(x = carat, y = price)) +
  geom_scatterfit(
    pointsize = .5, 
    pointalpha = .1,
    method = "gam",
    linecolor = "#EFAC00"
  )



scales_log <- function(sides = "xy") {
  list(
    if(stringr::str_detect(sides, "x")) {
      scale_x_log10(
        breaks = c(10^(1:100)), labels = scales::label_log()
      )
    },
    if(stringr::str_detect(sides, "y")) {
      scale_y_log10(
        breaks = c(10^(1:100)), labels = scales::label_log()
      )
    }
  )
}

ggplot(diamonds, 
       aes(x = carat, y = price)) +
  geom_scatterfit(
    pointsize = .5, 
    pointalpha = .1,
    method = "gam",
    linecolor = "#EFAC00"
  ) +
  scales_log(sides = "y")



trends_monthly <- function(grp = "January") {
  bikes |> 
    dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) |> 
    dplyr::filter(month %in% grp) |> 
    ggplot(aes(x = temp, y = count, color = day_night)) +
    geom_point(alpha = .2, show.legend = FALSE) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = c("#FFA200", "#757bc7")) +
    labs(title = grp, x = "Temperature", y = "Bike shares", color = NULL)
}

trends_monthly("July")



trends_monthly <- function(grp = "January") {
  bikes |> 
    dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) |> 
    dplyr::filter(month %in% grp) |> 
    ggplot(aes(x = temp, y = count, color = day_night)) +
    geom_point(alpha = .2, show.legend = FALSE) +
    geom_smooth(se = FALSE) +
    # keep axis ranges consistent
    scale_x_continuous(limits = range(bikes$temp)) +
    scale_y_continuous(limits = range(bikes$count)) +
    scale_color_manual(values = c("#FFA200", "#757bc7")) +
    labs(title = grp, x = "Temperature", y = "Bike shares", color = NULL)
}

trends_monthly("July")



plots <- purrr::map(month.name[1:12], trends_monthly) # also: ~ trends_monthly(.x)

plots[[9]]

patchwork::wrap_plots(plots)



plot_density <- function(data, var, grp = "") {
  ggplot(data, aes(x = !!sym(var))) +
    geom_density(aes(fill = !!sym(grp)), position = "identity",
                 color = "grey30", alpha = .3) +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_y_continuous(labels = scales::label_number()) +
    scale_fill_brewer(palette = "Dark2", name = NULL) +
    theme(legend.position = "top")
}

plot_density(
  bikes, "count"
)

plots <- purrr::map(
  c("count", "temp", "humidity", "wind_speed"), 
  ~ plot_density(data = bikes, var = .x, grp = "day_night")
)

patchwork::wrap_plots(plots, nrow = 1)

plots <- purrr::map(
  names(dplyr::select(midwest, where(is.numeric))),
  ~plot_density(data = midwest, var = .x)
)

patchwork::wrap_plots(plots)



## COMBINE PLOTS ---------------------------------------------------------------

library(patchwork)

p1 <- plot_density(data = bikes, var = "count", grp = "day_night")

p2 <- plot_density(data = bikes, var = "humidity", grp = "day_night")

p3 <- ggplot(bikes, aes(x = humidity, y = count)) + geom_scatterfit(pointalpha = .3)



(p1 + p2) / p3



(p1 + p2) / p3 + plot_layout(heights = c(1, 2))



(p1 + p2) / p3 + plot_layout(heights = c(1, 2), guides = "collect")



(p1 + p2) / p3 + plot_layout(heights = c(1, 2), guides = "collect") +
  plot_annotation(theme = theme(legend.justification = "top"))



(p1 + p2) / p3 + plot_layout(heights = c(1, 2), guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ".", theme = theme(legend.justification = "top"))


##----------------------------------------------------------------------------##
## THAT'S IT FOLKS! ----------------------------------------------------------##
##----------------------------------------------------------------------------##