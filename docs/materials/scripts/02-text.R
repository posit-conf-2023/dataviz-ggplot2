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

pkgs <- c("ggplot2", "readr", "dplyr", "stringr", "lubridate", "here", 
          "scales", "ggtext", "ggrepel", "ggforce", "ggannotate")
install.packages(setdiff(pkgs, rownames(installed.packages())))

if (!"geomtextpath" %in% rownames(installed.packages())) {
  remotes::install_github("AllanCameron/geomtextpath")
}



library(ggplot2)
library(dplyr)
library(stringr)



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



g <- ggplot(
    bikes,
    aes(x = temp, y = count,
        color = season)
  ) +
  geom_point(
    alpha = .5
  ) +
  labs(
    x = "Temperature (°C)",
    y = "Reported bike shares",
    title = "TfL bike sharing trends",
    subtitle = "Reported bike rents versus air temperature in London",
    caption = "Data: TfL",
    color = "Season:",
    tag = "1."
  )

g



## LABELS + THEME() ------------------------------------------------------------

g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot"
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d"
  )
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Spline Sans Mono",
    face = "italic",
    lineheight = 1.3, # no effect here
    angle = 45,
    hjust = 1,
    vjust = 0,
    margin = margin(10, 0, 20, 0)
  )
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Spline Sans Mono",
    face = "italic",
    lineheight = 1.3, # no effect here
    angle = 45,
    hjust = 1, # no effect here
    vjust = 0, # no effect here
    margin = margin(10, 0, 20, 0) # no effect here
  )
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Spline Sans Mono",
    face = "italic",
    lineheight = 1.3, # no effect here
    angle = 45,
    hjust = 1, # no effect here
    vjust = 0, # no effect here
    margin = margin(10, 0, 20, 0) # no effect here
  ),
  axis.text.x = element_text(
    hjust = 1,
    vjust = 0,
    margin = margin(10, 0, 20, 0) # trbl
  )
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Spline Sans Mono",
    face = "italic",
    lineheight = 1.3, # no effect here
    angle = 45,
    hjust = 1, # no effect here
    vjust = 0, # no effect here
    margin = margin(10, 0, 20, 0) # no effect here
  ),
  plot.tag = element_text(
    margin = margin(0, 12, -8, 0) # trbl
  )
)



g + theme(
  plot.title = element_text(face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(
    color = "#28a87d",
    family = "Spline Sans Mono",
    face = "italic",
    hjust = 1,
    vjust = 0,
    angle = 45,
    lineheight = 1.3, # no effect here
    margin = margin(10, 0, 20, 0), # no effect here
    debug = TRUE
  ),
  plot.tag = element_text(
    margin = margin(0, 12, -8, 0), # trbl
    debug = TRUE
  )
)



## LABELS + SCALE_*() ----------------------------------------------------------

g <- g + labs(tag = NULL, title = NULL, 
              subtitle = NULL)



g +
  scale_y_continuous(
    breaks = 0:4*15000
  )



g +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 10)
  )



g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format()
  )



g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format(
      suffix = " bikes"
    ),
    name = NULL
  )



g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = scales::comma_format(
      scale = .001
    ),
    name = "Reported bike shares in thousands"
  )



g +
  scale_y_continuous(
    breaks = 0:4*15000,
    labels = function(y) y / 1000,
    name = "Reported bike shares in thousands"
  )



g +
  scale_x_continuous(
    labels = function(y) paste0(y, "°C"),
    name = "Temperature"
  )



g +
  scale_color_discrete(
    name = NULL,
    labels = str_to_title
  )



## DUAL AXIS STYLING (DON'T DO THIS AT HOME) -----------------------------------

sec <- 
  bikes |> 
  group_by(
    month = lubridate::month(date, label = TRUE)
  ) |> 
  summarize(n = sum(count), temp = mean(temp)) |> 
  ggplot(aes(x = month)) +
  geom_col(aes(y = n), fill = "grey70") +
  geom_point(aes(y = temp * 10^5), color = "firebrick") +
  geom_line(aes(y = temp * 10^5, group = 1), color = "firebrick")

sec



sec +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1/10^6, suffix = "M"),
    name = "Rented bikes",
    sec.axis = sec_axis(
      trans = ~ . / 10^5,
      name = "Average daily temperature",
      labels = scales::label_comma(suffix = "°C")
    )
  )



sec +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1/10^6, suffix = "M"),
    name = "Rented bikes",
    sec.axis = sec_axis(
      trans = ~ . / 30000,
      name = "Average daily temperature",
      labels = scales::label_comma(suffix = "°C")
    )
  ) +
  theme(
    axis.title.y.left = element_text(color = "grey60", face = "bold"),
    axis.title.y.right = element_text(color = "firebrick", face = "bold",
                                      margin = margin(l = 10, r = 0))
  )



## STYLING LABELS WITH {GGTEXT} ------------------------------------------------

g +
  ggtitle("**TfL bike sharing trends by _season_**")



g +
  ggtitle("**TfL bike sharing trends by _season_**") +
  theme(
    plot.title = ggtext::element_markdown()
  )



g +
  ggtitle("<b style='font-family:Times;font-size:25pt'>TfL</b> bike sharing trends by <i style='color:#28A87D;'>season</i>") +
  theme(
    plot.title = ggtext::element_markdown()
  )



g +
  ggtext::geom_richtext(
    aes(x = 18, y = 48500,
        label = "What happened on these<br>two <b style='color:#F7B01B;'>summer days</b>?"),
    stat = "unique"
  ) +
  scale_color_manual(
    values = c("#6681FE", "#1EC98D", "#F7B01B", "#A26E7C")
  )



g +
  ggtext::geom_richtext(
    aes(x = 18, y = 48500,
        label = "What happened on these<br>two <b style='color:#F7B01B;'>summer days</b>?"),
    stat = "unique", 
    color = "grey20",
    family = "Asap SemiCondensed",
    fill = NA, 
    label.color = NA
  ) +
  scale_color_manual(
    values = c("#6681FE", "#1EC98D", "#F7B01B", "#A26E7C")
  )



friends <- readr::read_csv(
  here::here("data", "friends-mentions-partners.csv")
)

friends

match_colors <-
  tibble(
    key = c("Chandler", "Joey", "Monica", "Monica & Chandler", 
            "Phoebe", "Rachel", "Rachel & Joey", "Ross", "Ross & Rachel"),
    color = c("#48508c", "#55331d", "#a64d64", "#774f78", 
              "#5b7233", "#ba2a22", "#882f20", "#f6ab18", "#d86b1d")
  )

match_colors

friends |> 
  mutate(key = if_else(
    !partners %in% c("Ross & Rachel", "Rachel & Joey", "Monica & Chandler"),
    word(partners, 1), partners
  )) |> 
  left_join(
    match_colors
  )

friends_render <- friends |> 
  mutate(key = if_else(
    !partners %in% c("Ross & Rachel", "Rachel & Joey", "Monica & Chandler"),
    word(partners, 1), partners
  )) |> 
  left_join(
    match_colors
  ) |> 
  mutate(
    partners = if_else(
      key %in% c("Ross & Rachel", "Rachel & Joey", "Monica & Chandler"),
      paste0("<b style='color:", color, "'>", partners, "</b>"),
      str_replace(partners, key, paste0("<b style='color:", color, "'>", key, "</b>"))
    )
  )

friends_render |> select(key, color, partners) |> unique()

ggplot(friends_render,
       aes(x = id, y = partners)) + 
  theme(axis.text.y = ggtext::element_markdown(hjust = 0))

ggplot(friends_render,
  aes(x = id, y = partners)) + 
  geom_point(aes(size = mentions, color = color), alpha = .3) +
  scale_color_identity() +
  scale_size_area(max_size = 5, guide = "none") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Episodes", y = NULL) +
  theme_minimal(base_family = "Asap SemiCondensed") +
  theme(
    axis.text.y = ggtext::element_markdown(hjust = 0),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )



## FACET LABELLERS -------------------------------------------------------------

g +
  facet_wrap(
    ~ day_night,
    labeller = label_both
  )



g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = label_both
  )



g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      day_night = str_to_title
    )
  )



codes <- c(
  `TRUE` = "Workday",
  `FALSE` = "Weekend or Holiday"
)

g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      day_night = str_to_title,
      is_workday = codes
    )
  )



g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      .default = str_to_title,
      is_workday = codes
    )
  )



g +
  facet_grid(
    day_night ~ is_workday,
    labeller = labeller(
      day_night = str_to_title,
      is_workday = codes
    )
  ) +
  theme(
    legend.position = "top"
  )



## HANDLING LONG LABELS ---------------------------------------------------------

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = weather_type,
        y = count)
  ) +
  geom_boxplot()



ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = weather_type,
        y = count)
  ) +
  geom_boxplot() +
  scale_x_discrete(
    guide = guide_axis(
      n.dodge = 2
    )
  )



ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = str_wrap(weather_type, 6),
        y = count)
  ) +
  geom_boxplot()



g + 
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = element_text(size = 20),
    plot.title.position = "plot"
  )



g + 
  ggtitle("TfL bike sharing trends in 2015 and 2016\nby season for day and night periods") +
  theme(
    plot.title = element_text(size = 20),
    plot.title.position = "plot"
  )



g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title =
      ggtext::element_textbox_simple(size = 20),
    plot.title.position = "plot"
  )



g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      lineheight = .9
    ),
    plot.title.position = "plot"
  )



g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      fill = "grey90",
      lineheight = .9
    ),
    plot.title.position = "plot"
  )



g +
  ggtitle("TfL bike sharing trends in 2015 and 2016 by season for day and night periods") +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(t = 12, b = 12),
      padding = margin(rep(12, 4)),
      fill = "grey90",
      box.color = "grey40",
      r = unit(9, "pt"),
      halign = .5,
      face = "bold",
      lineheight = .9
    ),
    plot.title.position = "plot"
  )



## ANNOTATIONS -----------------------------------------------------------------

ga <- 
  ggplot(bikes, 
         aes(x = temp, y = count)) +
  geom_point(
    aes(color = count > 40000),
    size = 2
  ) +
  scale_color_manual(
    values = c("grey", "firebrick"),
    guide = "none"
  )

ga



ga +
  annotate(
    geom = "text",
    x = 18,
    y = 48000,
    label = "What happened here?"
  )



ga +
  annotate(
    geom = "text",
    x = 18,
    y = 48000,
    label = "What happened here?",
    color = "firebrick",
    size = 6,
    family = "Asap SemiCondensed",
    fontface = "bold",
    lineheight =  .8
  )



ga +
  annotate(
    geom = "text",
    x = c(18, max(bikes$temp)),
    y = c(48000, 1000),
    label = c("What happened here?", "Powered by TfL"),
    color = c("firebrick", "black"),
    size = c(6, 3),
    family = c("Asap SemiCondensed", "Hepta Slab"),
    fontface = c("bold", "plain"),
    hjust = c(.5, 1)
  )



## ggannotate::ggannotate(g)



ga + 
  annotate(
    geom = "text",
    x = 19.5,
    y = 42000,
    label = "What happened here?",
    family = "Asap SemiCondensed",
    size = 6,
    vjust = 1.3
  ) +
  annotate(
    geom = "rect",
    xmin = 17, 
    xmax = 22,
    ymin = 42000, 
    ymax = 54000,
    color = "firebrick", 
    fill = NA
  )



ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "segment",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870
  )



ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870
  )



ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .25,
    arrow = arrow()
  )



ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .25,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed",
      ends = "both"
    )
  )



ga +
  annotate(
    geom = "text",
    x = 10,
    y = 38000,
    label = "The\nhighest\ncount",
    family = "Asap SemiCondensed",
    size = 6,
    lineheight =  .8
  ) +
  annotate(
    geom = "curve",
    x = 13, 
    xend = 18.2,
    y = 38000, 
    yend = 51870,
    curvature = .8,
    angle = 130,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed"
    )
  )



gh <- 
  ggplot(
    data = filter(bikes, temp >= 27),
    aes(x = date, y = temp)
  ) +
  geom_point(
    data = bikes,
    color = "grey65", alpha = .3
  ) +
  geom_point(size = 2.5)

gh



gh +
  geom_text(
    aes(label = format(date, "%m/%d")),
    nudge_x = 10,
    hjust = 0
  )

gh +
  geom_label(
    aes(label = format(date, "%m/%d")),
    nudge_x = .3,
    hjust = 0
  )



set.seed(20230918)

gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d"))
  )



set.seed(20230918)

gh + 
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d")),
    family = "Spline Sans Mono",
    size = 4.5,
    fontface = "bold"
  )



gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%m/%d")),
    family = "Spline Sans Mono",
    # space between points + labels
    box.padding = .8,
    # always draw segments
    min.segment.length = 0
  )



gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    # force to the right
    xlim = c(NA, as.Date("2015-06-01")), 
    hjust = 1
  )



gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    xlim = c(NA, as.Date("2015-06-01")),
    # style segment
    segment.curvature = .01,
    arrow = arrow(length = unit(.02, "npc"), type = "closed")
  )



gh +
  ggrepel::geom_text_repel(
    aes(label = format(date, "%y/%m/%d")),
    family = "Spline Sans Mono",
    xlim = c(NA, as.Date("2015-06-01")),
    # style segment
    segment.curvature = .001,
    segment.inflect = TRUE
  )



g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000)
  )



g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    color = "black",
    label.family = "Asap SemiCondensed"
  )



g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed"
  )



g +
  ggforce::geom_mark_rect(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    radius = unit(12, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )



g +
  ggforce::geom_mark_circle(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )



g +
  ggforce::geom_mark_hull(
    aes(label = "Outliers?",
        filter = count > 40000),
    description = "What happened on\nthese two days?",
    color = "black",
    label.family = "Asap SemiCondensed",
    expand = unit(8, "pt"),
    con.cap = unit(0, "pt"),
    label.buffer = unit(15, "pt"),
    con.type = "straight",
    label.fill = "transparent"
  )



bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geom_line(linewidth = 1) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    name = NULL
  )



bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geomtextpath::geom_textline(
    aes(label = day_night),
    linewidth = 1,
    vjust = -.5, 
    family = "Asap SemiCondensed",
    fontface = "bold"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    guide = "none"
  )



bikes |>
  filter(year == "2016") |>
  group_by(month, day_night) |> 
  summarize(count = sum(count)) |> 
  mutate(day_night = if_else(
    day_night == "day", 
    "Day period (6am-6pm)", 
    "Night period (6pm-6am)"
  )) |> 
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  geomtextpath::geom_textline(
    aes(label = day_night),
    linewidth = 1,
    vjust = -.5, 
    hjust = .01,
    family = "Asap SemiCondensed",
    fontface = "bold"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    guide = "none"
  )



bikes |>
  filter(year == "2016") |>
  ggplot(aes(x = month, y = count, 
             color = day_night,
             group = day_night)) +
  stat_summary(
    geom = "line", fun = sum,
    linewidth = 1
  ) +
  geomtextpath::geom_textline(
    aes(label = day_night), 
    stat = "summary" # fails
  ) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(
    labels = scales::label_comma(
      scale = 1/10^3, suffix = "K"
    ),
    limits = c(0, 850000)
  ) +
  scale_color_manual(
    values = c("#FFA200", "#757BC7"),
    name = NULL
  )


##----------------------------------------------------------------------------##
## THAT'S IT FOLKS! ----------------------------------------------------------##
##----------------------------------------------------------------------------##