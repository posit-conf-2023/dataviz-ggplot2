#------------------------------------------------------------------------------#
#                                                                              #
#           Engaging and Beautiful Data Visualizations with ggplot2            #
#                                                                              #
#                              Working with Themes                             #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                         posit::conf(2023) Workshop                           #
#                              September 18, 2023                              #
#                                                                              #
#------------------------------------------------------------------------------#


## PREPARATION -----------------------------------------------------------------

pkgs <- c("ggplot2", "readr", "dplyr", "stringr", "lubridate", "here", "scales",
          "hrbrthemes", "ggthemes", "tvthemes", "systemfonts")
install.packages(setdiff(pkgs, rownames(installed.packages())))



library(readr)
library(ggplot2)



bikes <-
  read_csv(
    here::here("data", "london-bikes-custom.csv"),
    col_types = "Dcfffilllddddc"
  )



g <- bikes |>
  dplyr::group_by(month = lubridate::month(date, label = TRUE), day_night, year) |> 
  dplyr::summarize(count = sum(count)) |> 
  ggplot(aes(x = month, y = count, color = day_night)) +
  geom_line(aes(group = day_night)) +
  geom_point(size = 2) +
  facet_wrap(~year, ncol = 1, scales = "free_x") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(
    limits = c(0, 830000), expand = c(0, 0),
    labels = scales::label_comma(scale = 1/10^3, suffix = "K")
  ) +
  scale_color_manual(values = c(day = "#FFA200", night = "#757BC7")) +
  labs(
    x = NULL, y = "# rented bikes", color = NULL,
    title = "TfL Bike Shares per Month and Year",
    caption = "Data: TfL (Transport for London)"
  )

g



## COMPLETE THEMES -------------------------------------------------------------

g + theme_light()

g + theme_bw()

g + theme_minimal()

g + theme_classic()

g + theme_dark()

g + theme_void()



g + ggthemes::theme_stata()

g + ggthemes::theme_gdocs()



g + hrbrthemes::theme_ipsum_rc()



g + tvthemes::theme_simpsons()



## MODIFY THEME ELEMENTS -------------------------------------------------------

g + 
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(color ="#28A87D"),
    plot.title.position = "plot"
  )



## NON-DEFAULT TYPEFACES -------------------------------------------------------

library(systemfonts)



match_font("Asap", bold = TRUE)



system_fonts()



system_fonts() |>
  dplyr::filter(stringr::str_detect(family, "Asap")) |>
  dplyr::select(family) |>
  unique() |> 
  dplyr::arrange(family)



g +
  theme_minimal(
    base_family = "Asap SemiCondensed",
    base_size = 13
  )



system_fonts() |>
  dplyr::filter(family == "Asap SemiCondensed") |>
  dplyr::select(name) |>
  dplyr::arrange(name)



register_variant(
  name = "Asap SemiCondensed Semibold S1",
  family = "Asap SemiCondensed",
  weight = "semibold",
  features = font_feature(letters = "stylistic")
)



g + 
  theme_minimal(
    base_family = "Asap SemiCondensed Semibold S1",
    base_size = 13
  )



register_variant(
  name = "Spline Sans Tabular",
  family = "Spline Sans",
  weight = "normal",
  features = font_feature(numbers = "tabular")
)



## SET THEMES GLOBALLY ---------------------------------------------------------

theme_set(theme_minimal(base_family = "Asap SemiCondensed", base_size = 13))

theme_update(
  panel.grid.minor = element_blank(),
  strip.text = element_text(face = "bold", size = rel(1.1)),
  plot.title = element_text(face = "bold", size = rel(1.3)),
  plot.title.position = "plot",
  plot.caption.position = "plot"
)



g



## CUSTOM THEMES --------------------------------------------------------------

theme_grey

theme_minimal



theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {

  theme_minimal(base_size = base_size, base_family = base_family,
                base_line_size = base_line_size, base_rect_size = base_rect_size)
}



theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {

  theme_minimal(base_size = base_size, base_family = base_family,
                base_line_size = base_line_size, base_rect_size = base_rect_size)  %+replace%
    theme(
      # add your theme changes here
    )
}



theme_asap <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size),
                                  size = rel(0.8), hjust = 1, vjust = 1),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, vjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, vjust = 0, angle = 90, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(), 
      complete = TRUE
    )
}



g + 
  theme_asap()



theme_asap_plus <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme( 
      plot.title = element_text(size = rel(1.3), hjust = 0,
                                family = "Asap SemiCondensed Extrabold"),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank()
    )
}

theme_asap_replace <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme( 
      plot.title = element_text(size = rel(1.3), hjust = 0,
                                family = "Asap SemiCondensed Extrabold"),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank()
    )
}

g + theme_asap_plus()

g + theme_asap_replace()



g + 
  theme_asap() +
  theme(
    legend.position = "top",
    plot.background = element_rect(
      fill = NA, color = NA
    )
  )



g + 
  theme_asap(
    base_size = 9,
    base_family = "Hepta Slab"
  )



g + 
  theme_asap(
    base_size = 9,
    base_family = "Hepta Slab"
  ) +
  theme(
    plot.title = element_text(
      family = "Hepta Slab"
    )
  )



theme_asap_title <- function(base_size = 13, base_family = "Asap SemiCondensed", 
                             title_family = "Asap SemiCondensed Extrabold",
                             base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  if (title_family == "Asap SemiCondensed Extrabold") {
    register_variant(name = "Asap SemiCondensed Extrabold",
                     family = "Asap SemiCondensed",
                     weight = "ultrabold")
  }
  
  theme_minimal(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), hjust = 0, family = title_family),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank()
    )
}



g +
  theme_asap_title(
    base_size = 9,
    base_family = "Hepta Slab",
    title_family = "Hepta Slab"
  )



theme_fonts <- function(base_size = 12, base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {
  
  unavailable <- vector("character")
  
  if (sum(grepl("Hepta Slab", systemfonts::system_fonts()$family)) > 0) {
    systemfonts::register_variant(
      name = "Hepta Slab Extrabold",
      family = "Hepta Slab",
      weight = "ultrabold"
    )
    title_family <- "Hepta Slab Extrabold"
  } else {
    title_family <- ""
    unavailable <- c(unavailable, "Hepta Slab")
  }
  
  if (sum(grepl("Spline Sans", systemfonts::system_fonts()$family)) > 0) {
    base_family <- "Spline Sans"
  } else {
    base_family <- ""
    unavailable <- c(unavailable, "Spline Sans")
  }
  
  if (length(unavailable) > 0) {
    unavailable <- data.frame(
      name = unavailable, 
      url = paste0("https://fonts.google.com/specimen/", sub(" ", "+", unavailable))
    )
    message(paste(
      "Using system default typefaces.", 
      "For proper use, please install the following typeface(s):",
      paste0("  - ", unavailable$name, ": ", unavailable$url, collapse = "\n"),
      "Then restart your R session.",
      sep = "\n"
    ))
  }
  
  theme_asap(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), hjust = 0, family = title_family)
    )
}



g + theme_fonts()



theme_asap_grid <- function(base_size = 13, base_family = "Asap SemiCondensed", grid = "xy", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  out <- 
    theme_minimal(base_size = base_size, base_family = base_family, 
                  base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0), 
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = "grey20"),
      axis.ticks.length = unit(base_size/2, "pt")
    )
  
  if (stringr::str_detect(grid, "x|X")) {
    out <- out + theme(panel.grid.major.x = element_line(color = "grey87"),
                       axis.ticks.x = element_blank(),
                       axis.ticks.length.x = unit(base_line_size, "pt"))
  }
  if (stringr::str_detect(grid, "y|Y")) {
    out <- out + theme(panel.grid.major.y = element_line(color = "grey87"),
                       axis.ticks.y = element_blank(),
                       axis.ticks.length.y = unit(base_line_size, "pt"))
  }
  
  return(out)
}



g + 
  theme_asap_grid(
    grid = "y"
  )



g + theme_asap_grid()



g + theme_asap_grid(grid = "none")



g + 
  theme_asap_grid(
    grid = "all"
  )



theme_asap_grid <- function(base_size = 13, base_family = "Asap SemiCondensed", grid = "xy", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  if(!stringr::str_detect(grid, "none|x|X|y|Y")) stop('grid must be a character: "none" or any combination of "X", "Y", "x" and "y".')
  
  out <- 
    theme_minimal(base_size = base_size, base_family = base_family, 
                  base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = rel(1.3), margin = margin(b = base_size/2),
                                family = "Asap SemiCondensed Extrabold", hjust = 0), 
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey30", margin = margin(t = base_size)),
      plot.caption.position = "plot",
      axis.title.x = element_text(hjust = 0, margin = margin(t = base_size/3)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = base_size/3)),
      panel.background = element_rect(fill = "white", color = "grey20"), 
      panel.border = element_rect(fill = NA, color = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.justification = "top",
      strip.text = element_text(size = rel(1.05), margin = margin(base_size/2, 0, base_size/2, 0)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = "grey20"),
      axis.ticks.length = unit(base_size/2, "pt")
    )
  
  if (stringr::str_detect(grid, "x|X")) {
    out <- out + theme(panel.grid.major.x = element_line(color = "grey87"),
                       axis.ticks.x = element_blank(),
                       axis.ticks.length.x = unit(base_line_size, "pt"))
  }
  if (stringr::str_detect(grid, "y|Y")) {
    out <- out + theme(panel.grid.major.y = element_line(color = "grey87"),
                       axis.ticks.y = element_blank(),
                       axis.ticks.length.y = unit(base_line_size, "pt"))
  }
  
  return(out)
}



g +
  theme_asap_grid(
    grid = "all"
  )


##----------------------------------------------------------------------------##
## THAT'S IT FOLKS! ----------------------------------------------------------##
##----------------------------------------------------------------------------##