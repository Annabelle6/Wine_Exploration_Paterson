library(plotly)
library(dplyr)
library(tidyverse)

locations <- read.csv("flights.csv", stringsAsFactors = FALSE)


register_google(key = "AIzaSyARmTFbYyT98l6XQp0zwCJmbTtL54Oj2cw")
library(ggmap)
dots <- unique(c(locations$From, locations$To))
coords <- geocode(dots)
dots <- data.frame(dot=dots, coords)

dots$order <- seq.int(nrow(dots))

locations <- merge(locations, dots, by.x="To", by.y="dot")
locations <- merge(locations, dots, by.x="From", by.y="dot")

write_rds(dots, "dots_new", compress = "none")
write_rds(locations, "locations_new", compress = "none")


geo <- list(
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_geo(color = I("red")) %>%
  add_markers(
    data = dots, x = ~lon, y = ~lat, text = ~dot,
    size = ~order, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(locations, Order),
    x = ~lon.y, xend = ~lon.x,
    y = ~lat.y, yend = ~lat.x,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  ) %>%
  layout(
    title = 'New Zealand Wine Distribution around the world<br>(Hover for country names)',
    geo = geo, showlegend = FALSE, height=1000
  )



