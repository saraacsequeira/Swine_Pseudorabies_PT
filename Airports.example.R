# for loading our data
library(sf)
# for datasets
library(nycflights13)
# for plotting
library(mapdeck)
library(RColorBrewer)
# for data wrangling
library(dplyr)


# ADD DATA
# count the number of flights from each origin to each destination
flights_grouped <- flights %>%
  count(dest, origin)
colnames(airports)[1] <- "dest"
# join the flight data with the coordinates of the destination airports
flights_grouped <- flights_grouped %>%
  right_join(airports[, c(1, 3:4)])
colnames(flights_grouped)[4:5] <- c("lat_dest", "lon_dest")
colnames(airports)[1] <- "origin"
# join the flight data with the coordinates of the origin airports
flights_grouped <- flights_grouped %>%
  right_join(airports[, c(1, 3:4)])
colnames(flights_grouped)[6:7] <- c("lat_origin", "lon_origin")
# remove NAs
flights_grouped <- flights_grouped[!is.na(flights_grouped$dest), ]

#ADD MAP and TOKEN
ms <- mapdeck_style("satellite")
token <- "pk.eyJ1IjoidGVyZXNhcGNvdXRpbmhvIiwiYSI6ImNraG9tbGRvZTBiNW8yc3A1cHgwMTM3engifQ.IZkYiF2VaRnuW9lm6h3SgQ"



# ADD 2ND MAP (NEW STYLE)
style <- mapdeck_style("mapbox://styles/saraacsequeira/ckhootfbr214a19qj8ieui4br")
token1 <- "pk.eyJ1Ijoic2FyYWFjc2VxdWVpcmEiLCJhIjoiY2tob21yOXJsMDhqdjJxbHRqNXRzcWtuNSJ9.rSulzuWkuijZK1xmU_BPnQ"



# ADD_ARC EXAMPLES
mapdeck(data = flights_grouped, style = style, pitch = 30, token = token1) %>%
  add_arc(
    # coordinates of origin airports
    origin = c("lon_origin", "lat_origin"),
    # coordinates of destination airports
    destination = c("lon_dest", "lat_dest"),
    # color our strokes depending on the number of flights
    stroke_from = "n",
    stroke_to = "n",
    palette = "viridis",
    legend = TRUE
  ) %>%
  # set the view
  mapdeck_view(
    location = c(-110, 48),
    # set the zoom level
    zoom = 2,
    # set the pitch angle
    pitch = 45,
  )




url <- 'https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv'
flights <- read.csv(url)
flights$info <- paste0("<b>",flights$airport1, " - ", flights$airport2, "</b>")

mapdeck(token = token, style = ms) %>%
  add_arc(
    data = flights
    , origin = c("start_lon", "start_lat")
    , destination = c("end_lon", "end_lat")
    , stroke_from = "airport1"
    , stroke_to = "airport2"
    , tooltip = "info"
    , layer_id = 'arclayer'
  )
