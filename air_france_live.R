library(rvest)
library(dplyr)
library(plyr)
library(magrittr)
library(stringr)
library(plotly)


# load the airports locations
airports = read.csv("airports.csv")
airports = dplyr::filter(airports, type == "large_airport" | type == "medium_airport" | type == "small_aiport") %>%
  dplyr::select(ident, latitude_deg, longitude_deg)
colnames(airports)[2] <- "latitude"
colnames(airports)[3] <- "longitude"

# get live flights for AFR 
flights_page = read_html("https://fr.flightaware.com/live/fleet/AFR")
live_flights = flights_page %>% html_nodes(".prettyTable")
live_flights = html_table(live_flights, fill = TRUE, header = TRUE)
live_flights = data.frame(live_flights)
colnames(live_flights) = as.character(unlist(live_flights[1,]))
live_flights = live_flights[-1,1:6]

# clean data to obtain only IACO airports codes
live_flights = live_flights %>% mutate(Provenance = str_extract(Provenance, "[A-Z]{4}"))
live_flights = live_flights %>% mutate(Destination = str_extract(Destination, "[A-Z]{4}"))

# from IACO, add Provenance and Destination coordinates
live_airports = c(live_flights$Provenance, live_flights$Destination) %>% unique()
live_airports = airports %>% dplyr::filter(ident %in% live_airports)
live_airports = live_airports %>% mutate(ident = as.character(ident))
long = live_airports$longitude
lat = live_airports$latitude
names(long) = live_airports$ident
names(lat) = live_airports$ident

live_flights$Provenance_long = live_flights$Provenance
live_flights$Provenance_lat = live_flights$Provenance
live_flights$Destination_long = live_flights$Destination
live_flights$Destination_lat = live_flights$Destination
live_flights = live_flights %>% mutate(Provenance_long = long[Provenance_long])
live_flights = live_flights %>% mutate(Provenance_lat = lat[Provenance_lat])
live_flights = live_flights %>% mutate(Destination_long = long[Destination_long])
live_flights = live_flights %>% mutate(Destination_lat = lat[Destination_lat])

# Add from_france dummy to color later


# plot
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYWRvcHUiLCJhIjoiY2ozYWUycGRqMDAzdzMxbzdxcGtkYzM2cyJ9.F49LI0ZGRQC1to_MYA1jNA')
flights_map = plot_mapbox(mode = 'scattermapbox') %>%
  layout(title = "Live flights",
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark')) %>%
  add_markers(
    data = live_airports, x = ~longitude, y = ~latitude, text = ~ident,
    hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = live_flights,
    x = ~Provenance_long, xend = ~Destination_long,
    y = ~Provenance_lat, yend = ~Destination_lat,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  )

flights_map