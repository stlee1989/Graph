library(data.table) # fast data import
library(tidyverse) # data manipulation
library(plotly) # interactive visualizations
library(janitor) # data manipulation
library(stringr) # character class data manipulation
library(treemap) # tree map visualization
library(igraph)
library(gridExtra)
library(ggraph)

airport <- read_csv("master class/web analytics/project/airports-extended.csv", col_names = F)
names(airport) <- c("Airpot_ID", "Airport_Name", "City", "Country", "IATA", 
                    "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
                    "DST", "Tz", "Type", "Source")
airport <- airport %>% 
  filter(Type == "airport")
airline <- read_csv("master class/web analytics/project/airlines.csv") %>% 
  clean_names()
route <- read_csv("master class/web analytics/project/routes.csv") %>% 
  clean_names()
names(route)[5] <- "destination_airport"

countries <- read_csv("master class/web analytics/project/countries of the world.csv")

airport %>% 
  head(5) %>% 
  DT::datatable(options = list(
    lengthMenu = c(5,3,1)
  ))
airline %>% 
  head(5) %>% 
  DT::datatable(options = list(
    lengthMenu = c(5,3,1)
  ))
route %>% 
  head(5) %>% 
  DT::datatable(options = list(
    lengthMenu = c(5,3,1)
  ))

## Global Airports Distribution
geo <- list(
  scope = "world",
  projection = list(type = "orthographic"),
  showland = TRUE,
  resolution = 50,
  landcolor = toRGB("darkgreen"),
  countrycolor = toRGB("gray80"),
  oceancolor = toRGB("blue2"),
  showocean = TRUE
)

plot_geo(locationmode = "Greenwich") %>%
  add_markers(data = airport %>% 
                filter(Type == "airport"),
              x = ~Longitude,
              y = ~Latitude,
              text = ~paste('Airport: ', Airport_Name),
              alpha = .5, color = "red") %>%
  layout(
    title = "Global Airports",
    geo = geo,
    showlegend = FALSE
  )
print(paste("There are", airport %>% 
              filter(Type == "airport") %>% 
              nrow(), 
            "airports around the world."))
#"There are 7750 airports around the world."

#Global Airline route
route <- route %>% mutate(id = rownames(route))
route <- route %>% gather('source_airport', 'destination_airport', key = "Airport_type", value = "Airport")
gloabal.flight.route <- merge(route, airport %>% select(Airport_Name, IATA, Latitude, Longitude, Country, City),
                              by.x = "Airport", by.y = "IATA")

world.map <- map_data ("world")
world.map <- world.map %>% 
  filter(region != "Antarctica")

ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") +
  geom_point(data = gloabal.flight.route, 
             aes(x = Longitude, y = Latitude), 
             size = .1, alpha = .5, colour = "red") +
  geom_line(data = gloabal.flight.route, 
            aes(x = Longitude, y = Latitude, group = id), 
            alpha = 0.05, colour = "red") +
  labs(title = "Global Airline Routes")

ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="grey") + 
  geom_point(data = airport %>% 
               filter(Altitude >= 10000),
             aes(x = Longitude, y = Latitude, colour = Altitude), 
             size = .7) +
  labs(title = "Airports located over 10,000 feet altitude") +
  ylim(-60, 90) +
  theme(legend.position = c(.1, .25))

#airports with altitude > 10,000 feet
print(paste(airport %>% 
              filter(Altitude >= 10000) %>% 
              nrow(), 
            "airports are located over 5,000 feet altitude."))
#There are 22 airports that are located over 10,000 feet all over the world. 
