############    Import the packages  ##########

library(data.table)
library(tidyverse)
library(plotly)
library(janitor)
library(stringr)
library(treemap)
library(igraph)
library(gridExtra)
library(ggraph)

############    Import the Data   ###########
airport <- read_csv("airports-extended.csv", col_names = F)
names(airport) <- c("Airporlt_ID", "Airport_Name", "City", "Country", "IATA", 
                    "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
                    "DST", "Tz", "Type", "Source")
airport <- airport %>% 
  filter(Type == "airport")
airline <- read_csv("airlines.csv") %>% 
  clean_names()
route <- read_csv("routes.csv") %>% 
  clean_names()
names(route)[5] <- "destination_airport"

countries <- read_csv("countries of the world.csv")

############    Data Content  ##################

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

############    Global Airports Distribution   #############

geo <- list(
  scope = "world",
  projection = list(type = "orthographic"),
  showland = TRUE,
  resolution = 100,
  landcolor = toRGB("gray90"),
  countrycolor = toRGB("gray80"),
  oceancolor = toRGB("lightsteelblue2"),
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

############    Global Airline route   ########
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
               filter(Altitude >= 5000),
             aes(x = Longitude, y = Latitude, colour = Altitude), 
             size = .7) +
  labs(title = "Airports located over 5,000 feet altitude") +
  ylim(-60, 90) +
  theme(legend.position = c(.1, .25))


############    Which Country has the most Airports?  ######
connection.route <- route %>% 
  spread(key = Airport_type, value = Airport) %>% 
  select(destination_airport, source_airport, id)
airport.country <- airport %>% 
  select(City, Country, IATA)
flight.connection <- merge(connection.route, airport.country, by.x = "source_airport", by.y = "IATA")
names(flight.connection)[4:5] <- c("source.City", "source.Country")

flight.connection <- merge(flight.connection, airport.country, by.x = "destination_airport", by.y = "IATA")
names(flight.connection)[6:7] <- c("destination.City", "destination.Country")

flight.connection <- flight.connection %>% 
  select(id, contains("source"), contains("destination"))

data.frame(table(airport$Country)) %>% 
  arrange(desc(Freq)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1, label = Freq)) + 
  geom_bar(stat = "identity", show.legend = F) +
  labs(title = "Top 20 Countries that has most Airports", 
       x = "Country", y = "The number of Airports") +
  geom_label(angle = 45, show.legend = F) +
  theme(axis.text.x = element_text(angle = 40, size = 15))


treemap(data.frame(table(airport$Country)),
        index="Var1",
        vSize="Freq",
        type="index",
        title = "Overall Number of Airport owned by each Nation")

############    Which Country has the most Airlines? ###########
data.frame(table(airline$country)) %>% 
  arrange(desc(Freq)) %>% head(20) %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, 
             fill = Var1, label = Freq)) + 
  geom_bar(stat = "identity", show.legend = F) +
  geom_label(show.legend = F) +
  theme(axis.text.x = element_text(angle = 40, size = 15)) +
  labs(x = "Country", y = "The number of Airlines", 
       title = "Top 20 Countries that have most airlines")


############    Airports vs Airlines  ########
country.airport <- data.frame(table(airport$Country))
names(country.airport)[2] <- "Airport"

country.airline <- data.frame(table(airline$country))
names(country.airline)[2] <- "Airline"

lineports <- merge(country.airport, country.airline, by = "Var1")
lineports %>% 
  ggplot(aes(x = Airport, y = Airline)) + 
  geom_point(show.legend = F) +
  geom_smooth() + 
  labs(title = "Airports vs Airlines") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(10, 100, 500, 1000))



############    Singapore Airlines with other countries  ##### 
SG.airport <- airport %>% filter(Country == "Singapore")
SG.flight.connection <- flight.connection %>% 
  filter(source.Country == "Singapore" | destination.Country == "Singapore")
SG.gloabal.flight.route.id <- 
  gloabal.flight.route %>% 
  filter(Country == "Singapore") %>% select(id)

SG.gloabal.flight.route.id <- SG.gloabal.flight.route.id$id %>% as.vector()

SG.gloabal.flight.route.id <- 
  gloabal.flight.route %>% filter(id %in% SG.gloabal.flight.route.id)


Singapore.ggmap <- ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") +
  geom_point(data = SG.gloabal.flight.route.id, 
             aes(x = Longitude, y = Latitude), colour = "red") +
  geom_point(data = SG.airport, 
             aes(x = Longitude, y = Latitude), colour = "red") +
  geom_line(data = SG.gloabal.flight.route.id, 
            aes(x = Longitude, y = Latitude, group = id), colour = "red") + 
  xlim(-180, 180) + ylim(-50, 50) +
  labs(title = "Airports & International Airlines from/to/in Singapore") +
  coord_fixed(ratio = 1.1)

Flight.Country.Connection <- SG.flight.connection %>% 
  select(contains("Country"), id)
names(Flight.Country.Connection) <- c("From", "To", "id")
Flight.Country.Connection <- Flight.Country.Connection %>% 
  mutate(Combination = paste0(From, "-", To))
Flight.Country.Connection <- Flight.Country.Connection %>% 
  group_by(Combination) %>% 
  mutate(Weight = NROW(Combination)) %>% 
  arrange(-Weight) %>% 
  ungroup()
Flight.Country.Connection <- Flight.Country.Connection[!duplicated(Flight.Country.Connection$Combination),] %>% 
  select(-id, -Combination)
Flight.Country.graph <- graph_from_data_frame(Flight.Country.Connection, directed = FALSE)

Flight.Country.graph$name <- "Flight Country Network"
V(Flight.Country.graph)$id <- 1:vcount(Flight.Country.graph)


NW.Plot <- ggraph(Flight.Country.graph, layout = "kk") +
  geom_edge_link(aes(alpha = Weight), 
                 colour = "red") +
  geom_node_point(size = 5, colour = "red") +
  geom_node_text(aes(label = name), repel = TRUE, size = 7) +
  labs(title = "Flight Country Network", x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank())

Singapore.ggmap
NW.Plot









