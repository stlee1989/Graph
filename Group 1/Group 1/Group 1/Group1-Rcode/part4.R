install.packages("tidyverse")
install.packages("plotly")
install.packages("janitor")
install.packages("treemap")
install.packages("gridExtra")
install.packages("ggraph")

library(data.table) # fast data import
library(tidyverse) # data manipulation
library(plotly) # interactive visualizations
library(janitor) # data manipulation
library(stringr) # character class data manipulation
library(treemap) # tree map visualization
library(igraph)
library(gridExtra)
library(ggraph)

airport <- read_csv("D:/Rdataset/airports-extended.csv", col_names = F)
names(airport) <- c("Airpot_ID", "Airport_Name", "City", "Country", "IATA", 
                    "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
                    "DST", "Tz", "Type", "Source")
airport <- airport %>% 
  filter(Type == "airport")
airline <- read_csv("D:/Rdataset/airlines.csv") %>% 
  clean_names()
route <- read_csv("D:/Rdataset/routes.csv") %>% 
  clean_names()
names(route)[5] <- "destination_airport"

countries <- read_csv("D:/Rdataset/countries of the world.csv")

#############################################################################
####### Focus on Singapore #################################################
#############################################################################
#preparation
route <- route %>% mutate(id = rownames(route))
route <- route %>% gather('source_airport', 'destination_airport', key = "Airport_type", value = "Airport")
gloabal.flight.route <- merge(route, airport %>% select(Airport_Name, IATA, Latitude, Longitude, Country, City),
                              by.x = "Airport", by.y = "IATA")

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
#############################################################################
S_airport <- airport %>% filter(Country == "Singapore")
S_flight.connection <- flight.connection %>% 
  filter(source.Country == "Singapore" | destination.Country == "Singapore")
S_gloabal.flight.route.id <- 
  gloabal.flight.route %>% 
  filter(Country == "Singapore") %>% select(id)
S_gloabal.flight.route.id <- S_gloabal.flight.route.id$id %>% as.vector()

S_gloabal.flight.route.id <- 
  gloabal.flight.route %>% filter(id %in% S_gloabal.flight.route.id)
#############################################################################
#visualization
install.packages("maps")
library(maps)
world.map <- map_data ("world")
world.map <- world.map %>% 
  filter(region != "Antarctica")


Singapore.ggmap <- ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") +
  geom_point(data = S_gloabal.flight.route.id, 
             aes(x = Longitude, y = Latitude), colour = "red") +
  geom_point(data = S_airport, 
             aes(x = Longitude, y = Latitude), colour = "red") +
  geom_line(data = S_gloabal.flight.route.id, 
            aes(x = Longitude, y = Latitude, group = id), colour = "red") + 
  xlim(100, 140) + ylim(0, 45) +
  labs(title = "Airports & International Airlines from/to/in Singapore") +
  coord_fixed(ratio = 1.1)

Flight.Country.Connection <- S_flight.connection %>% 
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

grid.arrange(Singapore.ggmap, NW.Plot, ncol=2)

transform(Flight.Country.Connection,Weight=as.numeric(Weight))
sapply(Flight.Country.Connection, mode)
plot2=data.frame(Flight.Country.Connection)
plot3=plot2[plot2$From!="Singapore",]
plot4=plot2[plot2$To!="Singapore",]
sapply(plot3, mode) 
plot3$label <- paste(plot3$From, plot3$Weight, sep = ": ")
treemap(plot3,
        index="label",
        vSize="Weight",
        type="index",
        title = "Flights to Sinagpore")
plot4$label <- paste(plot4$To, plot4$Weight, sep = ": ")
treemap(plot4,
        index="label",
        vSize="Weight",
        type="index",
        title = "Flights from Singapore")
library(DT)
install.packages("DT")
S_gloabal.flight.route.id %>% 
  filter(Country != "Singapore") %>% 
  select(Airport_Name, Country, City, Latitude, Longitude) %>% 
  distinct(City, .keep_all = T) %>% 
  DT::datatable(options = list(
    lengthMenu = c(121,1)
  ))

#############################################################################
####### Network in Asia #################################################
#############################################################################
country.connection <- flight.connection %>% 
  select(contains("Country")) %>% 
  mutate(Link = paste0(source.Country, "-", destination.Country))
country.connection <- country.connection[!duplicated(country.connection$Link),]
names(country.connection) <- c("from", "to", "Link" )

#### Selecting Asian Countries
Country.list <- countries %>% 
  select(Country, Region)
Country.list$Country <- as.character(Country.list$Country)
Asian.Country.list <- Country.list %>% 
  arrange(Region) %>% 
  head(28)
Asian.Country <- Asian.Country.list$Country
Asian.Country <- gsub(pattern = "\\, ", replacement = "", Asian.Country) %>% 
  gsub(pattern = " ", replacement = "", Asian.Country) %>% 
  gsub(pattern = "KoreaNorth", replacement = "North Korea", Asian.Country) %>% 
  gsub(pattern = "KoreaSouth", replacement = "South Korea", Asian.Country)

country.connection <- country.connection %>% 
  filter(from %in% Asian.Country & to %in% Asian.Country) %>% 
  select(-Link)

country.connection <- country.connection %>% 
  mutate(TF = str_detect(from, to)) %>% 
  filter(TF == "FALSE") %>% 
  select(-TF)
g <- graph_from_data_frame(country.connection, directed = TRUE)
V(g)$color <- ifelse(
  V(g)$name == "Singapore", "red", "yellow"
)

plot(g, layout =  layout_with_dh(g),
     edge.arrow.size=0.05,
     vertex.size = 17, vertex.label.cex = 0.8)
