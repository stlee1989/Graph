####Preparation
library(data.table) # fast data import
library(tidyverse) # data manipulation
library(plotly) # interactive visualizations
library(janitor) # data manipulation
library(stringr) # character class data manipulation
library(treemap) # tree map visualization
library(igraph)
library(gridExtra)
library(ggraph)

airport <- read_csv("desktop/airports-extended.csv", col_names = F)
names(airport) <- c("Airport_ID", "Airport_Name", "City", "Country", "IATA", 
                    "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", 
                    "DST", "Tz", "Type", "Source")
airport <- airport %>% 
  filter(Type == "airport")
airline <- read_csv("desktop/airlines.csv") %>% 
  clean_names()
route <- read_csv("desktop/routes.csv") %>% 
  clean_names()
names(route)[5] <- "destination_airport"

countries <- read_csv("desktop/countries of the world.csv")

####Data Content

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


###Analysis
route <- route %>% mutate(id = rownames(route))
route <- route %>% gather('source_airport', 'destination_airport', key = "Airport_type", value = "Airport")


#### Which Country has the most Airports?

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

###Treemap Visualization
treemap(data.frame(table(airport$Country)),
        index="Var1",
        vSize="Freq",
        type="index",
        title = "Overall Number of Airport owned by each Nation")

##### Which Country has the most Airlines?
data.frame(table(airline$country)) %>% 
  arrange(desc(Freq)) %>% head(20) %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, 
             fill = Var1, label = Freq)) + 
  geom_bar(stat = "identity", show.legend = F) +
  geom_label(show.legend = F) +
  theme(axis.text.x = element_text(angle = 40, size = 15)) +
  labs(x = "Country", y = "The number of Airlines", 
       title = "Top 20 Countries that have most airlines")















