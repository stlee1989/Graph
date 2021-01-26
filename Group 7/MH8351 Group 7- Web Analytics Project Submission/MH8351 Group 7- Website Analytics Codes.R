#               WEB ANALYTICS PROJECT           #            
#################################################

     # Team Members #
#TAN CHOON KIAT (G1700699B)#
#TAN HONG GUANG (G1800654K)#
#LI LIQIAN (G1800749K)#
############################

#########################
# Set Working Directory #
# in order to read file #
#########################

getwd()
setwd("C:/Users/CK/Desktop/Web Analytics/Our Data")

#################################
# Install and load the packages # 
#################################
install.packages("dplyr")  
install.packages("ggthemes")
install.packages("igraph")
library(dplyr)
library(ggplot2)
library(igraph)
library(ggthemes)
library(tidyr)  # to add unite function

#######################################
# Coding for the Dataset and Analysis # 
#######################################
# Start off with inserting Airports as Nodes
# Load information about the Airports
airports_nodes <- read.csv("Airport.csv", header=TRUE)
airports_nodes

# Load Route Data information
# Create route from source airport to destination airport
routes_edges <- read.csv("Route.csv", header=TRUE)
routes_edges <- routes_edges[, c(4, 6, 1, 2, 3, 5, 7, 8, 9)] # Rearrange the columns to make it more intuitive
colnames(routes_edges)[1:2] <- c("Start", "End")
routes_edges

### Data Processing ###

# Drop Source and Target = \\N i.e. NULL values
routes_edges
routes_edges <- routes_edges[!(routes_edges$Start=="\\N" | routes_edges$End== "\\N"),]
routes_edges$Start <- factor(routes_edges$Start)
routes_edges$End <- factor(routes_edges$End)
routes_edges

### Data Exploration ###

# Number of Airports per Country
C_Airport<-airports_nodes %>% group_by(Country) %>% tally()
C_Airport <- C_Airport[order(-C_Airport[,2]),] 

# Top 10 countries with highest number of airports
C_Airport <- C_Airport[1:10, ]
C_Airport

# Number of Destinations per Airports
A_Destinations<- routes_edges %>% group_by(Destination.airport) %>% tally()
A_Destinations <- A_Destinations[order(-A_Destinations[,2]),] 

# Top 10 airports with highest number of destinations
A_Destinations <- A_Destinations[1:10, ]
A_Destinations

# Keep airport information for those found in routes (edges)
u <- factor(union(routes_edges$Start, routes_edges$End))
u
airports_nodes <- airports_nodes[(airports_nodes$ID %in% u), ]
airports_nodes
routes_edges_11 <- routes_edges[(routes_edges$Start %in% airports_nodes$ID), ]
routes_edges_11
routes_edges_22 <- routes_edges_11[(routes_edges_11$End %in% airports_nodes$ID), ]
routes_edges_22

# Map all airports and routes onto a graph
g <- graph_from_data_frame(d = routes_edges_22, vertices = airports_nodes, directed=TRUE)
g

# Group routes by number of airlines 
M_routes<-unite(routes_edges, new, c(Start, End), remove=FALSE)
routes<-M_routes %>% group_by(new) %>% tally()

# Find Top 10 popular routes 
Top10routes <- routes[order(-routes[,2]),] 
Top10routes

# Plot and virtualise the graph  
plot(g, vertex.size=5)

## Network Summary ##
# Number of nodes and edges
paste0("Number of nodes: ", vcount(g))
paste0("Number of edges: ", ecount(g))

# Degree of the graph
paste0("Average total degree: ", mean(degree(g, mode="total")))
paste0("Average in-degree-degree: ", mean(degree(g, mode="in")))
paste0("Average out-degree: ", mean(degree(g, mode="out")))

# Mean path length
paste0("Mean path length: ", mean_distance(g))

# Network Diameter
paste0("Network Diameter: ", diameter(g))
get.diameter(g)

# Network Density
edge_density(g)

# Global Clustering Coefficient
paste0("Global Clustering Coefficient: ", transitivity(g))

# Plot Degree Distribution Graph
gD<-degree_distribution(g)
length(gD)
plot(gD,main="Degree Distribution", xlab="Degree", ylab="Probability")

# Plot Degree Log-Log Graph
gD1<-c(1:1827)
logGD1<-log10(gD1)
logGD<-log10(gD)
plot(logGD1,logGD,main="Log-Log Plot", xlab="Degree", ylab="Probability")

# Generate a table which relates clustering coefficient distribution  C(k) with degree k
# Find degree of each node
deg <- degree(g)
clcoeff <- transitivity(g, type="local")
cldist <- cbind(data.frame(deg), clcoeff)
cldist <- cldist %>% 
  group_by(deg) %>% 
  summarize(Ck = mean(clcoeff))

cldist <- cldist[-1,]

# Plot Clustering Coefficient Distribution
cldist %>% ggplot() +
  geom_point(aes(x=deg, y=Ck), size=3) +
  theme(axis.text = element_text(size=14), axis.title = element_text(size=14), plot.title = element_text(size=24)) +
  labs(title="Clustering Coefficient Distribution", x="Degree", y="C(k)")

# Plot Clustering Coefficient Log-Log Graph
cldist %>% ggplot() +
  geom_point(aes(x=deg, y=Ck), size=3) +
  theme(axis.text = element_text(size=14), axis.title = element_text(size=14), plot.title = element_text(size=24)) +
  labs(title="Clustering Coefficient Distribution (log-log)", x="Degree", y="C(k)") +
  scale_x_log10() + scale_y_log10()

#### Perform Community Detection using various Algorithm ####

### Using Spinglass Method ###

# Simplify g with removal of loops and multiedges
gS <- simplify(g)

# Obtain a total of 7 clusters
clustC <- clusters(gS)

# Perform spinglass algorithm for cluster == 1 (also largest cluster)
clustC_max <- induced_subgraph(gS, which(clustC$membership == which.max(clustC$csize)))
spG <- spinglass.community(clustC_max)
length(spG)
paste0("Modularity of Spinglass Algoirthm: ", modularity(spG))

### Using Label Propagation Method ###

labelprop <- cluster_label_prop(g)
length(labelprop)
paste0("Modularity of Label Propagation Algoirthm: ", modularity(labelprop))

### Using Walktrap Method ###

wt <- walktrap.community(g)
wt_mem <- membership(wt)
wt_mem
length(wt)
paste0("Modularity of Walktrap Algoirthm: ", modularity(wt))

### Using Louvain Method ###

gu<-as.undirected(g)
louvain<-cluster_louvain(gu,weights = NULL)
length(louvain)
paste0("Modularity of Louvain Algoirthm: ", modularity(louvain))

# Extract all the members by groups since modularity score for Louvain is the highest 
louvain [1:39]

# Identify 3 largest airport commnunities after using Louvain Method
# Identify Key Player(s) [Air Hub] using various evaluation metric 

# Airport commnunity 1 with 759 nodes
Airports_C1 <- read.csv("Airport in Cluster 1 (759).csv", header=TRUE)
Airports_C1

# Keep airport information for those found in routes (edges)
u <- factor(union(routes_edges$Start, routes_edges$End))
u
Airports_C1<- Airports_C1[(Airports_C1$ID %in% u), ]
Airports_C1<-na.omit(Airports_C1)
Airports_C1
routes_edges_1 <- routes_edges[(routes_edges$Start %in% Airports_C1$ID), ]
routes_edges_2 <- routes_edges_1[(routes_edges_1$End %in% Airports_C1$ID), ]
routes_edges_2
g_C1 <- graph_from_data_frame(d = routes_edges_2, vertices = Airports_C1, directed=TRUE)
plot(g_C1,vertex.size=2)

# Identify Key Hub(s)/ Player(s) based on various evaluation metrics
sort(degree(g_C1))
sort(closeness(g_C1, normalized=TRUE))
sort(betweenness(g_C1))
sort(page_rank(g_C1)$vector)
sort(authority_score(g_C1)$vector)
sort(hub_score(g_C1)$vector)

# Airport commnunity 2 with 601 nodes
Airports_C2 <- read.csv("Airport in Cluster 2 (601).csv", header=TRUE)
Airports_C2

# Keep airport information for those found in routes (edges)
u <- factor(union(routes_edges$Start, routes_edges$End))
u
Airports_C2<- Airports_C2[(Airports_C2$ID %in% u), ]
Airports_C2<-na.omit(Airports_C2)
Airports_C2
routes_edges_1 <- routes_edges[(routes_edges$Start %in% Airports_C2$ID), ]
routes_edges_2 <- routes_edges_1[(routes_edges_1$End %in% Airports_C2$ID), ]
routes_edges_2
g_C2 <- graph_from_data_frame(d = routes_edges_2, vertices = Airports_C2, directed=TRUE)
plot(g_C2,vertex.size=2)

# Identify Key Hub(s)/ Player(s) based on various evaluation metrics
sort(degree(g_C2))
sort(closeness(g_C2, normalized=TRUE))
sort(betweenness(g_C2))
sort(page_rank(g_C2)$vector)
sort(authority_score(g_C2)$vector)
sort(hub_score(g_C2)$vector)

# Airport commnunity 3 with 554 nodes
Airports_C3 <- read.csv("Airport in Cluster 3 (554).csv", header=TRUE)
Airports_C3

# Keep airport information for those found in routes (edges)
u <- factor(union(routes_edges$Start, routes_edges$End))
u
Airports_C3<- Airports_C3[(Airports_C3$ID %in% u), ]
Airports_C3<-na.omit(Airports_C3)
Airports_C3
routes_edges_1 <- routes_edges[(routes_edges$Start %in% Airports_C3$ID), ]
routes_edges_2 <- routes_edges_1[(routes_edges_1$End %in% Airports_C3$ID), ]
routes_edges_2
g_C3 <- graph_from_data_frame(d = routes_edges_2, vertices = Airports_C3, directed=TRUE)
plot(g_C3,vertex.size=2)

# Identify Key Hub(s)/ Player(s) based on various evaluation metrics
sort(degree(g_C3))
sort(closeness(g_C3, normalized=TRUE))
sort(betweenness(g_C3))
sort(page_rank(g_C3)$vector)
sort(authority_score(g_C3)$vector)
sort(hub_score(g_C3)$vector)

