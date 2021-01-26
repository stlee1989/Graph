import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from mpl_toolkits.basemap import Basemap as Basemap
import os
from networkx.algorithms.community.modularity_max import greedy_modularity_communities
from networkx.algorithms.community.label_propagation import label_propagation_communities
from networkx.algorithms.community.quality import performance, coverage
from networkx.algorithms.centrality import betweenness_centrality, degree_centrality, voterank
from collections import Counter
import itertools
import community

os.environ["PROJ_LIB"] = "/Users/gregory/opt/anaconda3/pkgs/basemap-1.2.0-py36h0acbc05_0/lib/python3.6/site-packages/mpl_toolkits/basemap"

############################################# Read datasets #############################################
airport_colnames = ["Airport ID", "Name", "City", "Country", "IATA", "ICAO", "Latitude", "Longitude", "Altitude", 
                    "Timezone", "DST", "Tz database time zone", "Type", "Source"]

route_colnames = ["Airline", "Airline ID", "Source airport", "Source airport ID", "Destination airport", "Destination airport ID",
                    "Codeshare", "Stops", "Equipment"]

airport = pd.read_csv("/Users/gregory/Desktop/Scripts/network analysis/airports.csv", header=None, index_col=False, sep=",", names=airport_colnames)
route = pd.read_csv("/Users/gregory/Desktop/Scripts/network analysis/routes.csv", header=None, index_col=False, sep=",", names=route_colnames)

airport = airport.drop(["IATA", "Timezone", "DST", "Tz database time zone", "Type", "Source", "Altitude"], axis=1)
route = route.drop(["Codeshare", "Stops", "Equipment"], axis=1)

############################################# Drop rows in route dataframe containing "\N" in Source Airport ID and Destination Airport ID #############################################
route = route[route["Source airport ID"] != "\\N"]
route =route[route["Destination airport ID"] != "\\N"]

############################################# Convert data type of ID columns from string to interger #############################################
route = route.astype({"Source airport ID": np.int64, "Destination airport ID": np.int64}) 

############################################# Drop rows in route dataframe where Source Airport ID or Destination Airport ID not exist in airport dataframe Airport ID #############################################
route = route[route["Source airport ID"].isin(airport["Airport ID"])]
route = route[route["Destination airport ID"].isin(airport["Airport ID"])]

############################################# Drop route with similar source and destination IDs #############################################
route = route[route["Source airport ID"] != route["Destination airport ID"]]

############################################# Initiate graph #############################################
# Assume as undirected as certain communities algorithm cannot be used for directed
# Assume airline always travel 2-ways between source and destination
G = nx.Graph()

############################################# Add nodes and edges #############################################
airport_node = [i for i in airport["Airport ID"].values.tolist()]
# G.add_nodes_from(airport_node)    # Line commented so that only nodes existing in edges are added to original graph of G. Meaning that not all airports are used in routes.

route_edge = list(zip(route["Source airport ID"].values.tolist(), route["Destination airport ID"].values.tolist()))
G.add_edges_from(route_edge)

############################################# Draw graph based on selected nodes and edges #############################################
def draw_graph_on_map(G, save_filename, group_by_color=False, num_of_groups=0, nodelist=None):
    ############################################# Initiate basemap #############################################
    m = Basemap(projection='robin', lon_0=0, resolution='c')
    m.drawmapboundary(fill_color='white', zorder=1)
    m.fillcontinents(color='lightgray',lake_color='white', zorder=2)              
    m.drawcoastlines(linewidth=0.2, zorder=3)
    m.drawcountries(linewidth=0.2, color="dimgray", zorder=4)

    ############################################# Compute map projection from lat/long grid #############################################
    lat = airport["Latitude"].values.tolist()
    long = airport["Longitude"].values.tolist()
    mx, my = m(long, lat)

    ############################################# Key-value for airportid (nodes) and latitude, longitude #############################################
    values = tuple(zip(mx, my))
    pos = dict(zip(airport["Airport ID"].values.tolist(), values))

    if group_by_color == False:
        nodes = nx.draw_networkx_nodes(G, pos, node_size=5,node_color='red', alpha=1.0, with_labels=False)
        edges = nx.draw_networkx_edges(G, pos, edge_color="black", alpha=0.1, arrows=False, width=0.2, arrowsize=5)
        nodes.set_zorder(5)
        edges.set_zorder(5)
    
    elif group_by_color == True:
        # Note: edges are not plotted to preserve image clarity
        # Note: if edges are required, rollback and generated multiple subgraphs instead of via filtering orignal graph with nodelist
        if num_of_groups <= 10:
            color = ["tab:blue", "tab:orange", "tab:green", "tab:red", "tab:purple", "tab:brown", "tab:pink", "tab:gray", "tab:olive", "tab:cyan"]
            nodelist = nodelist
            for i in range(num_of_groups):
                nodes = nx.draw_networkx_nodes(G, pos, node_size=5,node_color=color[i], alpha=1.0, with_labels=False,nodelist=nodelist[i])
                nodes.set_zorder(5)
        else:
            raise Exception("Reduce number of groups to be less than 10")

    plt.savefig("/Users/gregory/Desktop/Scripts/network analysis/{}.pdf".format(save_filename))
    plt.close()

############################################# Main properties of the original network #############################################
def get_number_of_nodes(G):
    return G.number_of_nodes()

def get_number_of_edges(G):
    return G.number_of_edges()

def get_nodes(G):
    return G.nodes()

def get_edges(G):
    return G.edges()

def get_density_of_graph_in_percentage(G):
    edge = get_number_of_edges(G)
    node = get_number_of_nodes(G)
    density = (2*edge)/(node*(node-1))
    density = round(density*100, 3)
    return density

def get_average_clustering_coefficient(G):
    return round(nx.average_clustering(G), 3)

def plot_degree_dist(G, save_filename):
    degrees = [G.degree(n) for n in G.nodes()]
    average_degree = round((2*G.number_of_edges())/G.number_of_nodes(), 3)
    max_degree = np.max(degrees)
    min_degree = np.min(degrees)
    weight = np.ones_like(degrees)/len(degrees)
    plt.hist(degrees, color="blue", weights=weight, bins=np.arange(np.max(degrees)))
    plt.xlabel("degree (k)")
    plt.ylabel("degree distribution p(k)")
    plt.title("Degree Distribution")
    plt.axvline(average_degree, color="red", linestyle="dashed", linewidth=0.5)
    plt.annotate("Max. degree: {0}\nMin. degree: {1}\nAverage degree: {2}".format(max_degree, min_degree, average_degree), xycoords="figure fraction", xy=(0.63, 0.75))
    plt.savefig("/Users/gregory/Desktop/Scripts/network analysis/{}.pdf".format(save_filename))
    plt.close()

print("\nProperties of original network")
print("Number of nodes: {}".format(get_number_of_nodes(G)))
print("Number of edges: {}".format(get_number_of_edges(G)))
print("Density of network: {}%".format(get_density_of_graph_in_percentage(G)))
print("Average clustering coefficient: {}".format(get_average_clustering_coefficient(G)))
plot_degree_dist(G, "G_degree_distribution")
draw_graph_on_map(G, "Original_network")   # Draw map on original network graph showing only list of nodes generated by clustering
############################################# Community detection #############################################

# 1. Clauset-Newman-Moore greedy modularity maximization
def clauset_newman_moore_top_n_cluster(G, num_of_groups):
    # https://networkx.github.io/documentation/stable/reference/algorithms/generated/networkx.algorithms.community.modularity_max.greedy_modularity_communities.html#networkx.algorithms.community.modularity_max.greedy_modularity_communities
    "To retrieve top num_of_groups largest clusters and their subgraphs based Clauset-Newman-Moore greedy modularity maximization"
    C = greedy_modularity_communities(G)
    C_list_all = list(sorted(C, key=len, reverse=True))
    C_list = C_list_all[:num_of_groups]
    C_len = [len(i) for i in C_list]
    print("Number of nodes in each clauset_newman_moore cluster: {}".format(C_len))

    list_of_subgraphs = [G.subgraph(C_list[i]) for i in range(num_of_groups)]
    dict_of_subgraphs = {} # Assign each subgraph in list_of_subgraphs to a python variable for ease of access
    for i in range(num_of_groups):
        dict_of_subgraphs["subgraph_clauset_new_man{0}".format(i)] = list_of_subgraphs[i]
    return C_list, dict_of_subgraphs, C_list_all

C = clauset_newman_moore_top_n_cluster(G, num_of_groups=7) # Attempt to cluster routes into 6 territories == 6 continents on map
draw_graph_on_map(G, "clauset_new_man", group_by_color=True, num_of_groups=7, nodelist=C[0])   # Draw map on original network graph showing only list of nodes generated by clustering
keys_of_dict_of_subgraphs_in_C = list(C[1].keys())   
values_of_dict_of_subgraphs_in_C = list(C[1].values()) # Subgraph stored in list. Number of subgraphs depend on num_of_groups

for i in range(len(C[1])):
    print("\nProperties of {}".format(keys_of_dict_of_subgraphs_in_C[i]))
    print("Number of nodes: {}".format(get_number_of_nodes(values_of_dict_of_subgraphs_in_C[i])))
    print("Number of edges: {}".format(get_number_of_edges(values_of_dict_of_subgraphs_in_C[i])))
    print("Density of network: {}%".format(get_density_of_graph_in_percentage(values_of_dict_of_subgraphs_in_C[i])))
    print("Average clustering coefficient: {}".format(get_average_clustering_coefficient(values_of_dict_of_subgraphs_in_C[i])))
    plot_degree_dist(values_of_dict_of_subgraphs_in_C[i], "{}".format(keys_of_dict_of_subgraphs_in_C[i]))

# 2. Label_propagation_communities
def community_label_propagation(G, num_of_groups):
    # https://networkx.github.io/documentation/stable/reference/algorithms/generated/networkx.algorithms.community.label_propagation.label_propagation_communities.html#networkx.algorithms.community.label_propagation.label_propagation_communities
    Lab = label_propagation_communities(G)
    Lab_list_all = list(sorted(Lab, key=len, reverse=True))
    Lab_list = Lab_list_all[:num_of_groups]
    Lab_len = [len(i) for i in Lab_list]
    print("Number of nodes in each label_propagation cluster: {}".format(Lab_len))

    list_of_subgraphs = [G.subgraph(Lab_list[i]) for i in range(num_of_groups)]
    dict_of_subgraphs = {} # Assign each subgraph in list_of_subgraphs to a python variable for ease of access
    for i in range(num_of_groups):
        dict_of_subgraphs["subgraph_label_propagation_{0}".format(i)] = list_of_subgraphs[i]
    return Lab_list, dict_of_subgraphs, Lab_list_all

Lab = community_label_propagation(G, num_of_groups=7)
draw_graph_on_map(G, "label_propagation", group_by_color=True, num_of_groups=7, nodelist=Lab[0])   # Draw map on original network graph showing only list of nodes generated by clustering
keys_of_dict_of_subgraphs_in_Lab = list(Lab[1].keys())   
values_of_dict_of_subgraphs_in_Lab = list(Lab[1].values()) # Subgraph stored in list. Number of subgraphs depend on num_of_groups

for i in range(len(Lab[1])):
    print("\nProperties of {}".format(keys_of_dict_of_subgraphs_in_Lab[i]))
    print("Number of nodes: {}".format(get_number_of_nodes(values_of_dict_of_subgraphs_in_Lab[i])))
    print("Number of edges: {}".format(get_number_of_edges(values_of_dict_of_subgraphs_in_Lab[i])))
    print("Density of network: {}%".format(get_density_of_graph_in_percentage(values_of_dict_of_subgraphs_in_Lab[i])))
    print("Average clustering coefficient: {}".format(get_average_clustering_coefficient(values_of_dict_of_subgraphs_in_Lab[i])))
    plot_degree_dist(values_of_dict_of_subgraphs_in_Lab[i], "{}".format(keys_of_dict_of_subgraphs_in_Lab[i]))

# 3. community_best_partition
def community_best_partition(G, num_of_groups):
    # https://python-louvain.readthedocs.io/en/latest/api.html
    Part = community.best_partition(G)
    Part_list_all = {n:[k for k in Part.keys() if Part[k] == n] for n in set(Part.values())}
    Part_list_all = sorted(Part_list_all.values(), key=len, reverse=True)
    Part_list = Part_list_all[:num_of_groups]
    Part_len = [len(i) for i in Part_list]
    print("Number of nodes in each best_partition cluster: {}".format(Part_len)) 

    list_of_subgraphs = [G.subgraph(Part_list[i]) for i in range(num_of_groups)]
    dict_of_subgraphs = {} # Assign each subgraph in list_of_subgraphs to a python variable for ease of access
    for i in range(num_of_groups):
        dict_of_subgraphs["subgraph_best_partition_{0}".format(i)] = list_of_subgraphs[i]

    return Part_list, dict_of_subgraphs, Part_list_all

Part = community_best_partition(G, num_of_groups=7)
draw_graph_on_map(G, "best_partition", group_by_color=True, num_of_groups=7, nodelist=Part[0])   # Draw map on original network graph showing only list of nodes generated by clustering
keys_of_dict_of_subgraphs_in_Part = list(Part[1].keys())   
values_of_dict_of_subgraphs_in_Part = list(Part[1].values()) # Subgraph stored in list. Number of subgraphs depend on num_of_groups

for i in range(len(Part[1])):
    print("\nProperties of {}".format(keys_of_dict_of_subgraphs_in_Part[i]))
    print("Number of nodes: {}".format(get_number_of_nodes(values_of_dict_of_subgraphs_in_Part[i])))
    print("Number of edges: {}".format(get_number_of_edges(values_of_dict_of_subgraphs_in_Part[i])))
    print("Density of network: {}%".format(get_density_of_graph_in_percentage(values_of_dict_of_subgraphs_in_Part[i])))
    print("Average clustering coefficient: {}".format(get_average_clustering_coefficient(values_of_dict_of_subgraphs_in_Part[i])))
    plot_degree_dist(values_of_dict_of_subgraphs_in_Part[i], "{}".format(keys_of_dict_of_subgraphs_in_Part[i]))

def community_partitioning_performance_quality(G, seq):
    # https://networkx.github.io/documentation/stable/reference/algorithms/generated/networkx.algorithms.community.quality.coverage.html#networkx.algorithms.community.quality.coverage
    # https://networkx.github.io/documentation/stable/reference/algorithms/generated/networkx.algorithms.community.quality.performance.html#networkx.algorithms.community.quality.performance
    per = performance(G, seq)
    cov = coverage(G, seq)
    print("\nPerformance of the clustering: {}".format(round(per, 3)))
    print("Coverage of the clustering: {}".format(round(cov, 3)))
    return per, cov

community_partitioning_performance_quality(G, C[2])
community_partitioning_performance_quality(G, Lab[2])
community_partitioning_performance_quality(G, Part[2])

#############################################  key players based on label propagation partitioned subgraphs #############################################

def measure_centrality_top_n_nodes(G, n):
    dict_of_nodes_degree_centrality = degree_centrality(G)
    dict_of_nodes_betweenness_centrality = betweenness_centrality(G)
    list_of_nodes_voterank_centrality = voterank(G)

    # Reduce all values to 3 decimal places
    dict_of_nodes_degree_centrality = {k:round(v, 3) for k, v in dict_of_nodes_degree_centrality.items()}
    dict_of_nodes_betweenness_centrality = {k:round(v, 3) for k, v in dict_of_nodes_betweenness_centrality.items()}

    # Get top 3 ranked nodes centrality
    top_dict_of_nodes_degree_centrality = Counter(dict_of_nodes_degree_centrality).most_common(n)
    top_dict_of_nodes_betweenness_centrality = Counter(dict_of_nodes_betweenness_centrality).most_common(n)
    top_list_of_nodes_voterank_centrality = list_of_nodes_voterank_centrality[:n]

    # Convert voterank list to dict with highest vote holding value of 1
    top_list_of_nodes_voterank_centrality = {k: v for v, k in enumerate(top_list_of_nodes_voterank_centrality, 1)}.items()
    return top_dict_of_nodes_degree_centrality, top_dict_of_nodes_betweenness_centrality, top_list_of_nodes_voterank_centrality

def get_properties_of_key_nodes_from_dict(dict_of_nodes, col_name, filename):
    df_property = pd.DataFrame(dict_of_nodes, columns=["Airport ID", "{}".format(col_name)])
    df_airport = airport[["Airport ID", "Name", "City", "Country"]]
    df_merge = pd.merge(df_property, df_airport, left_on="Airport ID", right_on="Airport ID", how="inner")
    df_merge.to_excel("{}.xlsx".format(filename))
    print("\n")
    return df_merge

# Output data
centrality_of_Lab_subgraph_0 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[0], n=5)
print("\nDegree centrality of subgraph_0")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_0[0], "degree_centrality", "subgraph_0_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_0[1], "betweenness_centrality", "subgraph_0_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_0[2], "voterank_centrality", "subgraph_0_voterank"))

centrality_of_Lab_subgraph_1 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[1], n=5)
print("\nDegree centrality of subgraph_1")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_1[0], "degree_centrality", "subgraph_1_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_1[1], "betweenness_centrality", "subgraph_1_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_1[2], "voterank_centrality", "subgraph_1_voterank"))

centrality_of_Lab_subgraph_2 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[2], n=5)
print("\nDegree centrality of subgraph_2")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_2[0], "degree_centrality", "subgraph_2_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_2[1], "betweenness_centrality", "subgraph_2_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_2[2], "voterank_centrality", "subgraph_2_voterank"))

centrality_of_Lab_subgraph_3 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[3], n=5)
print("\nDegree centrality of subgraph_3")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_3[0], "degree_centrality", "subgraph_3_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_3[1], "betweenness_centrality", "subgraph_3_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_3[2], "voterank_centrality", "subgraph_3_voterank"))

centrality_of_Lab_subgraph_4 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[4], n=5)
print("\nDegree centrality of subgraph_4")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_4[0], "degree_centrality", "subgraph_4_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_4[1], "betweenness_centrality", "subgraph_4_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_4[2], "voterank_centrality", "subgraph_4_voterank"))

centrality_of_Lab_subgraph_5 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[5], n=5)
print("\nDegree centrality of subgraph_5")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_5[0], "degree_centrality", "subgraph_5_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_5[1], "betweenness_centrality", "subgraph_5_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_5[2], "voterank_centrality", "subgraph_5_voterank"))

centrality_of_Lab_subgraph_6 = measure_centrality_top_n_nodes(values_of_dict_of_subgraphs_in_Lab[6], n=5)
print("\nDegree centrality of subgraph_6")
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_6[0], "degree_centrality", "subgraph_6_degree"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_6[1], "betweenness_centrality", "subgraph_6_betweenness"))
print(get_properties_of_key_nodes_from_dict(centrality_of_Lab_subgraph_6[2], "voterank_centrality", "subgraph_6_voterank"))

print("End")