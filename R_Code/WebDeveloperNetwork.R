# Installing libraries
install.packages("igraph")
install.packages(c("ape", "network", "sna"))
library(ape)
library(network)
library(sna)
# clear everything out of memory
rm(list=ls())

# Loading libraries
library(igraph)

# Setting work directory
dir_path <- "~/Desktop/Projects/GitHubNetworkAnalysis"
setwd(dir_path)
infileOne<-"RoleFlag.csv"
infileTwo<-"Edges.csv"

# Creating dataframe for Nodes and Edges
RoleFlag <- read.csv(file="RoleFlag.csv", header=TRUE, sep=",")
gitHubEdges <- read.csv(file="Edges.csv", header=TRUE, sep=",")
View(RoleFlag)
View(gitHubEdges)
nodeFrame <- RoleFlag[1]
EdgeFrame <- gitHubEdges

# Read Dataframe
# Create network graph
gitHubNetwork=graph.data.frame(EdgeFrame, directed = FALSE, vertices= nodeFrame)
# Understand no of edges and Nodes
No_of_Edges <- ecount(gitHubNetwork)
No_of_vertices <- vcount(gitHubNetwork)
#  No_of_Edges
# [1] 289003
#  No_of_vertices
# [1] 37700

# Checking it is a weighted network
is.weighted(gitHubNetwork)
# FALSE the network is not weighted system

# Lets analyse the type of network
is.simple(gitHubNetwork)
is.connected(gitHubNetwork)
# The network is both simple and Connected.

# Constructing and Extracting info from Role
RoleFlag$Role <- ifelse(RoleFlag$ml_target == 1, "ml", "dev" )
RoleFlag$ml_target<-NULL
RoleFlag$name<-NULL

github_roles <- gitHubEdges
colnames(RoleFlag)[1] <- "id_1"
edge_role<-merge(x=github_roles,y=RoleFlag,by="id_1",all.x=TRUE)
colnames(RoleFlag)[1] <- "id_2"
colnames(RoleFlag)[2] <- "Role2"
edge_role_whole<-merge(x=edge_role,y=RoleFlag,by="id_2",all.x=TRUE)
str(edge_role_whole)
edge_role_whole$network <- ifelse(edge_role_whole$Role == "dev" & edge_role_whole$Role2 == "dev" , "developers", ifelse(edge_role_whole$Role == "ml" & edge_role_whole$Role2 == "ml"  , "machine_learning", "mixed" ))
developer_nw <- edge_role_whole[edge_role_whole$network=="developers",]
machine_learning_nw <- edge_role_whole[edge_role_whole$network=="machine_learning",]
mixed_nw <- edge_role_whole[edge_role_whole$network=="mixed",]


# Analysing Developer network
GitHub_Dev_Network <- developer_nw
GitHub_Dev_Network$Role <- NULL
GitHub_Dev_Network$Role2 <- NULL
GitHub_Dev_Network$network <- NULL
View(GitHub_Dev_Network)
write.csv(GitHub_Dev_Network,"GitHub_Dev_Network.csv")
GitHub_Developer_Edges <- GitHub_Dev_Network
Github_Developer_Nodes <- read.csv(file="GitHub_Dev_Network_Nodes.csv", header=TRUE, sep=",")
gitHubNetwork_Dev_graph=graph.data.frame(GitHub_Developer_Edges, directed = FALSE, vertices= Github_Developer_Nodes)
No_of_Edges <- ecount(gitHubNetwork_Dev_graph)
No_of_vertices <- vcount(gitHubNetwork_Dev_graph)
is.simple(gitHubNetwork_Dev_graph)
# Is a simple Graph
is.connected(gitHubNetwork_Dev_graph)
# It is not a connected graph


# total Degree of a network
degree_gitHubNetwork_Dev <- degree(gitHubNetwork_Dev_graph ,mode="total")
View(degree_gitHubNetwork_Dev)
mean(degree_gitHubNetwork_Dev)
table(degree_gitHubNetwork_Dev)
hist(degree_gitHubNetwork_Dev, breaks=20 , xlim=c(0,50), ylim=c(0,10000))
sort(degree_gitHubNetwork_Dev)
class(degree_gitHubNetwork_Dev)
write.csv(degree_gitHubNetwork_Dev,"degree_gitHubNetwork_Dev.csv")

# Top five degree of centrality
#31890	8194
#27803	5465
#35773	3204
#19222	2714
#13638	2288

# There is in and out degree because it is a directed network

# Strength
gitHubNetwork_Dev_strength <- strength(gitHubNetwork_Dev_graph)
View(gitHubNetwork_Dev_strength)
table(gitHubNetwork_Dev_strength)
mean(gitHubNetwork_Dev_strength)
median(gitHubNetwork_Dev_strength)
#31890	8194
#27803	5465
#35773	3204
#19222	2714
#13638	2288
#36652	1926

# closeness
gitHubNetwork_Dev_graph_closeness <- closeness(gitHubNetwork_Dev_graph, normalized=TRUE)
View(gitHubNetwork_Dev_graph_closeness)
table(gitHubNetwork_Dev_graph_closeness)
mean(gitHubNetwork_Dev_graph_closeness)  
median(gitHubNetwork_Dev_graph_closeness) 
max(gitHubNetwork_Dev_graph_closeness)
min(gitHubNetwork_Dev_graph_closeness)
# Top Five Closeness Measure for nodes
#31890	0.04027587
#27803	0.04019001
#19222	0.03990460
#35773	0.03985191
#13638	0.03977968
#19253	0.03974985
# 0.03830238 Mean
# 0.03839923 Median
# We see that mean and median are close so all the nodes have equal closeness centrality
# We see that the values are less 

# Betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
# Node betweenness
gitHubNetwork_Dev_Betweeness <- round(betweenness(gitHubNetwork_Dev_graph, v=V(gitHubNetwork_Dev_graph), directed = FALSE, nobigint =TRUE, normalized = FALSE))
View(gitHubNetwork_Dev_Betweeness)
max(gitHubNetwork_Dev_Betweeness) # 112348272
median(gitHubNetwork_Dev_Betweeness)  # 817
mean(gitHubNetwork_Dev_Betweeness) # 28937.97
min(gitHubNetwork_Dev_Betweeness) # 0
#31890	112348272
#27803	71544930
#19222	24206266
#35773	22291964
#13638	16567920

# Edge betwenness
gitHubNetwork_Dev_Edgebetweens<-edge.betweenness(gitHubNetwork_Dev_graph, e=E(gitHubNetwork_Dev_graph), directed = FALSE)
max(gitHubNetwork_Dev_Edgebetweens) 0.03839923  
median(gitHubNetwork_Dev_Edgebetweens) 
mean(gitHubNetwork_Dev_Edgebetweens) 
min(gitHubNetwork_Dev_Edgebetweens)

# Local clustering coefficients
gitHubNetwork_Dev_clustering <- transitivity(gitHubNetwork_Dev_graph, type="local", vids=V(gitHubNetwork_Dev_graph)) 
View(gitHubNetwork_Dev_clustering)
table(gitHubNetwork_Dev_clustering)
max(gitHubNetwork_Dev_clustering) 
median(gitHubNetwork_Dev_clustering) 
mean(gitHubNetwork_Dev_clustering) 
min(gitHubNetwork_Dev_clustering)

# Embeddedness/ inverse of structural hole access (see Burt 2004)
gitHubNetwork_Dev_Embeddness <- round(constraint(gitHubNetwork_Dev_graph, nodes=V(gitHubNetwork_Dev_graph)), digits=4)
View(gitHubNetwork_Dev_Embeddness)
max(gitHubNetwork_Dev_Embeddness) 
median(gitHubNetwork_Dev_Embeddness) 
mean(gitHubNetwork_Dev_Embeddness) 
min(gitHubNetwork_Dev_Embeddness)

#reciprocity
gitHubNetwork_Dev_reciprocity <- reciprocity(gitHubNetwork_Dev_graph)
View(gitHubNetwork_Dev_reciprocity)
max(gitHubNetwork_Dev_reciprocity) 
median(gitHubNetwork_Dev_reciprocity) 
mean(gitHubNetwork_Dev_reciprocity) 
min(gitHubNetwork_Dev_reciprocity)

# Transitivity
transitivity(gitHubNetwork_Dev_graph)

# Avg. path length 
average.path.length(gitHubNetwork_Dev_graph, directed=FALSE)

# Diameter
diameter(gitHubNetwork_Dev_graph)

# Summarize the graph structure
summary(g_SAPSub_simpl)


# Analysing people with 
neighbor <- neighbors(gitHubNetwork_Dev_graph, v=c('31890'))
table(neighbor)


# Choosing a subset of nodes.
Developer_Subset <- SubsetNodes
Developer_Subset_Edges <- edge_role<-merge(x=github_roles,y=RoleFlag,by="id_1",all.x=TRUE)



# select the nodes having these names
selnodes <- V(gitHubNetwork_Dev_graph)[name %in% nodes_of_interest]
# get their network neighborhood 
selegoV <- ego(gitHubNetwork_Dev_graph, order=1, nodes = selnodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(gitHubNetwork_Dev_graph,unlist(selegoV))

# plot the subgraph
plot(selegoG)
l <- layout.forceatlas2(selegoG, iterations=10000, plotstep=500)
i <- plot(layout.fruchterman.reingold(selegoG))

nodes_of_interest <- c("139",
                       "227",
                       "243",
                       "265",
                       "266",
                       "320",
                       "416",
                       "447",
                       "515",
                       "658",
                       "666",
                       "783",
                       "836",
                       "885",
                       "898",
                       "952",
                       "966",
                       "993",
                       "1093",
                       "1124",
                       "1150",
                       "1175",
                       "1191",
                       "1219",
                       "1241",
                       "1278",
                       "1401",
                       "1406",
                       "1469",
                       "1509",
                       "1538",
                       "1564",
                       "1749",
                       "1810",
                       "1874",
                       "2028",
                       "2068",
                       "2175",
                       "2180",
                       "2205",
                       "2208")

devnodes <- V(gitHubNetwork_Dev_graph)[name %in% nodes_of_interest]
devnodesV <- ego(gitHubNetwork_Dev_graph, order=1, nodes = devnodes, mode = "all", mindist = 0)
devnodesG <- induced_subgraph(gitHubNetwork_Dev_graph,unlist(devnodesV))
plot(devnodesG)
i <- layout.fruchterman.reingold(devnodesG)
plot(devnodesG,layout=i)

plot.igraph(devnodesG,vertex.size=12,vertex.label=NA,vertex.color="pink",edge.color="black",
            layout=layout.fruchterman.reingold(g, niter=10000, area=150*vcount(g)^2))

lp <- fastgreedy.community(devnodesG)
plot(lp, devnodesG,vertex.label= NA)


# 
nodes_of_interests <- c("20",
"51",
"66",
"68",
"76",
"85",
"106",
"137",
"198",
"208",
"222",
"254",
"257",
"287",
"295",
"9",
"31",
"40",
"74",
"89",
"110",
"116",
"162",
"180",
"210",
"272",
"275",
"6",
"12",
"26",
"30",
"58,",
"113",
"114",
"118",
"135"
)
devnodes <- V(gitHubNetwork_Dev_graph)[name %in% nodes_of_interests]
devnodesV <- ego(gitHubNetwork_Dev_graph, order=1, nodes = devnodes, mode = "all", mindist = 0)
devnodesG <- induced_subgraph(gitHubNetwork_Dev_graph,unlist(devnodesV))
plot(devnodesG)
i <- layout.fruchterman.reingold(devnodesG)
plot(devnodesG,layout=i)

plot.igraph(devnodesG,vertex.size=12,vertex.label=NA,vertex.color="pink",edge.color="black",
            layout=layout.fruchterman.reingold(g, niter=10000, area=50*vcount(g)^2))

lp <- fastgreedy.community(devnodesG)
lp <- walktrap.community(devnodesG)
plot(lp, devnodesG,vertex.label= NA)
membership(lp)

nodes_of_interests <- c(
'7177',
'14666',
'15669',
'22504',
'30379',
'31364')

devnodes <- V(gitHubNetwork_Dev_graph)[name %in% nodes_of_interests]
devnodesV <- ego(gitHubNetwork_Dev_graph, order=1, nodes = devnodes, mode = "all", mindist = 0)
devnodesG <- induced_subgraph(gitHubNetwork_Dev_graph,unlist(devnodesV))
plot(devnodesG)
i <- layout.fruchterman.reingold(devnodesG)
plot(devnodesG,layout=i)

plot.igraph(devnodesG,vertex.size=12,vertex.label=NA,vertex.color="pink",edge.color="black",
            layout=layout.fruchterman.reingold(g, niter=10000, area=50*vcount(g)^2))

lp <- fastgreedy.community(devnodesG)
lp <- walktrap.community(devnodesG)
plot(lp, devnodesG,vertex.label= NA)

# 31890
nodes_of_interests <- c("31890")
devnodes <- V(gitHubNetwork_Dev_graph)[name %in% nodes_of_interests]
devnodesV <- ego(gitHubNetwork_Dev_graph, order=1, nodes = devnodes, mode = "all", mindist = 0)
devnodesG <- induced_subgraph(gitHubNetwork_Dev_graph,unlist(devnodesV))
plot.igraph(devnodesG,vertex.size=12,vertex.label=NA,vertex.color="pink",edge.color="black",
            layout=layout.fruchterman.reingold(g, niter=10000, area=50*vcount(g)^2))
lp <- walktrap.community(devnodesG)
plot(lp, devnodesG,vertex.label= NA)
view <- membership(lp)
class(view)
View(view)

