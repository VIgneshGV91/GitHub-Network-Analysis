# Installing libraries
install.packages("igraph")

# clear everything out of memory
rm(list=ls())

# Loading libraries
library(igraph)



# Setting work directory
dir_path <- "E:/IDS564 Social Media/Project"
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


# Analysing Mixed network
GitHub_mix_Network <- mixed_nw
View(GitHub_mix_Network)
GitHub_mix_Network$Role <- NULL
GitHub_mix_Network$Role2 <- NULL
GitHub_mix_Network$network <- NULL
View(GitHub_mix_Network)
write.csv(GitHub_mix_Network,"GitHub_mix_Network.csv")
GitHub_mixed_Edges <- GitHub_mix_Network
Github_mixed_Nodes <- read.csv(file="GitHub_mix_Network_Nodes.csv", header=TRUE, sep=",")
gitHubNetwork_mix_graph=graph.data.frame(GitHub_mixed_Edges, directed = FALSE, vertices= Github_mixed_Nodes)

plot(gitHubNetwork_mix_graph)

No_of_Edges <- ecount(gitHubNetwork_mix_graph)
#44696
No_of_vertices <- vcount(gitHubNetwork_mix_graph)
#18635

is.simple(gitHubNetwork_mix_graph)

# Is a simple Graph
is.connected(gitHubNetwork_mix_graph)

# It is not a connected graph


# total Degree of a network
degree_gitHubNetwork_mix <- degree(gitHubNetwork_mix_graph ,mode="total")
a<-View(degree_gitHubNetwork_mix)
write.csv(degree_gitHubNetwork_mix,"degree.csv")

table(degree_gitHubNetwork_mix)
mean(degree_gitHubNetwork_mix)
#4.79
hist(degree_gitHubNetwork_mix, breaks=5 , xlim=c(0,2000), ylim=c(0,50000))
sort(-degree_gitHubNetwork_mix)
# Top five degree of centrality
#27803	1620
#31890	1264
#23589	864
#33671	655
#22321	540

# There is in and out degree because it is a directed network

# Strength
gitHubNetwork_mix_strength <- strength(gitHubNetwork_mix_graph)
View(gitHubNetwork_mix_strength)
table(gitHubNetwork_mix_strength)
mean(gitHubNetwork_mix_strength)
#4.80
median(gitHubNetwork_mix_strength)
#2
sort(-gitHubNetwork_mix_strength)
#27803	1620
#31890	1264
#23589	864
#33671	655
#22321	540

# closeness
gitHubNetwork_mix_graph_closeness <- closeness(gitHubNetwork_mix_graph, normalized=TRUE)
View(gitHubNetwork_mix_graph_closeness)
table(gitHubNetwork_mix_graph_closeness)
mean(gitHubNetwork_mix_graph_closeness)  
median(gitHubNetwork_mix_graph_closeness) 
max(gitHubNetwork_mix_graph_closeness)
min(gitHubNetwork_mix_graph_closeness)
sort(-gitHubNetwork_mix_graph_closeness)
# Top Five Closeness Measure for nodes
#27803	0.001568363
#31890	0.001567530
#23589	0.001567362
#22321	0.001567219
#26649	0.001567211

# 0.001512422 Mean
# 0.001564012 Median
# We see that mean and median are close so all the nodes have equal closeness centrality
# We see that the values are less 

# Betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
# Node betweenness
gitHubNetwork_mix_Betweeness <- round(betweenness(gitHubNetwork_mix_graph, v=V(gitHubNetwork_mix_graph), directed = FALSE, nobigint =TRUE, normalized = FALSE))
View(gitHubNetwork_mix_Betweeness)
max(gitHubNetwork_mix_Betweeness) # 58392850
median(gitHubNetwork_mix_Betweeness)  # 491
mean(gitHubNetwork_mix_Betweeness) # 30908.13
min(gitHubNetwork_mix_Betweeness) # 0
sort(-gitHubNetwork_mix_Betweeness)
#27803	58392850
#31890	31692253
#23589	15061894
#33671	12708451
#22321	8259632

# Edge betwenness
gitHubNetwork_mix_Edgebetweens<-edge.betweenness(gitHubNetwork_mix_graph, e=E(gitHubNetwork_mix_graph), directed = FALSE)
max(gitHubNetwork_mix_Edgebetweens) #2275535   
median(gitHubNetwork_mix_Edgebetweens) #11356.36 
mean(gitHubNetwork_mix_Edgebetweens) #16510.75
min(gitHubNetwork_mix_Edgebetweens) #1

# Local clustering coefficients
gitHubNetwork_mix_clustering <- transitivity(gitHubNetwork_mix_graph, type="local", vids=V(gitHubNetwork_mix_graph)) 
View(clustering_SAP)
table(clustering_SAP)
max(gitHubNetwork_mix_clustering) 
median(gitHubNetwork_mix_clustering) 
mean(gitHubNetwork_mix_clustering) 
min(gitHubNetwork_mix_clustering)

# Embeddedness/ inverse of structural hole access (see Burt 2004)
gitHubNetwork_mix_Embeddness <- round(constraint(gitHubNetwork_mix_graph, nodes=V(gitHubNetwork_mix_graph)), digits=4)
View(gitHubNetwork_Dev_Embeddness)
max(gitHubNetwork_mix_Embeddness) #1 
median(gitHubNetwork_mix_Embeddness) #0.5
mean(gitHubNetwork_mix_Embeddness) #0.57
min(gitHubNetwork_mix_Embeddness) #6e-04

#reciprocity
reciprocity(gitHubNetwork_mix_graph)
#1

# Transitivity
transitivity(gitHubNetwork_mix_graph)
#0

# Avg. path length 
average.path.length(gitHubNetwork_mix_graph, directed=TRUE)
#4.55

# Diameter
diameter(gitHubNetwork_mix_graph)
#13

plot(gitHub, edge.arrow.size=.2, vertex.color="orange", vertex.label.color="black") 

sub_net<-induced.subgraph(gitHubNetwork_mix_graph, v=c('14954','144',
                                                       '6042',
                                                       '10983',
                                                       '15139',
                                                       '17865',
                                                       '21035',
                                                       '23415',
                                                       '26492',
                                                       '28595',
                                                       '32197',
                                                       '35601',
                                                       '1032',
                                                       '2415',
                                                       '937',
                                                       '6059',
                                                       '11260',
                                                       '15382',
                                                       '17871',
                                                       '21047',
                                                       '23740',
                                                       '26493',
                                                       '28713',
                                                       '32233',
                                                       '35822',
                                                       '364',
                                                       '3436',
                                                       '1228',
                                                       '6228',
                                                       '11365',
                                                       '15485',
                                                       '18154',
                                                       '21262',
                                                       '23798',
                                                       '26618',
                                                       '29055',
                                                       '32369',
                                                       '36011',
                                                       '2729',
                                                       '11528',
                                                       '1445',
                                                       '6256',
                                                       '11590',
                                                       '15528',
                                                       '18222',
                                                       '21309',
                                                       '23879',
                                                       '26683',
                                                       '29067',
                                                       '32391',
                                                       '36069',
                                                       '1377',
                                                       '6047',
                                                       '5419',
                                                       '10743',
                                                       '13978',
                                                       '17554',
                                                       '20340',
                                                       '22872',
                                                       '26159',
                                                       '28153',
                                                       '31336',
                                                       '35090',
                                                       '27843',
                                                       '24720',
                                                       '34236',
                                                       '5513',
                                                       '10772',
                                                       '14083',
                                                       '17560',
                                                       '20605',
                                                       '22881',
                                                       '26165',
                                                       '28296',
                                                       '31351',
                                                       '35114',
                                                       '10966',
                                                       '18488',
                                                       '11267',
                                                       '5973',
                                                       '10922',
                                                       '14612',
                                                       '17775',
                                                       '20963',
                                                       '23163',
                                                       '26486',
                                                       '28542',
                                                       '31616',
                                                       '35375',
                                                       '2948',
                                                       '4215',
                                                       '13528',
                                                       '2270',
                                                       '6498',
                                                       '11928',
                                                       '15817',
                                                       '18550',
                                                       '21333',
                                                       '23916',
                                                       '26763',
                                                       '29219',
                                                       '32509',
                                                       '36107',
                                                       '2644',
                                                       '11558',
                                                       '2281',
                                                       '6616',
                                                       '12191',
                                                       '16039',
                                                       '18814',
                                                       '21376',
                                                       '23941',
                                                       '26958',
                                                       '29543',
                                                       '33453',
                                                       '36333',
                                                       '18687',
                                                       '13403',
                                                       '2442',
                                                       '7100',
                                                       '12336',
                                                       '16078',
                                                       '18866',
                                                       '21501',
                                                       '24431',
                                                       '27233',
                                                       '29545',
                                                       '33537',
                                                       '36389',
                                                       '539',
                                                       '590',
                                                       '2755',
                                                       '7166',
                                                       '12481',
                                                       '16709',
                                                       '18996',
                                                       '21508',
                                                       '24718',
                                                       '27270',
                                                       '29913',
                                                       '33671',
                                                       '36511',
                                                       '12721',
                                                       '21518',
                                                       '2904',
                                                       '7195',
                                                       '12488',
                                                       '16726',
                                                       '19222',
                                                       '21796',
                                                       '25057',
                                                       '27334',
                                                       '30055',
                                                       '33768',
                                                       '36542',
                                                       '1819',
                                                       '5066',
                                                       '3466',
                                                       '7675',
                                                       '13478',
                                                       '16765',
                                                       '19253',
                                                       '22191',
                                                       '25217',
                                                       '27655',
                                                       '30596',
                                                       '34016',
                                                       '36834',
                                                       '1511',
                                                       '9864',
                                                       '3801',
                                                       '8289',
                                                       '13768',
                                                       '17063',
                                                       '19812',
                                                       '22423',
                                                       '25575',
                                                       '27721',
                                                       '30834',
                                                       '34244',
                                                       '37380',
                                                       '3735',
                                                       '14517',
                                                       '4454',
                                                       '8903',
                                                       '13801',
                                                       '17120',
                                                       '20028',
                                                       '22671',
                                                       '25677',
                                                       '27855',
                                                       '30886',
                                                       '34942',
                                                       '37480',
                                                       '6855',
                                                       '9259',
                                                       '3338',
                                                       '7292',
                                                       '12501',
                                                       '16753',
                                                       '19224',
                                                       '21805',
                                                       '25109',
                                                       '27518',
                                                       '30442',
                                                       '33804',
                                                       '36807',
                                                       '2855',
                                                       '609',
                                                       '3620',
                                                       '7712',
                                                       '13565',
                                                       '16929',
                                                       '19422',
                                                       '22192',
                                                       '25477',
                                                       '27719',
                                                       '30712',
                                                       '34046',
                                                       '37268',
                                                       '29150',
                                                       '14038',
                                                       '4220',
                                                       '8549',
                                                       '13774',
                                                       '17079',
                                                       '19981',
                                                       '22610',
                                                       '25606',
                                                       '27803',
                                                       '30872',
                                                       '34628',
                                                       '37471',
                                                       '35003',
                                                       '5326',
                                                       '4860',
                                                       '9015',
                                                       '13833',
                                                       '17174',
                                                       '20321',
                                                       '22798',
                                                       '26029',
                                                       '28139',
                                                       '31225',
                                                       '35032',
                                                       '37658',
                                                       '6797',
                                                       '24555'))
sub_graph<- neighbors(gitHubNetwork_mix_graph,'31890')

str(sub_graph)
l <- list();
i <- 1
while(i<1265) {
  l[[i]] <- sub_graph[i]
  i <- i + 1
}

l[1264]

str(l)
compg.edges <- as.data.frame(get.edgelist(l))

compg.df <- as.data.frame(list(Vertex=V(sub_graph)))

as.table(sub_graph)

V(sub_net)$color="yellow"
V(sub_net)$shape<-"circle"
#Makes a distinct color for the nodes
V(sub_net)[c("14954")]$color="red"
V(sub_net)[c("14954")]$shape<-"square"

plot(sub_net)



subNetwork(nodeList, network, neighbors=c("none", "first"))

node.list <- c("27803", "31890", "23589","33671","22321")

graph2 <- subNetwork(nodeList=node.list, network=gitHubNetwork_mix_graph)


View(sub_graph)

t<- neighbors(gitHubNetwork_mix_graph, v=c('27803'))

sub_net1<-induced.subgraph(gitHubNetwork_mix_graph, v=c('27803'))

sub_net1<-induced.subgraph(gitHubNetwork_mix_graph, vids =  (t))

sub_net1
        
V(sub_net1)[c('27803')]$color = "red"

degree(gitHubNetwork_mix_graph)

#community detection

eb <- edge.betweenness.community(gitHubNetwork_mix_graph)
plot(eb, g)

mixgraph_new<- gitHubNetwork_mix_graph

V(mixgraph_new)$name <- as_ids(V(mixgraph_new))

# make a list of the names of the nodes of interest
nodes_of_interest <- c("94",
                       "1017",
                       "1631",
                       "1882",
                       "2495",
                       "2614",
                       "3088",
                       "3145",
                       "3297",
                       "3418",
                       "3508",
                       "3510",
                       "3796",
                       "4280",
                       "4974",
                       "5084",
                       "5507",
                       "5661",
                       "6011",
                       "6276"
                              )

# select the nodes having these names
selnodes <- V(mixgraph_new)[name %in% nodes_of_interest]
# get their network neighborhood 
selegoV <- ego(mixgraph_new, order=1, nodes = selnodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(mixgraph_new,unlist(selegoV))

# plot the subgraph
plot(selegoG)

l <- layout_with_kk(selegoG)
plot(selegoG, layout=l)

plot.igraph(selegoG,vertex.size=10,vertex.label=NA,vertex.color="red",edge.color="black",
            layout=layout.fruchterman.reingold(selegoG, niter=10000, area=150*vcount(selegoG)^2))

lp <- fastgreedy.community(selegoG)
plot(lp, selegoG, vertex.lablel = NA)


lp <- label.propagation.community(selegoG)
plot(lp, selegoG, vertex.label = NA)


#Nodes with degree 5
nodes_of_interest <- c("78",
                       "627",
                       "752",
                       "814",
                       "1245",
                       "1298",
                       "1402",
                       "1524",
                       "1618",
                       "1767",
                       "2008",
                       "2105",
                       "2404",
                       "2424",
                       "2439",
                       "2443",
                       "2468",
                       "2489",
                       "2550",
                       "2816"
                       
)

# select the nodes having these names
selnodes <- V(mixgraph_new)[name %in% nodes_of_interest]
# get their network neighborhood 
selegoV <- ego(mixgraph_new, order=1, nodes = selnodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(mixgraph_new,unlist(selegoV))

# plot the subgraph
plot(selegoG)

l <- layout_with_kk(selegoG)
plot(selegoG, layout=l)

plot.igraph(selegoG,vertex.size=10,vertex.label=NA,vertex.color="magenta",edge.color="black",
            layout=layout.fruchterman.reingold(selegoG, niter=10000, area=150*vcount(selegoG)^2))

lp <- fastgreedy.community(selegoG)
plot(lp, selegoG, vertex.label = NA)


lp <- label.propagation.community(selegoG)
plot(lp, selegoG, vertex.label = NA)  

lp <- walktrap.community(selegoG)
plot(lp, selegoG, vertex.label =NA)



#Nodes with degree 50
nodes_of_interest <- c("5419",
                       "7211",
                       "10060",
                       "17050",
                       "23051",
                       "24161",
                       "29421",
                       "36208"
)

# select the nodes having these names
selnodes <- V(mixgraph_new)[name %in% nodes_of_interest]
# get their network neighborhood 
selegoV <- ego(mixgraph_new, order=1, nodes = selnodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(mixgraph_new,unlist(selegoV))

# plot the subgraph
plot(selegoG)

l <- layout_with_kk(selegoG)
plot(selegoG, layout=l)

plot.igraph(selegoG,vertex.size=10,vertex.label=NA,vertex.color="green",edge.color="black",
            layout=layout.fruchterman.reingold(selegoG, niter=10000, area=150*vcount(selegoG)^2))

lp <- fastgreedy.community(selegoG)
plot(lp, selegoG, vertex.label = NA)


lp <- label.propagation.community(selegoG)
plot(lp, selegoG, vertex.label = NA)  

lp <- walktrap.community(selegoG)
plot(lp, selegoG, vertex.label =NA)

