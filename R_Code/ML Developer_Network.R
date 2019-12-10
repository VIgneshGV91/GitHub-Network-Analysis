# Installing libraries
install.packages("igraph")

# clear everything out of memory
rm(list=ls())

# Loading libraries
library(igraph)



# Setting work directory
dir_path <- "C:/Vignesh/Studies/Fall2019/564 Social Media Analytics/project"
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
gitHubNetwork<-graph.data.frame(EdgeFrame, directed = FALSE, vertices= nodeFrame)

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


# Analysing Machine Learning network
GitHub_ML_Network <- machine_learning_nw
GitHub_ML_Network$Role <- NULL
GitHub_ML_Network$Role2 <- NULL
GitHub_ML_Network$network <- NULL
View(GitHub_ML_Network)
write.csv(GitHub_ML_Network,"GitHub_ML_Network.csv")
GitHub_ML_Edges <- GitHub_ML_Network
Github_ML_Nodes <- read.csv(file="GitHub_ML_Network_Nodes.csv", header=TRUE, sep=",")
gitHubNetwork_ML_graph<-graph.data.frame(GitHub_ML_Edges, directed = FALSE, vertices= Github_ML_Nodes)
No_of_Edges <- ecount(gitHubNetwork_ML_graph)
#19684
No_of_vertices <- vcount(gitHubNetwork_ML_graph)
#7431
is.simple(gitHubNetwork_ML_graph)
# Is a simple Graph
is.connected(gitHubNetwork_ML_graph)
# It is not a connected graph


# total Degree of a network
degree_gitHubNetwork_ML <- degree(gitHubNetwork_ML_graph ,mode="total")
View(degree_gitHubNetwork_ML)
t<-table(degree_gitHubNetwork_ML)
View(t)
mean(degree_gitHubNetwork_ML)
hist(degree_gitHubNetwork_ML, breaks=50 , xlim=c(0,50), ylim=c(0,8000),col="darkmagenta")
sort(degree_gitHubNetwork_ML)
# Top five degree of centrality
#nodes   Degree
#14954	 482
#37289	 305
#16631	 276
#29023	 211
#20528	 206

# There is No in and out degree because it is a directed network

# Strength
gitHubNetwork_ML_strength <- strength(gitHubNetwork_ML_graph)
View(gitHubNetwork_ML_strength)
table(gitHubNetwork_ML_strength)
mean(gitHubNetwork_ML_strength)
#5.297806
median(gitHubNetwork_ML_strength)
#2
#nodes  Strength
#14954	482
#37289	305
#16631	276
#29023	211
#20528	206

# closeness
gitHubNetwork_ML_graph_closeness <- closeness(gitHubNetwork_ML_graph, normalized=TRUE)
View(gitHubNetwork_ML_graph_closeness)
table(gitHubNetwork_ML_graph_closeness)
mean(gitHubNetwork_ML_graph_closeness)  
#0.00271144
median(gitHubNetwork_ML_graph_closeness) 
#0.002838528
max(gitHubNetwork_ML_graph_closeness)
#0.002851155
min(gitHubNetwork_ML_graph_closeness)
#0.0001345895
# Top Five Closeness Measure for nodes
#Nodes  Closeness
#29023	0.002851155
#14954	0.002850695
#20193	0.002850448
#6149	  0.002850378
#37289	0.002850191
# 0.03830238 Mean
# 0.03839923 Median
# We see that mean and median are close so all the nodes have equal closeness centrality
# We see that the values are less 

# Betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
# Node betweenness
gitHubNetwork_ML_Betweeness <- round(betweenness(gitHubNetwork_ML_graph, v=V(gitHubNetwork_ML_graph), directed = FALSE, nobigint =TRUE, normalized = FALSE))
View(gitHubNetwork_ML_Betweeness)
max(gitHubNetwork_ML_Betweeness) # 3212693
median(gitHubNetwork_ML_Betweeness)  # 465
mean(gitHubNetwork_ML_Betweeness) # 11885.84
min(gitHubNetwork_ML_Betweeness) # 0
#Nodes  Betweenness
#14954	3212693
#16631	1824127
#37289	1817510
#29023	1737195
#20193	1317030

# Edge betwenness
gitHubNetwork_ML_Edgebetweens<-edge.betweenness(gitHubNetwork_ML_graph, e=E(gitHubNetwork_ML_graph), directed = FALSE)
View(gitHubNetwork_ML_Edgebetweens)
max(gitHubNetwork_ML_Edgebetweens)  
#174385.5
median(gitHubNetwork_ML_Edgebetweens) 
#4054.558
mean(gitHubNetwork_ML_Edgebetweens) 
#5761.27
min(gitHubNetwork_ML_Edgebetweens)
#1

#Nodes  Edge betwenness
#8996	  174385.52
#6211	  144747.39
#13329	141898.50
#8964	  118640.41
#6998	  114716.17

# Local clustering coefficients
gitHubNetwork_ML_clustering <- transitivity(gitHubNetwork_ML_graph, type="local", vids=V(gitHubNetwork_ML_graph)) 
View(gitHubNetwork_ML_clustering)
table(gitHubNetwork_ML_clustering)
max(gitHubNetwork_ML_clustering) 
median(gitHubNetwork_ML_clustering) 
mean(gitHubNetwork_ML_clustering) 
min(gitHubNetwork_ML_clustering)

# Embeddedness/ inverse of structural hole access (see Burt 2004)
gitHubNetwork_ML_Embeddness <- round(constraint(gitHubNetwork_ML_graph, nodes=V(gitHubNetwork_ML_graph)), digits=4)
View(gitHubNetwork_ML_Embeddness)
max(gitHubNetwork_ML_Embeddness) 
#1.125
median(gitHubNetwork_ML_Embeddness) 
#0.5
mean(gitHubNetwork_ML_Embeddness) 
#0.5375045
min(gitHubNetwork_ML_Embeddness)
#0.0045

#Nodes  Embeddedness
#8851	  1.1250
#20342	1.1250
#27533	1.1250
#34147	1.1250
#2651	  1.1250

#Nodes  Structural Holes
#16631	0.0045
#26110	0.0058
#20528	0.0059
#14954	0.0060
#29023	0.0069

#reciprocity
gitHubNetwork_ML_reciprocity <- reciprocity(gitHubNetwork_ML_graph)
#1

# Transitivity
gitHubNetwork_ML_transitivity <-transitivity(gitHubNetwork_ML_graph)
#0.03381751

# Avg. path length 
gitHubNetwork_ML_avg_path_length <-average.path.length(gitHubNetwork_ML_graph, directed=FALSE)
#4.52152

# Diameter
gitHubNetwork_ML_diameter<- diameter(gitHubNetwork_ML_graph)
#13

# Summarize the graph structure
summary(gitHubNetwork_ML_graph)

t<- neighbors(gitHubNetwork_ML_graph, v=c('14954'))
as.table(t)
t

t[276]

a<-t[276]
a

l <- list();
i <- 1
while(i<277) {
    l[[i]] <- t[i]
    i <- i + 1
  }

l[[277]]<-16631
View(l)
l
class(l)

t

g2 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=c('14954',
                                                            '239',
                                                            '4195',
                                                            '7795',
                                                            '10892',
                                                            '13328',
                                                            '15464',
                                                            '17137',
                                                            '18724',
                                                            '19779',
                                                            '21522',
                                                            '23817',
                                                            '25678',
                                                            '26988',
                                                            '28683',
                                                            '30481',
                                                            '32406',
                                                            '34113',
                                                            '35910',
                                                            '37394',
                                                            '31832',
                                                            '205',
                                                            '12004',
                                                            '1334',
                                                            '506',
                                                            '4202',
                                                            '7830',
                                                            '11013',
                                                            '13451',
                                                            '15546',
                                                            '17138',
                                                            '18730',
                                                            '19847',
                                                            '21679',
                                                            '23887',
                                                            '25746',
                                                            '27015',
                                                            '28693',
                                                            '30510',
                                                            '32518',
                                                            '34124',
                                                            '36045',
                                                            '37461',
                                                            '3256',
                                                            '13008',
                                                            '13461',
                                                            '1520',
                                                            '560',
                                                            '4307',
                                                            '7867',
                                                            '11050',
                                                            '13507',
                                                            '15739',
                                                            '17185',
                                                            '18744',
                                                            '19905',
                                                            '21711',
                                                            '23950',
                                                            '25885',
                                                            '27064',
                                                            '28887',
                                                            '30536',
                                                            '32558',
                                                            '34187',
                                                            '36091',
                                                            '37518',
                                                            '138',
                                                            '31794',
                                                            '2768',
                                                            '10569',
                                                            '953',
                                                            '4325',
                                                            '8061',
                                                            '11068',
                                                            '13602',
                                                            '15792',
                                                            '17330',
                                                            '18785',
                                                            '20193',
                                                            '21720',
                                                            '24092',
                                                            '25962',
                                                            '27188',
                                                            '28944',
                                                            '30552',
                                                            '32577',
                                                            '34189',
                                                            '36123',
                                                            '37552',
                                                            '1089',
                                                            '13286',
                                                            '155',
                                                            '10140',
                                                            '1418',
                                                            '4972',
                                                            '8093',
                                                            '11442',
                                                            '13664',
                                                            '15821',
                                                            '17357',
                                                            '18941',
                                                            '20249',
                                                            '22187',
                                                            '24179',
                                                            '25966',
                                                            '27246',
                                                            '29023',
                                                            '30624',
                                                            '32611',
                                                            '34301',
                                                            '36140',
                                                            '9101',
                                                            '9854',
                                                            '11061',
                                                            '37197',
                                                            '13255',
                                                            '1517',
                                                            '5084',
                                                            '8142',
                                                            '11586',
                                                            '14141',
                                                            '15822',
                                                            '17416',
                                                            '18973',
                                                            '20273',
                                                            '22280',
                                                            '24205',
                                                            '26099',
                                                            '27317',
                                                            '29092',
                                                            '31097',
                                                            '32657',
                                                            '34669',
                                                            '36168',
                                                            '3435',
                                                            '1804',
                                                            '2106',
                                                            '12718',
                                                            '12973',
                                                            '1747',
                                                            '5099',
                                                            '8210',
                                                            '11771',
                                                            '14460',
                                                            '15843',
                                                            '17581',
                                                            '19036',
                                                            '20275',
                                                            '22411',
                                                            '24225',
                                                            '26113',
                                                            '27489',
                                                            '29123',
                                                            '31132',
                                                            '32785',
                                                            '34744',
                                                            '36169',
                                                            '24258',
                                                            '32606',
                                                            '9091',
                                                            '6143',
                                                            '3636',
                                                            '1942',
                                                            '5288',
                                                            '8329',
                                                            '11782',
                                                            '14583',
                                                            '15857',
                                                            '17677',
                                                            '19067',
                                                            '20368',
                                                            '22418',
                                                            '24293',
                                                            '26171',
                                                            '27665',
                                                            '29137',
                                                            '31236',
                                                            '32804',
                                                            '34779',
                                                            '36233',
                                                            '14882',
                                                            '806',
                                                            '10462',
                                                            '10809',
                                                            '2814',
                                                            '2075',
                                                            '6028',
                                                            '8569',
                                                            '11883',
                                                            '14613',
                                                            '16050',
                                                            '17745',
                                                            '19095',
                                                            '20523',
                                                            '22509',
                                                            '24349',
                                                            '26212',
                                                            '27668',
                                                            '29169',
                                                            '31263',
                                                            '32856',
                                                            '34804',
                                                            '36398',
                                                            '521',
                                                            '4365',
                                                            '1833',
                                                            '14535',
                                                            '14471',
                                                            '2330',
                                                            '6149',
                                                            '8768',
                                                            '12070',
                                                            '14651',
                                                            '16079',
                                                            '17895',
                                                            '19120',
                                                            '20556',
                                                            '22616',
                                                            '24372',
                                                            '26308',
                                                            '27941',
                                                            '29208',
                                                            '31354',
                                                            '32937',
                                                            '34867',
                                                            '36491',
                                                            '218',
                                                            '665',
                                                            '2570',
                                                            '7012',
                                                            '4660',
                                                            '2747',
                                                            '6276',
                                                            '8783',
                                                            '12081',
                                                            '14661',
                                                            '16156',
                                                            '17915',
                                                            '19214',
                                                            '20601',
                                                            '22821',
                                                            '24585',
                                                            '26341',
                                                            '27994',
                                                            '29225',
                                                            '31377',
                                                            '33086',
                                                            '35106',
                                                            '36513',
                                                            '36987',
                                                            '25354',
                                                            '331',
                                                            '37304',
                                                            '6839',
                                                            '2935',
                                                            '6400',
                                                            '8979',
                                                            '12107',
                                                            '14664',
                                                            '16334',
                                                            '17916',
                                                            '19282',
                                                            '20611',
                                                            '22913',
                                                            '24630',
                                                            '26356',
                                                            '28157',
                                                            '29584',
                                                            '31450',
                                                            '33115',
                                                            '35131',
                                                            '36803',
                                                            '891',
                                                            '2082',
                                                            '6640',
                                                            '5795',
                                                            '4175',
                                                            '3030',
                                                            '6608',
                                                            '9328',
                                                            '12140',
                                                            '14897',
                                                            '16446',
                                                            '18003',
                                                            '19303',
                                                            '20657',
                                                            '22954',
                                                            '24658',
                                                            '26387',
                                                            '28201',
                                                            '29670',
                                                            '31619',
                                                            '33285',
                                                            '35300',
                                                            '36867',
                                                            '1258',
                                                            '6836',
                                                            '3025',
                                                            '13277',
                                                            '4003',
                                                            '3044',
                                                            '6626',
                                                            '9455',
                                                            '12547',
                                                            '14915',
                                                            '16614',
                                                            '18066',
                                                            '19332',
                                                            '20701',
                                                            '23151',
                                                            '24677',
                                                            '26402',
                                                            '28371',
                                                            '29698',
                                                            '31663',
                                                            '33332',
                                                            '35339',
                                                            '36883',
                                                            '601',
                                                            '8381',
                                                            '8644',
                                                            '9104',
                                                            '876',
                                                            '3218',
                                                            '6669',
                                                            '9725',
                                                            '12549',
                                                            '14985',
                                                            '16651',
                                                            '18102',
                                                            '19334',
                                                            '20861',
                                                            '23304',
                                                            '24740',
                                                            '26603',
                                                            '28415',
                                                            '29702',
                                                            '31796',
                                                            '33395',
                                                            '35367',
                                                            '36936',
                                                            '4134',
                                                            '11737',
                                                            '126',
                                                            '12938',
                                                            '14563',
                                                            '4036',
                                                            '7625',
                                                            '10401',
                                                            '13275',
                                                            '15315',
                                                            '17099',
                                                            '18693',
                                                            '19725',
                                                            '21258',
                                                            '23559',
                                                            '25281',
                                                            '26912',
                                                            '28601',
                                                            '30391',
                                                            '32332',
                                                            '34068',
                                                            '35739',
                                                            '37276',
                                                            '1154',
                                                            '11464',
                                                            '6458',
                                                            '11999',
                                                            '6518',
                                                            '4069',
                                                            '7775',
                                                            '10703',
                                                            '13279',
                                                            '15454',
                                                            '17114',
                                                            '18713',
                                                            '19729',
                                                            '21389',
                                                            '23619',
                                                            '25491',
                                                            '26971',
                                                            '28630',
                                                            '30396',
                                                            '32379',
                                                            '34080',
                                                            '35798',
                                                            '37289',
                                                            '3892',
                                                            '4339',
                                                            '696',
                                                            '3924',
                                                            '3542',
                                                            '6946',
                                                            '9769',
                                                            '12836',
                                                            '15001',
                                                            '16684',
                                                            '18143',
                                                            '19417',
                                                            '21033',
                                                            '23397',
                                                            '24751',
                                                            '26662',
                                                            '28416',
                                                            '29710',
                                                            '31911',
                                                            '33486',
                                                            '35416',
                                                            '36959',
                                                            '710',
                                                            '26379',
                                                            '14861',
                                                            '6072',
                                                            '5036',
                                                            '3757',
                                                            '7145',
                                                            '9888',
                                                            '12857',
                                                            '15171',
                                                            '16999',
                                                            '18149',
                                                            '19482',
                                                            '21082',
                                                            '23424',
                                                            '24891',
                                                            '26760',
                                                            '28537',
                                                            '29874',
                                                            '31925',
                                                            '33502',
                                                            '35417',
                                                            '37026',
                                                            '789',
                                                            '9895',
                                                            '28172',
                                                            '36369',
                                                            '33998',
                                                            '3858',
                                                            '7206',
                                                            '9980',
                                                            '12978',
                                                            '15172',
                                                            '17080',
                                                            '18485',
                                                            '19545',
                                                            '21167',
                                                            '23479',
                                                            '24897',
                                                            '26776',
                                                            '28554',
                                                            '30018',
                                                            '32043',
                                                            '33700',
                                                            '35471',
                                                            '37068',
                                                            '173',
                                                            '9066',
                                                            '2585',
                                                            '6809',
                                                            '37533',
                                                            '3917',
                                                            '7482',
                                                            '10234',
                                                            '13124',
                                                            '15229',
                                                            '17087',
                                                            '18608',
                                                            '19652',
                                                            '21243',
                                                            '23505',
                                                            '25142',
                                                            '26834',
                                                            '28568',
                                                            '30273',
                                                            '32216',
                                                            '33709',
                                                            '35694',
                                                            '37134',
                                                            '10412',
                                                            '5030',
                                                            '8414',
                                                            '3173',
                                                            '3601'))




t<- neighbors(gitHubNetwork_ML_graph, v=c('14954'))
t

g2 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=(t))


plot(g2)
g2
plot(g2,main="Neighbors of 14954",vertex.label=V(g2)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)

#Makes a distinct color for the nodes
V(g2)[c("14954")]$color="green"


#Changes the shape of nodes '511','541'.'518','519' to square while keeping the rest of the nodes in circle.
V(g2)[c("14954")]$shape<-"square"

V(g2)$color="orange"
V(g2)$shape<-"circle"

library(readxl)
neighbors <- read_excel("neighbors.xlsx")
nw_degree_all <- read_excel("neighborsanddegree.xlsx")

neigh_degree<-merge(x=neighbors,y=nw_degree_all,by="id",all.x=TRUE)

#Using ego function to visualise highly important nodes in the network
ego.list <- make_ego_graph(gitHubNetwork_ML_graph, order=1, nodes=V(gitHubNetwork_ML_graph)['14954'])

desired_subset <- NULL
for (i in seq_along(ego.list)){
  x <- ego.list[[i]]
  desired_subset <- graph.union(desired_subset, x)
}
V(desired_subset)$color<-"yellow"
V(desired_subset)['14954']$color<-"blue"
plot(desired_subset,vertex.label=NA, main="Ego Network of Node-14954")

plot(desired_subset,main="Ego Network of Node-14954",vertex.label=V(desired_subset)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)


###################################################################
# Node betweenness
betweens_ML <- round(betweenness(gitHubNetwork_ML_graph, v=V(gitHubNetwork_ML_graph), directed = FALSE, nobigint =TRUE, normalized = FALSE))
View(betweens_ML)

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_ML <- round(constraint(gitHubNetwork_ML_graph, nodes=V(gitHubNetwork_ML_graph)), digits=4)
View(constraints_ML)

# Local clustering coefficients
clustering_ML <- transitivity(gitHubNetwork_ML_graph, type="local", vids=V(gitHubNetwork_ML_graph)) 
View(clustering_ML)

# Degree centrality
degree_ML <- degree(gitHubNetwork_ML_graph)

View(degree_ML)

node_frame<-data.frame(betweens_ML, constraints_ML, clustering_ML, degree_ML)
View(node_frame)
a_node<-aggregate(betweens_ML ~ clustering_ML, data=node_frame, mean)
View(a_node)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")
#Using the above definitons and combining them together, it can be understood that nodes which have less clustering values 
#have the highest betweenness because those people who are not tightly connected to any cluster serve as the structural hole 
#and act as the main node for information flow through them. Moreover the plot also proves this inverse relation
#between the average betweenness and the clustering coefficient values that nodes which have less clustering coefficients have higher 
#average betweenness and the relationship is linear with a negative slope.

dev.off()

# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_ML ~ degree_ML, data=node_frame, mean)
View(a_node)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
#Betweenness Centrality and Degree centrality measure almost the same thing - the importance of the node to its 
#neighbours, except degree measures th number of incoming links to itself whereas betweenness measures how important is the 
#node for information exchange in the network. A node may have very high betweenness, but that does not necessarily mean that 
#it should have a high degree and vice versa, but in genera; if we aggregate the values for mean, we can find 
#from the plot that there is a direct relationship with the linear line with positive slope.
#The top 5 betweenness values and their degrees are given as:
#Deg  Betweenness
#34	  29145.0
#454	24991.0
#49	  11846.0
#29	  11167.3
#40	  9143.0

a_node<-aggregate(clustering_ML ~ degree_ML, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
#Clustering coefficient is a measure to denote how tightly knotted the nodes are in their cluster 
#whereas degree denotes how many neighbors both internal and external nodes of the cluster the node is in are connected to it.
#So any cluster of people sharing similar ideas in the same domain wilh have high clustering coefficient, whereas less degree.
#This inference is evident from the plot that with increasing degree centrality, the average clustering coefficient value 
#decreases showing an inverse relationship with a linear line with a negative slope.
#The more knowledgable people are usually located outside those clusters and connected to nodes from different clusters.

a_node<-aggregate(constraints_ML ~ degree_ML, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")
#Embeddedness denotes absense of new ideologies and high overlap of thoughts and ideas of nodes in a cluster
#So a node having high degree will definitely have less embeddedness. This is evident in the plot that with 
#increasing degree centrality, the Embeddedness coefficient of the nodes decreases showing a negative relationship
#with the regression line having a negative slope.

# Log-log degree distribution
par(mfrow=c(1, 2))
d.net <-degree(gitHubNetwork_ML_graph)
View(d.net)
#Total degrees of the SAP network
dd.net <- degree.distribution(gitHubNetwork_ML_graph)
View(dd.net)
#Total degree distribution of the SAP network
d <- 1:max(d.net)-1
View(d)
ind <- (dd.net != 0)
#Classifying a binary index with values >0 as True and values <0 as False
View(ind)
d[ind]
# picking the True Index Degree values
dd.net[ind]
#picking the True Index Degree Distribution values
ds_df<- data.frame(d[ind],dd.net[ind])
#The dataframe after sorting shows that for low values of dd, there are higher values of d and vice versa

plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")
#Hence there is an inverse relation between the degree & its distribution with a regression line with a negative slope.

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(gitHubNetwork_ML_graph,V(gitHubNetwork_ML_graph))$knn
View(a.nn.deg)
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))
#From the plot we can observe that there are huge values of Log vertex degree which are 
#within 1 to 15 and the low average neighbor values become very less for values of Log Vertex more than 20
#which again shows us an inverse relationship.

############################Running Community Detection Algorithms#########################################

##################Label Propagation #######################################################################

#Label Propogation Algorithm
ML_comm_lab_prop <- label.propagation.community(gitHubNetwork_ML_graph,weights=E(gitHubNetwork_ML_graph)$weight)
c.m_lab_prop <- membership(ML_comm_lab_prop)
c.m_lab_prop

t<-table(c.m_lab_prop)
View(t)

#Label Propagation Algorithm detects 251 Communities

#Grp Vertices
#1	     6325
#3	      230
#10	       53
#13	       27
#83	       22
#14        20
#25        18
plot(ML_comm_lab_prop, gitHubNetwork_ML_graph, vertex.label= NA,vertex.size=5, main="Label Propagation Algorithm")

comm_1 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==1]
ML_Lab_Prop_grp1 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_1)
V_grp1<-V(ML_Lab_Prop_grp1)$name
# Degree centrality #
degree_ML_grp1 <- degree(ML_Lab_Prop_grp1)
View(degree_ML_grp1)

#Group    Vertex(With highest Degree)  Degree
#    1    14954	                         474

comm_3 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==3]
ML_Lab_Prop_grp3 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_3)
V_grp3<-V(ML_Lab_Prop_grp3)$name
# Degree centrality #
degree_ML_grp3 <- degree(ML_Lab_Prop_grp3)
View(degree_ML_grp3)

#Group  Vertex(With highest Degree)  Degree
#    3  17128	                          51


comm_10 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==10]
ML_Lab_Prop_grp10 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_10)
V_grp10<-V(ML_Lab_Prop_grp10)$name
# Degree centrality
degree_ML_grp10 <- degree(ML_Lab_Prop_grp10)
View(degree_ML_grp10)

comm_13 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==13]
ML_Lab_Prop_grp13 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_13)
V_grp13<-V(ML_Lab_Prop_grp13)$name
# Degree centrality
degree_ML_grp13 <- degree(ML_Lab_Prop_grp13)
View(degree_ML_grp13)

comm_83 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==83]
ML_Lab_Prop_grp83 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_83)
V_grp83<-V(ML_Lab_Prop_grp83)$name
degree_ML_grp83 <- degree(ML_Lab_Prop_grp83)
View(degree_ML_grp83)

comm_14 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==14]
ML_Lab_Prop_grp14 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_14)
V_grp14<-V(ML_Lab_Prop_grp14)$name
degree_ML_grp14 <- degree(ML_Lab_Prop_grp14)
View(degree_ML_grp14)


comm_25 <- V(gitHubNetwork_ML_graph)[c.m_lab_prop==25]
ML_Lab_Prop_grp25 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_25)
V_grp25<-V(ML_Lab_Prop_grp25)$name
degree_ML_grp25 <- degree(ML_Lab_Prop_grp25)
View(degree_ML_grp25)

combined<- V(gitHubNetwork_ML_graph)[c.m_lab_prop==10 |c.m_lab_prop==13 |c.m_lab_prop==83 |c.m_lab_prop==14 | c.m_lab_prop==25]

  
ML_Fast_combined <- induced.subgraph(graph=gitHubNetwork_ML_graph,
                                      vids=combined)

V(ML_Lab_Prop_combined)$shape<-"circle"

V(ML_Lab_Prop_combined)[V_grp10]$color="green"
V(ML_Lab_Prop_combined)["26918"]$color="red"


V(ML_Lab_Prop_combined)[V_grp13]$color="darkred"
V(ML_Lab_Prop_combined)["28429"]$color="yellow"

V(ML_Lab_Prop_combined)[V_grp83]$color="blue"
V(ML_Lab_Prop_combined)["18663"]$color="red"

V(ML_Lab_Prop_combined)[V_grp14]$color="orange"
V(ML_Lab_Prop_combined)["6875"]$color="red"

V(ML_Lab_Prop_combined)[V_grp25]$color="yellow"
V(ML_Lab_Prop_combined)["35403"]$color="red"

plot(ML_Lab_Prop_combined,main="3rd - 7th Biggest Communities (Label Propagation)",vertex.label=V(ML_Lab_Prop_combined)$name, vertex.label.dist=1.5,vertex.size=6,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)


##############################Fast Greedy Algorithm#########################################

ML_comm_fast <- fastgreedy.community(gitHubNetwork_ML_graph, weights=E(gitHubNetwork_ML_graph)$weight)
ML_fastgreedy <- membership(ML_comm_fast)

# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
t<-table(ML_fastgreedy, useNA = c("no"))

View(t)
#Group Vertices
#1	    1537
#5	    1487
#3	    1092
#2	    677
#4	    402

#Groups used for analysis
#Group Vertices
#14	151
#8	140
#6	110
#9	 92
#7	 89

#FastGreedy Algorithm Detects - 305 Communities


plot(ML_fastgreedy, gitHubNetwork_ML_graph, vertex.label= NA,vertex.size=5, main="Fast Greedy Algorithm")


comm_1 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==1]
ML_Lab_Prop_grp1 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_1)
V_grp1<-V(ML_Lab_Prop_grp1)$name
# Degree centrality 
degree_ML_grp1 <- degree(ML_Lab_Prop_grp1)
View(degree_ML_grp1)
#Group    Vertex(With highest Degree)  Degree
#    1    14954	                         474

comm_5 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==5]
ML_Lab_Prop_grp5 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_5)
V_grp5<-V(ML_Lab_Prop_grp5)$name
# Degree centrality
degree_ML_grp5 <- degree(ML_Lab_Prop_grp5)
View(degree_ML_grp5)
#Group    Vertex(With highest Degree)  Degree
#    5    29023		                       119

comm_3 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==3]
ML_Lab_Prop_grp3 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_3)
V_grp3<-V(ML_Lab_Prop_grp3)$name
# Degree centrality
degree_ML_grp3 <- degree(ML_Lab_Prop_grp3)
View(degree_ML_grp3)
#Group    Vertex(With highest Degree)  Degree
#    3    16631			                      95

comm_2 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==2]
ML_Lab_Prop_grp2 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_2)
V_grp2<-V(ML_Lab_Prop_grp2)$name
# Degree centrality
degree_ML_grp2 <- degree(ML_Lab_Prop_grp2)
View(degree_ML_grp2)
#Group    Vertex(With highest Degree)  Degree
#    2    17850				                     35


comm_4 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==4]
ML_Lab_Prop_grp4 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_4)
V_grp4<-V(ML_Lab_Prop_grp4)$name
# Degree centrality
degree_ML_grp4 <- degree(ML_Lab_Prop_grp4)
View(degree_ML_grp4)
#Group    Vertex(With highest Degree)  Degree
#    4    23589					                  76




comm_14 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==14]
ML_Lab_Prop_grp14 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_14)
V_grp14<-V(ML_Lab_Prop_grp14)$name
# Degree centrality #910
degree_ML_grp14 <- degree(ML_Lab_Prop_grp14)
View(degree_ML_grp14)

comm_8 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==8]
ML_Lab_Prop_grp8 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_8)
V_grp8<-V(ML_Lab_Prop_grp8)$name
# Degree centrality #18298
degree_ML_grp8 <- degree(ML_Lab_Prop_grp8)
View(degree_ML_grp8)

comm_6 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==6]
ML_Lab_Prop_grp6 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_6)
V_grp6<-V(ML_Lab_Prop_grp6)$name
# Degree centrality #25692
degree_ML_grp6 <- degree(ML_Lab_Prop_grp6)
View(degree_ML_grp6)

comm_9 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==9]
ML_Lab_Prop_grp9 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_9)
V_grp9<-V(ML_Lab_Prop_grp9)$name
# Degree centrality #4184
degree_ML_grp9 <- degree(ML_Lab_Prop_grp9)
View(degree_ML_grp9)


comm_7 <- V(gitHubNetwork_ML_graph)[ML_fastgreedy==7]
ML_Lab_Prop_grp7 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=comm_7)
V_grp7<-V(ML_Lab_Prop_grp7)$name
# Degree centrality #30481
degree_ML_grp7 <- degree(ML_Lab_Prop_grp7)
View(degree_ML_grp7)

combined_fast<- V(gitHubNetwork_ML_graph)[ML_fastgreedy==14 |ML_fastgreedy==8 |ML_fastgreedy==6 |ML_fastgreedy==9 | ML_fastgreedy==7]


ML_Fast_combined <- induced.subgraph(graph=gitHubNetwork_ML_graph,
                                         vids=combined_fast)

V(ML_Fast_combined)$shape<-"circle"

V(ML_Fast_combined)[V_grp14]$color="green"
V(ML_Fast_combined)["910"]$color="red"


V(ML_Fast_combined)[V_grp8]$color="darkred"
V(ML_Fast_combined)["18298"]$color="yellow"

V(ML_Fast_combined)[V_grp6]$color="blue"
V(ML_Fast_combined)["25692"]$color="red"

V(ML_Fast_combined)[V_grp9]$color="orange"
V(ML_Fast_combined)["4184"]$color="red"

V(ML_Fast_combined)[V_grp7]$color="yellow"
V(ML_Fast_combined)["30481"]$color="red"

plot(ML_Fast_combined,main="7th - 11th Biggest Communities (Fast Greedy)",vertex.label=V(ML_Fast_combined)$name, vertex.label.dist=1.5,vertex.size=6,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)


##################################Walktrap Algorithm######################################
# Community detection using the Walktrap Algorithm
comm_walk <- walktrap.community(gitHubNetwork_ML_graph,weights=E(gitHubNetwork_ML_graph)$weight)
str(comm_walk)

comwalk<-comm_walk$membership
str(comwalk)
t<-table(comwalk)
View(t)

#IGRAPH clustering walktrap, groups: 1056, mod: 0.46

#Walktrap identifies 1056 Communities 

#comwalk Vertices
#9	      1705
#6	      1245
#20	      249
#28	      173
#30	      98

#Groups(5th to 9th) used for Analysis
#Grp Vertices
#30		98
#21		88
#40		77
#115	72
#13		66

wk_comm_9 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==9]
ML_walk_grp9 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_9)
V_grp9<-V(ML_walk_grp9)$name
# Degree centrality #3757
degree_ML_walk_grp9 <- degree(ML_walk_grp9)
View(degree_ML_walk_grp9)
#Group    Vertex(With highest Degree)  Degree
#    9    14954				                   335

wk_comm_6 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==6]
ML_walk_grp6 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_6)
V_grp6<-V(ML_walk_grp6)$name
# Degree centrality #3757
degree_ML_walk_grp6 <- degree(ML_walk_grp6)
View(degree_ML_walk_grp6)
#Group    Vertex(With highest Degree)  Degree
#    6    16631					                 99

wk_comm_20 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==20]
ML_walk_grp20 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_20)
V_grp20<-V(ML_walk_grp20)$name
# Degree centrality #3757
degree_ML_walk_grp20 <- degree(ML_walk_grp20)
View(degree_ML_walk_grp20)
#Group    Vertex(With highest Degree)  Degree
#    20    34603						              64

wk_comm_28 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==28]
ML_walk_grp28 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_28)
V_grp28<-V(ML_walk_grp28)$name
# Degree centrality #3757
degree_ML_walk_grp28 <- degree(ML_walk_grp28)
View(degree_ML_walk_grp28)
#Group    Vertex(With highest Degree)  Degree
#   28    17792							           55

wk_comm_30 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==30]
ML_walk_grp30 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_30)
V_grp30<-V(ML_walk_grp30)$name
# Degree centrality #3757
degree_ML_walk_grp30 <- degree(ML_walk_grp30)
View(degree_ML_walk_grp30)


wk_comm_21 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==21]
ML_walk_grp21 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_21)
V_grp21<-V(ML_walk_grp21)$name
# Degree centrality #26918
degree_ML_walk_grp21 <- degree(ML_walk_grp21)
View(degree_ML_walk_grp21)

wk_comm_40 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==40]
ML_walk_grp40 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_40)
V_grp40<-V(ML_walk_grp40)$name
# Degree centrality #23589
degree_ML_walk_grp40 <- degree(ML_walk_grp40)
View(degree_ML_walk_grp40)

wk_comm_115 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==115]
ML_walk_grp115 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_115)
V_grp115<-V(ML_walk_grp115)$name
# Degree centrality #32566
degree_ML_walk_grp115 <- degree(ML_walk_grp115)
View(degree_ML_walk_grp115)

wk_comm_13 <- V(gitHubNetwork_ML_graph)[comm_walk$membership==13]
ML_walk_grp13 <- induced.subgraph(graph=gitHubNetwork_ML_graph,vids=wk_comm_13)
V_grp13<-V(ML_walk_grp13)$name
# Degree centrality #2701
degree_ML_walk_grp13 <- degree(ML_walk_grp13)
View(degree_ML_walk_grp13)



combined_walk<- V(gitHubNetwork_ML_graph)[comm_walk$membership==30 |comm_walk$membership==21 |comm_walk$membership==40 |comm_walk$membership==115 | comm_walk$membership==13]


ML_walk_combined <- induced.subgraph(graph=gitHubNetwork_ML_graph,
                                     vids=combined_walk)

V(ML_walk_combined)$shape<-"circle"

V(ML_walk_combined)[V_grp30]$color="green"
V(ML_walk_combined)["3757"]$color="red"


V(ML_walk_combined)[V_grp21]$color="darkred"
V(ML_walk_combined)["26918"]$color="yellow"

V(ML_walk_combined)[V_grp40]$color="blue"
V(ML_walk_combined)["23589"]$color="red"

V(ML_walk_combined)[V_grp115]$color="orange"
V(ML_walk_combined)["32566"]$color="red"

V(ML_walk_combined)[V_grp13]$color="yellow"
V(ML_walk_combined)["2701"]$color="red"

plot(ML_walk_combined,main="5th to 9th Biggest Communities (Walktrap Algorithm)",vertex.label=V(ML_walk_combined)$name, vertex.label.dist=1.5,vertex.size=6,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)

#Group  Vertex (With highest degree)  Degree
#30      3757
#21     26918
#40     23589
#115    32566
#13      2701