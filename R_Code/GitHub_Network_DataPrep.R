# Installing libraries
install.packages("igraph")

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

str(RoleFlag)
str(gitHubEdges)

RoleFlag$Role <- ifelse(RoleFlag$ml_target == 1, "ml", "dev" )

RoleFlag$ml_target<-NULL
RoleFlag$name<-NULL


ds_ml<- RoleFlag[RoleFlag$ml_target==1,]
ds_dev<- RoleFlag[RoleFlag$ml_target==0,]

ds_ml$role<-"ml"
ds_ml$ml_target<-NULL
ds_dev$role<-"dev"
ds_dev$ml_target<-NULL

github_roles <- gitHubEdges

colnames(RoleFlag)[1] <- "id_1"

edge_role<-merge(x=github_roles,y=RoleFlag,by="id_1",all.x=TRUE)

colnames(RoleFlag)[1] <- "id_2"
colnames(RoleFlag)[2] <- "Role2"

edge_role_whole<-merge(x=edge_role,y=RoleFlag,by="id_2",all.x=TRUE)

str(edge_role_whole)

edge_role_whole$network <- ifelse(edge_role_whole$Role == "dev" & edge_role_whole$Role2 == "dev" , "developers", ifelse(edge_role_whole$Role == "ml" & edge_role_whole$Role2 == "ml"  , "machine_learning", "mixed" ))

developer_nw <- edge_role_whole[edge_role_whole$network=="developers",]

write.csv(developer_nw,"developers.csv")

machine_learning_nw <- edge_role_whole[edge_role_whole$network=="machine_learning",]

write.csv(machine_learning_nw,"ml_engineers.csv")

mixed_nw <- edge_role_whole[edge_role_whole$network=="mixed",]

write.csv(mixed_nw,"mixed_nw.csv")





nodeFrame <- RoleFlag[1]
EdgeFrame <- gitHubEdges