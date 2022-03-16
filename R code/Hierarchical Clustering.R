#import libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(FactoMineR)
library(gplots)
library(hrbrthemes)
library(viridis)
library(ggdendro)
library(MASS)
library(NbClust)
###########################################################################################################################
#import dataset
data <- read.csv("data for analysis.csv")
data <- data[, -c(1,2,3,5,6)]

#starting dataset, to retrieve initial information about players
data_understanding_clusters <- read.csv("data for analysis.csv")
###########################################################################################################################
###########################################################################################################################
#scale variable
nums <- unlist(lapply(data, is.numeric))
colnames(data[,nums])
data[-c(1,7,24,45)]<- lapply(data[-c(1,7,24,45)], function(x) c(scale(x)))
###########################################################################################################################
#transform categorical variables in factors for daisy funnction
data$Position <- as.factor(as.character(data$Position))
data$CrdR <- as.factor(as.character(data$CrdR))
data$Fld <- as.factor(as.character(data$Fld))
data$PK <- as.factor(as.character(data$PK))
###########################################################################################################################
#dissimilarity matrix
gower_dist <- daisy(data, metric = "gower")


######create the hierarchical clustering
play_clust <- hclust(gower_dist, method= "complete")
play_clust_cut <- cutree(play_clust, k = 8)
plot(play_clust)

#plot hierarchical clustering
h_play_clust <- as.dendrogram(play_clust)
plot(cut(h_play_clust, h= 0.28)$upper, 
     main="Upper tree of cut at h=0.270")
abline(h = 0.275, lty = 2)

###########################################################################################################################
#function to create sub dataset based on cluster
cluster_analysis <- function(number){
  cluster <- data_understanding_clusters[which(play_clust_cut == number),]
  Cluster_obs <<- cluster
}

#create a vector to count def, mid, and striker in a cluster
position_count <- function(dataset){
  proportions <- c()
  prop1 <- sum(dataset == "D")
  prop2 <- sum(dataset == "M")
  prop3 <- sum(dataset == "ST")
  proportions <- append(proportions, prop1,0) 
  proportions <- append(proportions, prop2,2)
  proportions <- append(proportions, prop3,3)
  obs <- nrow(dataset)
  proportions <- proportions/obs
    
  f_props <<- proportions
}

#general analysis of the clusters created by the algorithm
differences <- function(cluster){
  extra_cluster <- anti_join(data_understanding_clusters, cluster) 
  print(paste0("The average age the cluster is ", round(mean(cluster$Age),2)))
  print(paste0('The average  age fot the dataset is', round(mean(extra_cluster$Age),2)))
  #
  print(paste0('The average goals per season is for the clusters is ', round(mean(cluster$goal.90),2)))
  print(paste0('The average goals per season is for the dataset is ', round(mean(extra_cluster$goal.90),2)))
  #
  print(paste0('The average salary per season is for the clusters is ',round(mean(cluster$Yearly.Salary),2)))
  print(paste0('The average salary per season is for the dataset is ',round(mean(extra_cluster$Yearly.Salary),2)))
  #
  print(paste0('The average match played per season is for the clusters is ', round(mean(cluster$X90s),2)))
  print(paste0('The average match played per season is for the dataset is ', round(mean(extra_cluster$X90s),2)))
  #
  print(paste0('The average number of dribbled players per season is for the clusters is ', round(mean(cluster$X.Pl),2)))
  print(paste0('The average number of dribbled players per season is for the dataset is ', round(mean(extra_cluster$X.Pl),2)))
  #
  print(paste0('The average number of cross per season is for the clusters is ', round(mean(cluster$Crs),2)))
  print(paste0('The average number og cross per season is for the dataset is ', round(mean(extra_cluster$Crs),2)))
  #
  entire <- position_count(data_understanding_clusters)
  print(paste0('The proportion on the entire of defender is ', round(f_props[1],2),' ,of midfielder is ', round(f_props[2],2),' and for striker is ', round(f_props[3],2)))
  cluster_data <- position_count(cluster)
  print(paste0('The proportion within the cluster of defender is ', round(f_props[1],2),' ,of midfielder is ', round(f_props[2],2),' and for striker is ', round(f_props[3],2)))
  #
  print(paste0('The average of tackle won per season is for the clusters is ', round(mean(cluster$TklW),2)))
  print(paste0('The average percentage of tackle won per season for the dataset is ', round(mean(extra_cluster$TklW),2)))
  #
  print(paste0('The average  goal creating actions for the entire season for the clusters is ', round(mean(cluster$GCA),2)))
  print(paste0('The average goal creating actions for the entire season for the dataset is ', round(mean(extra_cluster$GCA),2)))
  #
  print(paste0('The average  completed pass for the entire season for the clusters is ', round(mean(cluster$total.completed),2)))
  print(paste0('The average completed pass for the entire season for the dataset is ', round(mean(extra_cluster$total.completed),2)))
}

#clusters analysis
cluster_1 <-cluster_analysis(1)
characteristics_1 <- differences(cluster_1) #on the mean defender
#
cluster_2 <-cluster_analysis(2)
characteristics_2 <- differences(cluster_2) #low used offensive player
#
cluster_3 <-cluster_analysis(3)
characteristics_3 <- differences(cluster_3) #strong side backs
#
cluster_4 <- cluster_analysis(4)
characteristics_4 <- differences(cluster_4) #low used players, very low used offensive players
#
cluster_5 <-cluster_analysis(5) #cdc adn box-to-box midfielders
characteristics_5 <- differences(cluster_5)
#
cluster_6 <-cluster_analysis(6) #centre-forward
characteristics_6 <- differences(cluster_6)
#
cluster_7 <-cluster_analysis(7) #10
characteristics_7 <- differences(cluster_7)
#
cluster_8 <- cluster_analysis(8) # striker
characteristics_8 <- differences(cluster_8)

##########################################################################################################################
#create columns which contains the number of cluster of the player
data_understanding_clusters$grouping <- play_clust_cut
data_understanding_clusters$grouping <- as.factor(as.character(data_understanding_clusters$grouping))


#function to create dummy when a player belong to a particular cluster
create_subgroup <- function(dataset, number){
  for (i in 1:nrow(dataset)){
    if (dataset[i,51] == number){
      dataset[i,52] <- "cluster"
    } else if (dataset[i,51] != number){
      dataset[i,52] <- "dataset"}
  }
  data_for_plot <<- dataset
}

#function for density plot
plot_function <- function(dataset, variable, var_name){
  name <- ggplot(data= dataset, aes(x= variable, group = V52, fill = V52)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum() +
    labs(fill = "Group", x = var_name )
  output_plot <<- name
}


#########################################################################################################################
#Cluster 1
create_subgroup(data_understanding_clusters,1)

#number of player dribbled
plot_function(data_for_plot, data_for_plot$X.Pl, "Number of Player Dribbled")
plot_players_dribbled_c1 <- output_plot
plot_players_dribbled_c1

#match played
plot_function(data_for_plot, data_for_plot$X90s, "Match Played")
plot_X90s_c1 <- output_plot
plot_X90s_c1

#percentage of tackle won
plot_function(data_for_plot, data_for_plot$TklW, "Percentage of Tackle Won")
plot_TklW_c1 <- output_plot
plot_TklW_c1

#touches in defensive penalty area
plot_function(data_for_plot, data_for_plot$Def.Pen, "Touches in defensive penalty area")
plot_def_penarea_c1 <- output_plot
plot_def_penarea_c1

#number of Sh.90 per season
plot_function(data_for_plot, data_for_plot$Sh.90, "Shot per game")
plot_Sh.90_c1 <- output_plot
plot_Sh.90_c1

#plot for cluster compositition
p <- ggplot(data= data_for_plot, aes(x= Position,y= ..prop.., group = V52, fill= as.factor(V52) ))+
  geom_bar(position = position_dodge2(preserve = "single"),width =  0.5) +
  labs(fill = "Group", y = "Group composition") 
p

##########################################################################################################################
#Cluster 8: Strikers
data_for_plot<- NULL
create_subgroup(data_understanding_clusters, 8)
#number of player dribbled
plot_function(data_for_plot, data_for_plot$X.Pl, "Number of Player Dribbled")
plot_players_dribbled_c8 <- output_plot
plot_players_dribbled_c8

#match played
plot_function(data_for_plot, data_for_plot$X90s, "Match Played")
plot_X90s_c8 <- output_plot
plot_X90s_c8

#percentage of tackle won
plot_function(data_for_plot, data_for_plot$goal.90, "Goal per season")
plot_goal.90_c8 <- output_plot
plot_goal.90_c8

#touches in defensive penalty area
plot_function(data_for_plot, data_for_plot$GCA, "Goal creating actions")
plot_gca_c8 <- output_plot
plot_gca_c8

#number of Sh.90 per season
plot_function(data_for_plot, data_for_plot$Crs, "Crosses")
plot_Crs_c8 <- output_plot
plot_Crs_c8

#plot for cluster compositition
p <- ggplot(data= data_for_plot, aes(x= Position,y= ..prop.., group = V52, fill= as.factor(V52) ))+
  geom_bar(position = position_dodge2(preserve = "single"),width =  0.5) +
  labs(fill = "Group", y = "Group composition") 
p
##########################################################################################################################
#cluster 3: strong side backs
data_for_plot <- NULL
create_subgroup(data_understanding_clusters,3)
#match played
plot_function(data_for_plot, data_for_plot$X90s, "Match Played")
plot_X90s_c3 <- output_plot
plot_X90s_c3

#GCA
plot_function(data_for_plot, data_for_plot$Crs, "Crs")
plot_Crs_c3 <- output_plot
plot_Crs_c3

#number of player dribbled
plot_function(data_for_plot, data_for_plot$X.Pl, "Number of Player Dribbled")
plot_players_dribbled_c3 <- output_plot
plot_players_dribbled_c3

#
plot_function(data_for_plot, data_for_plot$GCA, "Goal creating actions")
plot_gca_c3 <- output_plot
plot_gca_c3

##########################################################################################################################
#cluster 5: defensive and box to box midfielder
create_subgroup(data_understanding_clusters,5)

#match played
plot_function(data_for_plot, data_for_plot$X90s, "Match Played")
plot_X90s_c5 <- output_plot
plot_X90s_c5

#tackle won during the season
plot_function(data_for_plot, data_for_plot$TklW, "Percentage of Tackle Won")
plot_tklW_c5 <- output_plot
plot_tklW_c5

#goal creating action
plot_function(data_for_plot, data_for_plot$GCA, "Goal creating actions")
plot_gca_c5 <- output_plot
plot_gca_c5

#proportion
p <- ggplot(data= data_for_plot, aes(x= Position,y= ..prop.., group = V52, fill= as.factor(V52) ))+
  geom_bar(position = position_dodge2(preserve = "single"),width =  0.5) +
  labs(fill = "Group", y = "Group composition") 
p
##########################################################################################################################
#cluster 6: centre-forward 
data_for_plot <- NULL
create_subgroup(data_understanding_clusters,6)

#proportion
p <- ggplot(data= data_for_plot, aes(x= Position,y= ..prop.., group = V52, fill= as.factor(V52) ))+
  geom_bar(position = position_dodge2(preserve = "single"),width =  0.5) +
  labs(fill = "Group", y = "Group composition") 
p

plot_function(data_for_plot, data_for_plot$X90s, "Match Played")
plot_X90s_c6 <- output_plot
plot_X90s_c6

plot_function(data_for_plot, data_for_plot$goal.90, "Goal per season")
plot_Crs_c6 <- output_plot
plot_Crs_c6

plot_function(data_for_plot, data_for_plot$Crs, "Cross")
plot_crs_c6 <- output_plot
plot_crs_c6
