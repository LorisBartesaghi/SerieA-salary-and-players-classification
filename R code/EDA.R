library(tidyverse)
library(hrbrthemes)
library(viridis)
library(caret)
library(MASS)
#import useful libraries
library(leaps)
library(olsrr)
library(randomForest)
library(alookr)
library(boot)

#IMPORT DATASET
data<- read.csv("Final_dataset.csv")
data<- data[, -c(1)]
###############################################################DATA UNDERSTANDING###########################
#I decide to remove GoalKeeper from my analysis, due to the fact that the parameter to evaluate the performance are different
data <- data[!grepl("GK", data$Position),]
 

###Search for Na values
which(is.na(data), arr.ind=TRUE)
#the Na values are present only in the column "Won." when the player have zero on both "Won" and "Lost
#the best way to solve the problem(problems will arise when a linear regression is applied) is to sobstitute Na with 0. 
data$Won.[is.na(data$Won.)] <- 0

#################################################################################################
###################################################################################################################
#########################################REGRESSORS ANALYSIS#######################################################
###################################################################################################################
#COMPUTE THE CORRELATION MATRIX
data_correlation <- data[,-c(1,2,4,5,6)]
corr1 <- cor(data_correlation)

#FIND THE NUMBER OF HIGLY CORRELATED VARIABLE
hc <- findCorrelation(data_correlation, cutoff = 0.8)
length(hc)
#REMOVE THE UPPER TRIANGLE OF THE CORRELATION MATRIX
corr1[upper.tri(corr1)] <- 0

#correlation matrix to data.frame
corr1 <- as.data.frame(corr1)

#function to extract names of higly correlated variable, with threshold to specify
print_correlated_variable <- function(cor_matrix,index, threshold){
  vector <- c()
  for (i in 1:length(cor_matrix)){
    if(corr1[i,index] > threshold){
      vector <- c(vector, rownames(corr1)[i])
    }
  }
  cor_variables <<- vector
  return(vector)
}

#print correlation matrix of higly correlated variables
print_correlation <- function(cor_matrix, vector){
  return(corr1[vector,vector])
}

#remove selected higly correlated variables
removing_correlation <- function(cor_data, names){
  corr1 <<- cor_data[-which(names(cor_data) %in% cor_variables), -which(names(cor_data) %in% cor_variables)]
}

###MAKE A ONE TO ONE ANALYSIS FOR EACH REGRESSOR TO IDENTIFY PROBLEMATIC VARIABLE, I WOULD LIKE TO CONTROL THE PROCESS OF
###SELECTION, BECAUSE MANY FOOTBALL STATISTICS ARE HIGHLY CORRELATED, BUT SOMETIMES CAN EXPRESS VERY DIFFERENT FEATURE OF PERFORMANCES

#STEP 1: correlation between variables which express time played for each player
print_correlated_variable(corr1, 2, 0.85)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(4)] #X90s is a good proxy for the removed variable.'Targ' is correlated, but it measures different thing
removing_correlation(corr1, cor_variables)


#STEP 2: removing variables which express information on goals/shot on target
print_correlated_variable(corr1, 3, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1,7)] #goal.90 good proxy variable shots, not for passages received
removing_correlation (corr1, cor_variables)


#STEP 3: removing variables which explayns dangerous passages
print_correlated_variable(corr1, 4, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(5)] #GCA is the most informative variable for key passes or assists
removing_correlation (corr1, cor_variables)


#STEP 4: Penalty variables
print_correlated_variable(corr1, 4, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(2)] #Penalty Kicked variable
removing_correlation (corr1, cor_variables)


#STEP 5: retain gca.90 to express dangerous situation created by the players (ast,..)
print_correlated_variable(corr1, 7, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(4)] #GCA.90 is the most usefull variables to explain the others higly correlated
removing_correlation (corr1, cor_variables)


#STEP 6: percentage of gol on shot/shot on target
print_correlated_variable(corr1, 11, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1)] #retain G.Sh 
removing_correlation (corr1, cor_variables)


#STEP 7: remove highly correlated variables with respect to passages, difensive actions and touches
print_correlated_variable(corr1, 14, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1,13,16)] #retain total.completed, Touches.
removing_correlation (corr1, cor_variables)


#STEP 8: retain att.3dy which is a features that can express the position on the field of a player
print_correlated_variable(corr1, 19, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(3)] #retain Att.3dy which is a proxy for touches in opposite last 1/3
removing_correlation (corr1, cor_variables)


#STEP 9: Cross variables
print_correlated_variable(corr1, 19, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(2)] #
removing_correlation (corr1, cor_variables)


#STEP 10: GCA is the best proxy for touches and 
print_correlated_variable(corr1, 25, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1)] #
removing_correlation (corr1, cor_variables)


#STEP 11: Defensive actions, TklW and Touches retained
print_correlated_variable(corr1, 32, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(2,11)] 
removing_correlation (corr1, cor_variables)


#STEP 12: Retain Tackle won and touches
print_correlated_variable(corr1, 32, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1,3)] 
removing_correlation (corr1, cor_variables)


#STEP 14: remove ball losing data for tackle
print_correlated_variable(corr1, 37, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(1,2)] 
removing_correlation (corr1, cor_variables)


#STEP 15: retain touches in def.penalty area
print_correlated_variable(corr1, 38, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(3)] 
removing_correlation (corr1, cor_variables)

#STEP 17: retain Touches
print_correlated_variable(corr1, 40, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(2)] 
removing_correlation (corr1, cor_variables)


#STEP 18: retain informations about the number of player dribbled
print_correlated_variable(corr1, 42, 0.80)
print_correlation(corr1, cor_variables)
cor_variables <- cor_variables[-c(3)] 
removing_correlation (corr1, cor_variables)


#create an index based on the remaining variables, plus the starting excluded variables
indexing <- rownames(corr1)
indexing <- append(indexing, "Yearly.Salary", 0)
indexing <- append(indexing,"Player.Name" , 1)
indexing <- append(indexing,"Position", 2)
indexing <- append(indexing, "Nationality",3)
indexing <- append(indexing, "team.y", 4)

#data
data <- data[,indexing]




#binarize PK, CrdY, Fld(80% of player had been drawn max 1 time)
data$PK <- ifelse(data$PK >0,"Yes","No")
data$CrdR <- ifelse(data$CrdR>0,"Yes","No")
data$Fld <- ifelse(data$Fld>4,"Over","Equal and under")
#remove player that are not useful for the purpose of the model, they played few minutes, but with assist.So the ast is not 

#player that shoot on target with 100% precision (data[data$SoT.==100,]), not realistc value for player that played
#5/10 minutes
players_high_precision <-data %>% filter(SoT. == 100) %>% filter(X90s < 1)
names_player_high_precision <- players_high_precision$Player.Name
data[data[,2]== names_player_high_precision[1],13] <- mean(data$SoT.)
data[data[,2]== names_player_high_precision[2],13] <- mean(data$SoT.)
data[data[,2]== names_player_high_precision[3],13] <- mean(data$SoT.)

#Players which have 100 of success in pass
Player_high_accuracy <- data %>% filter(X.total..completed. == 100) %>% filter(X90s <1 )
names_players_high_accuracy <- Player_high_accuracy$Player.Name
data[data[,2]== names_players_high_accuracy[1],20] <- mean(data$X.total..completed.)
data[data[,2]== names_players_high_accuracy[2],20] <- mean(data$X.total..completed.)
data[data[,2]== names_players_high_accuracy[3],20] <- mean(data$X.total..completed.)
data[data[,2]== names_players_high_accuracy[4],20] <- mean(data$X.total..completed.)
data[data[,2]== names_players_high_accuracy[5],20] <- mean(data$X.total..completed.)
data[data[,2]== names_players_high_accuracy[6],20] <- mean(data$X.total..completed.)

#players with to high Sh.90
Player_outliers_shot <- data %>% filter(Sh.90 > 3) %>% filter(X90s <1 )
name_players_outliers_shot <- Player_outliers_shot$Player.Name
data[data[,2]== name_players_outliers_shot[1],14] <- mean(data$Sh.90)
data[data[,2]== name_players_outliers_shot[2],14] <- mean(data$Sh.90)
data[data[,2]== name_players_outliers_shot[3],14] <- mean(data$Sh.90)
data[data[,2]== name_players_outliers_shot[4],14] <- mean(data$Sh.90)
data[data[,2]== name_players_outliers_shot[5],14] <- mean(data$Sh.90)
data[data[,2]== name_players_outliers_shot[6],14] <- mean(data$Sh.90)

#players with to high GCA90, with less than a match played
players_gca <-data %>% filter(GCA90 > 1.2) %>% filter(X90s <1 ) 
name_players_sca <- players_gca$Player.Name
data[data[,2]== name_players_sca[1],31] <- mean(data$GCA90)
data[data[,2]== name_players_sca[2],31] <- mean(data$GCA90)
data[data[,2]== name_players_sca[3],31] <- mean(data$GCA90)
data[data[,2]== name_players_sca[4],31] <- mean(data$GCA90)


#players with to high SCA90, with less than a match played
players_sca <-data %>% filter(SCA90 > 2) %>% filter(X90s <1 ) 
name_players_sca <- players_sca$Player.Name
data[data[,2]== name_players_sca[1],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[2],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[3],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[4],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[5],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[6],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[7],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[8],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[9],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[10],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[11],24] <- mean(data$SCA90)
data[data[,2]== name_players_sca[12],24] <- mean(data$SCA90)


#remove data about ATG, information are in other columns and over 80% of the values are 0
data$ATG.drib <- NULL
data$ATG.finalized <- NULL
data$ATG.fouls <- NULL
data$ATG.not.in.play <- NULL
data$ATG.shot <- NULL

#remove Succ. (same information of Drib), Sub_fouls(Same as Fld)
data$Succ. <- NULL
data$sub_fouls <- NULL


#remove X., Tkl., ShSv, Err., Rec., X2Crdy, OG <- no correlation with yearly.salary
data$X. <- NULL
data$Tkl. <- NULL
data$ShSv <- NULL
data$Err. <- NULL
data$Rec. <- NULL
data$X2Crdy <- NULL
data$OG <- NULL
data$Fls <- NULL

#create csv
write.csv(data, "data for analysis.csv")


####function for analysis of numerical variables
integer <- function(integer_variable){
  
  fig <- ggplot(data, aes(x= integer_variable, y = Yearly.Salary)) + 
    geom_point()+
    geom_smooth(method = "lm")
  print(fig)
  
  
  fig2 <- ggplot(data, aes(x= integer_variable)) + 
    geom_boxplot(fill="slateblue", alpha=0.2)
  print(fig2)
  
  cor(integer_variable, data$Yearly.Salary)
}


####function for analysis of categorical variables
categorical <- function(categorical_variable){
  fig1 <- data %>%
    ggplot( aes(x= categorical_variable, y= Yearly.Salary, fill=categorical_variable)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("A boxplot with jitter") +
    xlab("")
  
  print(fig1)
}
  




