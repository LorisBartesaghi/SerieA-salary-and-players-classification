library(hrbrthemes)
library(viridis)
library(caret)
library(MASS)
library(leaps)
library(olsrr)
library(randomForest)
library(xtable)
library(plotly)
##########################################################################################################################
#import data
data <- read.csv("data for analysis.csv")
data <- data[, -c(1)]

#import same dataset, necessary to extract starting information deleted in 'data' during the analysis
data_starting <-read.csv("data for analysis.csv")
###########################################################################################################################
#transform yearly salary, with box-cox transformation
data <- data[,-c(2,4)]

lr <- lm(Yearly.Salary~ ., data = data)

bc = boxcox(lr, lambda = seq(-5,5))
best_lam = bc$x[which(bc$y==max(bc$y))]

#use Box-Cox transformation
data$Yearly.Salary <- (((data$Yearly.Salary)^(best_lam))-1)/best_lam
###########################################################################################################################
#scale variable
data1 <- data[,-c(1)]
library(MASS)
ind <- sapply(data1, is.numeric)
data1[ind] <- lapply(data1[ind], scale)
data1$Yearly.Salary <- data$Yearly.Salary
#################################################FEATURES SELECTION WITH RF################################################
#create mean squared error function
mse = function(y_hat, y_true) {
  mean((y_hat - y_true)^2)
}

#compute mse for given values of trees and depth of the trees
error.oob = c()
error.train = c()
error.depth = c()
error.tree = c()
for(tree in c(100,250,500,1000,2000)){
  for(depth in c(2,8,16,32)) {
    rf <- randomForest(Yearly.Salary ~ . , data = data, ntree=tree,
                       mtry=sqrt(dim(data)[2]), maxnodes=depth)
    error.oob = c(error.oob, rf$mse[tree])
    y_hat = predict(rf, data)
    error.train = c(error.train, mse(data$Yearly.Salary, y_hat))
    error.depth = c(error.depth, depth)
    error.tree = c(error.tree, tree)
  }
}

#create tibble with test and training error for each combination of trees and max number of nodes
error = tibble(error.oob = sqrt(error.oob),
               error.train = sqrt(error.train),
               error.depth = as.factor(error.depth),
               error.tree= error.tree)

#view table
view(error)

#plot errors table
ggplot(data=error) +
  geom_line(mapping=aes(x=error.tree, y=error.train, color=error.depth)) +
  geom_point(mapping=aes(x=error.tree, y=error.train, color=error.depth)) +
  geom_line(mapping=aes(x=error.tree, y=error.oob, color=error.depth)) +
  geom_point(mapping=aes(x=error.tree, y=error.oob, color=error.depth)) +
  scale_x_discrete(limits=c(100,250,500,1000,2000))

##########################################################################################################################
#run random forest with 500 trees and 8 nodes
set.seed(632)
#run random forest
rf <- randomForest(Yearly.Salary ~ . , data = data, ntree=500, importance = TRUE,
                   mtry=sqrt(dim(data)[2]), maxnodes=8)

#plot the 10 most importna variables
varImpPlot(rf, n.var = 10)

#create a table with imortant variables for both criterion (incMSE and Purity)
imp = as.data.frame(importance(rf))
colnames(imp) = c("mse","purity")

# select variables based on %IncMSE
imp = imp[order(imp$mse, decreasing=TRUE),]

# select first 6 variable
var_selected = c(rownames(imp[1:6, ]), "Yearly.Salary")
data[,var_selected]

#create a new data set with only the variables selected
data2 = data[, var_selected]

##########################################################################################################################
#apply linear regression
#run regression on selected varaibles
lr = lm(Yearly.Salary ~ ., data=data2)
summary(lr)
plot(lr)

#multicollnearity test
car::vif(lr)

#remove outliers
cd <- cooks.distance(lr)
influential <- as.numeric(names(cd)[(cd > 4 * mean(cd, na.rm = TRUE))])
length(influential)
data3 <- data2[-influential,]

#run again regression without correlated varaibles and outliers
lr = lm(Yearly.Salary ~., data=data3)
summary(lr)
plot(lr)

##########################################################################################################################
#model evaluation
#create partitioning to compute rmse for the model
set.seed(40)
bh_index <- createDataPartition(data2$Yearly.Salary, p = .75, list = FALSE)
bh_tr <- data2[ bh_index, ]
bh_te <- data2[-bh_index, ]

#compute accuracy for the linear model
set.seed(36)
lm_fit <- train(Yearly.Salary ~.,
                data = bh_tr,
                method = "lm")

bh_pred <- predict(lm_fit, bh_te)
lm_fit
postResample(pred = bh_pred, obs = bh_te$Yearly.Salary)

###########################################################################################################################
###compute prediction and differences
#prediction with regression in box-cox scale
data$Prediction <- predict(lr, data)
#transform prediction in original scale
data$Prediction_real <- (1+(data$Prediction*best_lam))^(1/best_lam)

#add again Yearly Salary (original scale) and Player Name
data$real <- data_starting$Yearly.Salary
data$name <- data_starting$Player.Name

#Compute differences between real and predicted salary
data$diff <- round(data$real - data$Prediction_real,0)
##########################################################################################################################
###aggregate by teams, to understand managers' performances

overpaid_table <- aggregate(data$diff, by=list(Category=data$team), FUN=sum)

#plot of over-expenditure by team
fig <- plot_ly(data = overpaid_table,
               x = ~Category,
               y = ~x,
               type = "bar")

fig <- fig %>% layout(xaxis = list(title = "Gross Salary Expenditure"), yaxis = list(title = "Team"))
fig

###########################################################################################################################
###Logistic regression to compute probabilities to be overpaid by team
#filter on overpaid player on box-cox scale
data$diff_Box <- data$Yearly.Salary - data$Prediction

#create a subset for overpaid players
data_over <- data %>% filter(data$diff_Box > 0)
data_over <- data_over[order(-data_over$diff_Box),]

#compute quantilies for the overpaid players
quantiles <- quantile(data_over$diff_Box, probs = seq(0, 1, by= 0.1)) 
data <- data[order(-data$diff_Box),]

#create dummy variable for the 20% most overpaid in the league
data$higly_over <- ifelse(data$diff_Box > quantiles[8],1,0)


#apply logistic regression to compute the probability to be overpaid in each team
logi <- glm(data = data, higly_over ~ team.y, family = "binomial")

#extract coefficient
coeff <- summary(logi)$coef[1:20,1]
coeff <- round(coeff,2)
view(coeff)

##########################################################################################################################
#function for retrieving probabilities from coefficients of logistic regression
retrive_prob <- function(position){
  prob <- (exp(coeff[1]+coeff[position]))/(1+exp(coeff[1]+coeff[position]))
  return(prob)
}

#retrieve vector of probabilities for each team
probabilities_vector <- c()
for (i in 2:20){
  probabilities_vector <- c(probabilities_vector,retrive_prob(coeff[i]))
}
view(probabilities_vector)
#add probability for the team which has no coefficient
probabilities_vector <- probabilities_vector[1:19]
probabilities_vector <- round(probabilities_vector, 3)
probabilities_vector <- append(probabilities_vector,(exp(coeff[1]))/(1+exp(coeff[1])), 0)
probabilities_vector <- as.data.frame(probabilities_vector)
rownames(probabilities_vector)[1] <- "ac-milan"
probabilities_vector$dif <- overpaid_table$x

