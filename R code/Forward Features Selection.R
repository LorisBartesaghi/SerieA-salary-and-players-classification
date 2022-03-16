#import libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(caret)
library(MASS)
library(leaps)
library(olsrr)
library(randomForest)
library(alookr)
library(boot)
library(lmtest)
library(mlbench)
library(car)
library(plotly)
library(ggpubr)

#import dataset
data <- read.csv("data for analysis.csv")
data <- data[, -c(1)]

#import same dataset, for extract original information during tha analysis
data_starting <-read.csv("data for analysis.csv")
##################################################################################################
########DATA UNDERSTANDING########################################################################

#nationality is not a statistical data on performance players, exclude it
###Analysis on Yearly.Salary

data <- data[,-c(2,4)]
#search for normality in salary
ggdensity(data, x = "Yearly.Salary", fill = "lightgray", title = "Gross Yearly Salary") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
shapiro.test(data$Yearly.Salary)

#strong rejection of normality for Yearly.salary, try to introduce the BoxCox transformation(for regression)

lr <- lm(Yearly.Salary~ ., data = data)

bc = boxcox(lr, lambda = seq(-5,5))
best_lam = bc$x[which(bc$y==max(bc$y))]

#use Box-Cox transformation(wait to transform, highly correlated variables are a problems for linear models)
data$Y_bc <- (((data$Yearly.Salary)^(best_lam))-1)/best_lam

ggdensity(data, x = "Y_bc", fill = "lightgray", title = "Gross Yearly Prices") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

shapiro.test(data$Y_bc) #even after the transformation, the Yearly.Salary is not normally distributed;
data$Y_bc <- NULL

###########################################################################
#transform players gross salary, use boc-cox transformation for the analysis
lr <- lm(Yearly.Salary~ ., data = data)

bc = boxcox(lr, lambda = seq(-5,5))
best_lam = bc$x[which(bc$y==max(bc$y))]

#use Box-Cox transformation
data$Yearly.Salary <- (((data$Yearly.Salary)**(best_lam))-1)/best_lam

############################################################################
#scale variable
data1 <- data[,-c(1)]
ind <- sapply(data1, is.numeric)
data1[ind] <- lapply(data1[ind], scale)
data1$Yearly.Salary <- data$Yearly.Salary
###############################################################################################
###STEPWISE FORWARD SELECTION##################################################################

#Forward selection with K-fold cross-validation
set.seed(333)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Yearly.Salary ~., data = data1,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:25),
                    trControl = train.control
)

#analysis of the results from cross validation
step.model$bestTune #best number of variables to include
step.model$results #print rmse, mae and r squared
print(coef(step.model$finalModel, 20)) #print the coefficient of the selected variables


#linear model with selected variables
linear <- lm(Yearly.Salary ~ Age+ team.y  + Sh.90 + total.completed + X.total..completed. + GCA, data = data1)
summary(linear)
plot(linear)

car::vif(linear) #find correlation between selected variables



#linear model without residual
linear <- lm(Yearly.Salary ~ Age+ team.y  + Sh.90 + total.completed + X.total..completed. +  GCA, data = data1)
summary(linear)
plot(linear)

#remove outliers, not nornality of errors detected looking at the plots of lineare regression
cd <- cooks.distance(linear)
influential <- as.numeric(names(cd)[(cd > 4 * mean(cd, na.rm = TRUE))])
length(influential)
data2 <- data[-influential,]



#compute estimate coefficient, removing the outliers
linear <- lm(Yearly.Salary ~ Age+ team.y  + Sh.90 + total.completed + X.total..completed. +  GCA, data = data2)
summary(linear)
plot(linear)

#check normality of residuals, homoskedasticity and autocorrelation
ols_test_normality(linear) #normality
bptest(linear) #heteroskedasticity
durbinWatsonTest(linear) #autocorrelation



##############################################################################################################
#evaluate prediction performance of the model#################################################################


#create partitioning
set.seed(240)
bh_index <- createDataPartition(data1$Yearly.Salary, p = .75, list = FALSE)
bh_tr <- data1[ bh_index, ]
bh_te <- data1[-bh_index, ]
###compute training and test error
set.seed(6)
lm_fit <- train(Yearly.Salary ~ Age + team.y  + Sh.90 + total.completed + X.total..completed. +GCA,
                data = bh_tr, 
                method = "lm")

#compute prediction with lm_fit
bh_pred <- predict(lm_fit, bh_te)

#training error
lm_fit

#test error
postResample(pred = bh_pred, obs = bh_te$Yearly.Salary)
###############################################################################################################


