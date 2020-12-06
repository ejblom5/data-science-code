# Homework 3: Decision Trees
# Erik Blom
# 11-24-2020

load("class_survey.RData")
library(ggplot2)
library(randomForest)
library(gbm)
library(boot)
set.seed(42)

na.omit(survey)
survey <- survey[complete.cases(survey), ]

#1. Make a plot of the response 'exercise' that shows the distribution of observations. 
#Also report the minimum, maximum, and average value in the response. Briefly comment on what you observe.
summary(survey$exercise)
# min: 0 hrs, max: 14 hrs, average: 3.987 hrs
# the average student in the class excercises roughly 4 hours a week
# while some student(s) excercise up to 14hrs a week
ggplot(survey, aes(x=exercise)) + 
  geom_histogram(binwidth = 1)
# the plot seems to have a normal distribution centering around
# 4 hours of excercise a week with some more extreme values at 10 and 15 hrs


#2. Split your data into a training set and test set, 
N <- nrow(survey)
test <- sample(1:N, 12)
train <- seq(1:N)[-test]


#3. Estimate three random forests with all other columns in the data as predictors.
P <- ncol(survey)
# random forest with remaining predictors (it threw and error when I set mtry=P so I used P-1 instead)
rf.p <-  randomForest(exercise~., data=survey, subset=train, mtry=P-1, importance=TRUE)
# with P/3 predictors
rf.third.p <-  randomForest(exercise~., data=survey, subset=train, mtry=P/3, importance=TRUE)
# with square root of P predictors
rf.sqrt.p <-  randomForest(exercise~., data=survey, subset=train, mtry=sqrt(P), importance=TRUE)


#4. Estimate the test MSE for each of the three models. Briefly comment on what you observe. Which MSE is the smallest? Why?
# as the random forest reduces the number of predictors taken into account the test MSE reduces
# the rf with squareroot of P predictors has the lowest test mse since it probably overfits the
# training data the least amount

survey.test <- survey[test, "exercise"]
yhat <- predict(rf.p, survey[test,])
mean((yhat-survey.test)^2)

yhat <- predict(rf.third.p, survey[test,])
mean((yhat-survey.test)^2)

yhat <- predict(rf.sqrt.p, survey[test,])
mean((yhat-survey.test)^2)


#5. Fit a boosted regression tree and estimate its test MSE. 
boosted <- gbm(exercise~., data=survey[train,], distribution="gaussian",bag.fraction = 1,shrinkage=0.001)

# test mse for boosted model
yhat.boost <- predict(boosted, newdata=survey[test,],n.trees = 100)
mean((yhat.boost-survey.test)^2)
