# Homework 2: Classification
# Erik Blom
# 11-8-2020

load("class_survey.RData")
library(ggplot2)
library(boot)

dim(survey)
# clean data
na.omit(survey)
survey <- survey[complete.cases(survey), ]

attach(survey)
sapply(survey,class)
View(survey)
summary(survey)

# 1) pineapple category
# there is an equal distribution of people that prefer
# pineapple on pizza and those that do not in the class
g <- ggplot(survey, aes(pineapple))
g + geom_bar()

# 2) logistic regression model
model <- glm(pineapple ~ social_media+roommates+distance_home+movies+states_visited,family="binomial")
summary(model)
# none of the predictors are very statistically significant
# the ones with a negative impact are socal_media,roommates,and states_visited
# the ones with a positive impact are distance_home and movies
# the strongest predictor is social_media with a coefficient of -4.122e-02

# 3) predictions holding everything constant but social media
pred.data <- data.frame(social_media=c(5,10,15,20),
                        roommates=2,
                        distance_home=300,
                        movies=20,
                        states_visited=12)
predict(model,pred.data,type="response")
# results:
# 1         2         3         4 
# 0.5351815 0.4817140 0.4286620 0.3772005

# 4) 5-fold cv. Threshold of 0.6
mycost <- function(r,pi=0) {
   mean(abs(r-pi) > 0.6)
}

cv.error.5 <- cv.glm(survey,model,cost=mycost,K=5)
cv.error.5$delta

# 5) LOOCV. Threshold of 0.6
loocv.error <- cv.glm(survey,model,cost=mycost,K=nrow(survey))
loocv.error$delta

# the loocv produces a smaller missclassification error
# the adjusted missclass. error for 5 fold was around 0.2756
# the adjusted missclass. error for loocv was around 0.2389
# Depends on the context, but i would say that generally a missclassifcation
# rate greater than 0.2 is not acceptable in most cases.