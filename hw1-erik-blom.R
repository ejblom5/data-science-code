# Homework 1
# Erik Blom
# 10-19-2020

load("class_survey.RData")
library(ggplot2)
dim(survey)
na.omit(survey)

names(survey)
p <- ggplot(survey, aes(x=games)) + geom_histogram()
print(p)

lm.fit <- lm(games ~ exercise+us+os+social_media+facebook+movies+roommates+pineapple+death_valley+states_visited+sweet_tea+semesters_clemson+semesters_college+country+football+siblings+distance_home,data=survey)
summary(lm.fit)

# semesters college
# an increase in the total number of semesters in college by 1
# is associated with an average increase in the number of hours playing games per week by 1.287 hours
# while holding all other predictors constant

# since there are so many predictors I will asses the model fit using the adjusted R squared
# the adjusted R squared is 0.2148 which indicated the model is not a good fit
