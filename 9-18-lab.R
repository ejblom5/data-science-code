# Author: Erik Blom
# Date: 9-18-20
# Purpose: Linear regression in R

library(MASS)
library(ISLR)
View(Boston)
names(Boston)
summary(Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit
summary(lm.fit)
confint(lm.fit)
34.55384-(0.95005*5)
34.55384-(0.95005*10)
34.55384-(0.95005*15)
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)
plot(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)
library(ggplot2)
ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method="lm")
help(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower)
lm.fit
summary(lm.fit)
ggplot(Auto, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method="lm")
predict(lm.fit, data.frame(horsepower = c(120, 200, 300)), interval = "confidence")
