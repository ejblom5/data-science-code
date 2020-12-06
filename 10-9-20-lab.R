# Erik Blom
# Lab - Resampling Methods
# 10-9-20

library(ISLR)
library(boot)
attach(Default)
summary(Default)
set.seed(42)

N <- nrow(Default)
train <- sample(N, N/2)
glm.fit <- glm(default ~ balance + income, family = binomial, subset=train)
pred.probs <- predict (glm.fit, Default[-train,], type = "response")
pred.default <- rep("No", N/2)
pred.default[pred.probs > 0.5] <- "Yes"
error.rate <- mean(pred.default != default[-train])
print(error.rate)

# kfold
mycost <- function(r, pi = 0) {
  mean(abs(r-pi) > 0.5)
}


# general function cv.glm(data, glmfit, cost, K)
glm.fit <- glm(default ~ balance + income, family = binomial)
cv.error.10 <- cv.glm(Default, glm.fit, cost=mycost, K=10)
cv.error.10$delta

# leave one out
loocv.error <- cv.glm(Default, glm.fit, cost=mycost, K = nrow(Default))
loocv.error$delta
