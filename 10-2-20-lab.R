# Author: Erik Blom
# Date: 10-2-20
# Classification with logistic regression 

library(ISLR)
attach(Default)
summary(Default)
m.balance <- glm(default ~ balance, family = binomial)
summary(m.balance)

pred.data <- data.frame(balance=c(1000, 2000, 3000))
predict(m.balance, pred.data, type="response")

m <- glm(default ~ balance + income + student, family = binomial)
summary(m)

pred.data <- data.frame(balance=c(1000, 2000, 3000), income=1000, student="No")
predict(m, pred.data, type="response")

pred.probs <- predict(m, type = "response")

pred.default <- rep("No", nrow(Default))
pred.default[pred.probs > 0.9] <- "Yes"

confusion.matrix <- table(default,pred.default)
print(addmargins(confusion.matrix))
