library(MASS)
library(tree)
library(randomForest)
library(gbm)
set.seed(1)
N <- nrow(Boston)
train <- sample(1:N, N/2)
test <- seq(1:N)[-train]




tree.boston <- tree(medv ~., Boston,subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")

prune.boston <- prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston)

yhat.tree <- predict(tree.boston,Boston[test,])
boston.test <- Boston[test,"medv"]

plot(yhat.tree, boston.test)
abline(0,1)
mean((yhat.tree-boston.test)^2)

lm.boston <- lm(medv~ ., Boston, subset=train)
yhat.lm <- predict(lm.boston,Boston[test,])
boston.test <- Boston[test,"medv"]
mean((yhat.lm-boston.test)^2)

set.seed(1)

P <- ncol(Boston)-1
bag.boston <- randomForest(medv~., data=Boston, subset=train,mtry=P,importance=TRUE)
bag.boston

yhat.bag <- predict(bag.boston,Boston[test,])
mean((yhat.bag-boston.test)^2)

bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=P, ntree=25)
yhat.bag <- predict(bag.boston, Boston[test,])
mean((yhat.bag-boston.test)^2)

rf.boston <- randomForest(medv~., data=Boston, subset=train, ry=6, importance=
                              TRUE)
yhat.rf <- predict(rf.boston, Boston[test,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)

boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees
                    =5000, interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost <- predict(boost.boston, newdata=Boston[test,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees
                    =5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost <- predict(boost.boston, Boston[test,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
