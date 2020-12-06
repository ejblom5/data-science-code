library(ISLR)
library(splines)
library(gam)
attach(Wage)


age.grid <- seq(from=min(age),to = max(age))
degrees <- c(1,2,3,4)
for (degree in degrees){ 
  fit <- lm(wage ~ poly(age,4))
  pred <- predict(fit,newdata = list(age = age.grid),se=TRUE)
  se.bands <- cbind(pred$fit + 2*pred$se.fit,pred$fit-2*pred$se.fit)
  if (degree==1)plot(age,wage,cex=.5,col="darkgray",main="Degree-4Polynomial")
  lines(age.grid,pred$fit,lwd=2,col="blue")
  matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
}
#poly logistic regression
fit<-glm(I(wage>250)~poly(age,4),family=binomial)
pred<-predict(fit,newdata=list(age=age.grid),se=TRUE)
pfit<-exp(pred$fit)/(1+exp(pred$fit))
se.bands.logit<-cbind(pred$fit + 2*pred$se.fit,pred$fit-2*pred$se.fit)
se.bands <- exp(se.bands.logit)/ (1+exp(se.bands.logit))

plot(age,I(wage>250),type="n",ylim=c(0,.2))
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
rug(jitter(age[wage<=250]),side=1,ticksize=0.02)
rug(jitter(age[wage>250]),side=3,ticksize=0.02)

# step functions
fit <- lm(wage~cut(age,4))

#splines
knot.position <- c(25,40.60)
fit <- lm(wage~bs(age,knots=knot.position),data=Wage)
pred<-predict(fit,newdata=list(age=age.grid),se=TRUE)
plot(age,wage,cex=.5,col="darkgray")
lines(age.grid,pred$fit,lwd=2,col="red")
lines(age.grid,pred$fit + 2*pred$se,lty="dashed",col="red")
lines(age.grid,pred$fit-2*pred$se,lty="dashed",col="red")
abline(v=knot.position,lty="dashed")

#smoothing splines
fit <- smooth.spline(age,wage,df=16)
fit2 <- smooth.spline(age,wage,cv=TRUE)

plot(age,wage,cex=.5,col="darkgray")
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF (LOOCV)"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# GAM
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education)
gam2 <- gam(wage~s(year,4)+s(age,5)+education)
par(mfrow=c(1,3))
plot(gam2,se=TRUE,col="blue")
