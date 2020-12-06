college <- read.csv("College.csv")
View(college)
rownames(college) <- college[ ,1]
View(college[,3:12])
summary(college)
college$Private <- as.factor(college$Private)
pairs(college[,1:10])
plot(college$Private, college$Outstate)