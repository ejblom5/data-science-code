rm(list = ls())

library(ggplot2)

# Author: Alex Herzog
# Date: Fall 2020
# Purpose: EDA of senate candidate Twitter data

data <- read.csv("combined_tweets.csv")

dim(data)

names(data)

head(data$author)

tmp1 <- aggregate(article_id ~ author + incumbent + party + state, data, length)
names(tmp1)[names(tmp1)=="article_id"] <- "tweets"

tmp2 <- aggregate(followers ~ author + incumbent + party + state, data, max)

a <- merge(tmp1, tmp2)


# Visualizing distributions
# Categorical variables
summary(a$incumbent)
plot(a$incumbent)

a$incumbent <- factor(a$incumbent, levels=c(0,1), 
                      labels=c("Not incumbent", "Incumbent"))

plot(a$incumbent)

p <- ggplot(data=a, mapping = aes(x=incumbent)) +
    geom_bar()
print(p)

p <- ggplot(a, aes(x=party)) +
    geom_bar()
print(p)


# Continuous variables
summary(a$tweets)
plot(a$tweets)

p <- ggplot(a, aes(x=tweets)) +
    geom_histogram()
print(p)

a$author[which(a$tweets > 7000)]

p <- ggplot(a[a$author!="corybooker",], aes(x=tweets)) +
    geom_histogram()
print(p)


# Categorical and continuous variable
p <- ggplot(a, aes(x=party, y=tweets)) +
    geom_boxplot()
print(p)

p <- ggplot(a, aes(x=state, y=tweets)) +
    geom_boxplot()
print(p)


# Two continuous variables
p <- ggplot(a, aes(x=tweets, y=followers)) +
    geom_point()
print(p)

p <- ggplot(a[a$author!="corybooker",], aes(x=tweets, y=followers)) +
    geom_point() +
    geom_smooth(method='lm')
print(p)


p <- ggplot(a[a$author!="corybooker",], aes(x=tweets, y=followers)) +
    #facet_wrap(~ incumbent) +
    geom_point()
    #geom_smooth(method='lm')
print(p)


# Two categorical variables
# Not a good plot when the number of categories is small, but I am including the code here for illustrative purposes!
p <- ggplot(a, aes(x=party, y=incumbent)) +
    geom_count()
print(p)

