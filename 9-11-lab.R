# Author: Erik Blom
# Date: 9-11-20
# Purpose: EDA and Visualization

library(ggplot2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg)
dim(mpg)
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
