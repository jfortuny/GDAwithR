# Chapter 1
library(GDAdata)

# Exercise 1
ggplot(data = iris, aes(x = Sepal.Width)) + geom_histogram(binwidth = 0.1)

ggplot(data = iris, aes(x = Sepal.Width, color = Species)) + geom_histogram(binwidth = 0.1)
ggplot(data = iris, aes(x = Sepal.Width, color = Species)) + geom_density()

# Exercise 2
ggplot(data = MASS::Pima.tr2, aes(type)) + geom_bar()
