# Chapter 3
library(GDAdata)

data(btw2009, package = "flexclust")
summary(btw2009)
btw2009 <- within(btw2009, Linke2 <- 100*LINKE2/valid2)
ggplot(btw2009, aes(Linke2)) + geom_bar(binwidth = 1,
                                        fill = "mediumpurple") + 
  ylab("") +
  xlab("Percentage voter support for Die Linke in 2009")

ggplot(btw2009, aes(Linke2)) + geom_histogram(binwidth = 1,
                                              fill = "mediumpurple") +
  ylab("") +
  xlab("Percentage voter support for Die Linke in 2009")