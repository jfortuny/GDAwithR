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

# 3.3
data(galton, package="UsingR")
summary(galton)
ht <- "height (in)"
par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab=ht, main="Children", col="green")
  hist(parent, xlab=ht, main="Parents", col="blue")})

par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab=ht, main="Children", col="green", xlim = c(60,75), ylim = c(0,220), breaks = 40)
  hist(parent, xlab=ht, main="Parents", col="blue", xlim = c(60,75), ylim = c(0,220), breaks = 40)})

par(mfrow=c(1,2), mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  MASS::truehist(child, h=0.1)
  MASS::truehist(parent, h=0.1)})

c1 <- ggplot(galton, aes(child)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept=median(galton$child),
             col="red")
p1 <- ggplot(galton, aes(parent)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent),
             col="red")
#grid.arrange(c1, p1)
library(cowplot)
plot_grid(c1, p1, nrow = 2, ncol = 1)

c1 <- ggplot(galton, aes(child)) + geom_histogram( bins = 40) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept=median(galton$child),
             col="red")
p1 <- ggplot(galton, aes(parent)) + geom_histogram( bins = 40) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent),
             col="red")
#grid.arrange(c1, p1)
library(cowplot)
plot_grid(c1, p1, nrow = 2, ncol = 1)
