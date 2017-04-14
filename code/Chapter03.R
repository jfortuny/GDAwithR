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
# Figure 3.2
data(galton, package="UsingR")
summary(galton)
ht <- "height (in)"
par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab=ht, main="Children", col="green")
  hist(parent, xlab=ht, main="Parents", col="blue")})
# My version of Figure 3.3
par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab=ht, main="Children", col="green", xlim = c(60,75), ylim = c(0,220), breaks = 40)
  hist(parent, xlab=ht, main="Parents", col="blue", xlim = c(60,75), ylim = c(0,220), breaks = 40)})

# Figure 3.3
par(mfrow=c(1,2), mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  MASS::truehist(child, h=0.1)
  MASS::truehist(parent, h=0.1)})

# Figure 3.4
c1 <- ggplot(galton, aes(child)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept=median(galton$child),
             col="red")
p1 <- ggplot(galton, aes(parent)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent),
             col="red")
#grid.arrange(c1, p1)
plot_grid(c1, p1, nrow = 2, ncol = 1)

# Figure 3.4 reviseted with skinny bars
c1 <- ggplot(galton, aes(child)) + geom_histogram( bins = 40) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept=median(galton$child),
             col="red")
p1 <- ggplot(galton, aes(parent)) + geom_histogram( bins = 40) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent),
             col="red")
#grid.arrange(c1, p1)
plot_grid(c1, p1, nrow = 2, ncol = 1)

# Figure 3.5
data(father.son, package = "UsingR")
str(father.son)
c2 <- ggplot(father.son, aes(sheight)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density() + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") + ggtitle("Sons")
p2 <- ggplot(father.son, aes(fheight)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density() + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") +
  ggtitle("Fathers")
#grid.arrange(c2, p2, nrow = 1)
plot_grid(c2, p2, nrow = 1, ncol = 2)

# Figure 3.6
with(father.son, {
  qqnorm(sheight, main="Sons", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(sheight)
  qqnorm(fheight, main="Fathers", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(fheight)})
library(nortest)
pearson.test(father.son$fheight)
ad.test(father.son$fheight)

# Scottish Hill races
data(hills, package = "MASS")
str(hills)
hist(hills$time)
with (hills, {
  MASS::truehist(time)
})
qplot(hills$time)
# Figure 3.7
par(mfrow=c(1,1), mar=c(3.1, 4.1, 1.1, 2.1))
with(MASS::hills,
     boxplot(time, horizontal=TRUE, pch=16, ylim=c(0, 220)))

