# Chapter 3

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
#
grid.arrange(c1, p1)
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

# Figure 3.8
ggplot(MASS::Boston, aes(medv)) + geom_bar() + ylab("") +
  xlab("Median housing value (thousands of dollars)")
# or
ggplot(MASS::Boston, aes(medv)) + geom_histogram(aes(y=..density..), binwidth = 1) + ylab("") +
  xlab("Median housing value (thousands of dollars)")
# or
with(MASS::Boston, {
  MASS::truehist(medv, h=1)
})

# Figure 3.9
data(Boston, package="MASS")
str(Boston)
B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
ggplot(B2, aes(BosValues)) + geom_histogram() + xlab("") +
  ylab("") + facet_wrap(~ BosVars, scales = "free")

with(Boston, hist(ptratio))
with(Boston, MASS::truehist(ptratio))

range(Boston$medv)
boxplot(Boston$medv, pch=16, horizontal = TRUE, ylim=c(0,60))

stripchart(Boston$medv, method = "jitter", pch=16)

stem(Boston$medv)

library(ash)
plot(ash1(bin1(Boston$medv, nbin = 50)), type = "l")

d1 <- density(Boston$medv)
plot(d1, ylim = c(0,0.08))
rug(Boston$medv)
lines(density(Boston$medv, d1$bw/2), col = "green")
lines(density(Boston$medv, d1$bw/5), col = "blue")

# Figure 3.10
library(KernSmooth)
#data(Hidalgo1872, package="MMST")
Hidalgo1872 <- readxl::read_excel("./data/Hidalgo.xlsx")
par(las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(Hidalgo1872, {
  hist(thickness,breaks=seq(0.055,0.135,0.001), freq=FALSE, main="", col="bisque2", ylab="")
  lines(density(thickness), lwd=2)
  ks1 <- bkde(thickness, bandwidth=dpik(thickness))
  lines(ks1, col="red", lty=5, lwd=2)})

# Figure 3.11
data(movies, package = "ggplot2movies")
ggplot(movies, aes(length)) + geom_bar() + ylab("")
ggplot(movies, aes(length)) + geom_bar() + ylab("") + ylim(c(0,1))

# Figure 3.12
ggplot(movies, aes("var", length)) + geom_boxplot() +
  xlab("")  + scale_x_discrete(breaks=NULL) + coord_flip()
longMovies <- filter(movies, length > 2000)
print(longMovies[,c("length", "title")], row.names = FALSE)

# Figure 3.13
ggplot(movies, aes(x = length)) +  xlim(0,180) +
  geom_histogram(binwidth=1)  +
  xlab("Movie lengths in minutes") + ylab("")

# Figure 3.14
data(btw2009, package = "flexclust")
btw2009 <- within(btw2009, Bundesland <- state)
btw2009 <- within(btw2009, levels(Bundesland) <- c("BW", "BY", "BE", "BB",
                                                   "HB", "HH", "HE", "MV", "NI", "NW","RP", "SL", "SN", "ST", "SH", "TH"))
ggplot(btw2009, aes(Bundesland, LINKE2)) + geom_boxplot(varwidth=TRUE) + ylab("")
