# Chapter 5
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Figure 5.1
data(oly12, package="VGAMdata")
str(oly12)
ggplot(oly12, aes(Height, Weight)) + geom_point() +
  ggtitle("Athletes at the London Olympics 2012")

# Figure 5.2
data(DrinksWages, package="HistData")
ggplot(DrinksWages, aes(drinks/n, wage)) + geom_point() +
  xlab("Proportion of drinkers") + xlim(0,1) + ylim(0,40)

with(DrinksWages, hist(n, breaks = 0:max(n)))
with(DrinksWages, table(n))
with(DrinksWages, max(n[drinks==0]))
with(DrinksWages, trade[drinks==0 & n==max(n[drinks==0])])
with(DrinksWages, max(n[sober==0]))
with(DrinksWages, trade[sober==0 & n==max(n[sober==0])])

# Figure 5.3
bigDW <- filter(DrinksWages, n > 4)
ggplot(bigDW, aes(drinks/n, wage)) + geom_point() +
  xlab("Proportion of drinkers") + xlim(0,1) +  ylim(0,40)

# Figure 5.4
data(geyser, package="MASS")
ggplot(geyser, aes(duration, waiting)) + geom_point()

# Figure 5.5
ggplot(geyser, aes(duration, waiting)) + geom_point() +
  geom_density2d()

# Figure 5.6
library(hdrcde)
par(mar=c(3.1, 4.1, 1.1, 2.1))
with(geyser, hdr.boxplot.2d(duration, waiting, 
                            show.points=TRUE, prob=c(0.01,0.05,0.5,0.75)))

# Figure 5.7
data(movies, package = "ggplot2movies")
ggplot(movies, aes(votes, rating)) + geom_point() + ylim(1,10)

# Figure 5.8
data(Cars93, package="MASS")
ggplot(Cars93, aes(Weight, MPG.city)) + geom_point() +
  geom_smooth(colour="green") + ylim(0,50)

# Figure 5.9
data(father.son, package="UsingR")
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
  geom_smooth(method="lm", colour="red") +
  geom_abline(slope=1, intercept=0)
m1 <- lm(sheight~fheight, data = father.son)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

# Figure 5.10
data(father.son, package="UsingR")
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
  geom_smooth(method="lm", colour="red", se=FALSE) +
  stat_smooth()
