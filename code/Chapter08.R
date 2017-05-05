# Chapter08
source('~/R Work/GDAwithR/code/StartLibraries.R')

data(HI, package = "Ecdat")
str(HI)
head(HI)
with(HI[HI$husby > 98 & HI$husby < 102,], table(husby))


# Figure 8.1
library(reshape2)
HIvs <- c("whrswk", "experience", "husby", "wght")
HIs <- melt(HI[, HIvs], value.name = "HIx",
            variable.name = "HIvars")
head(HIs)
ggplot(HIs, aes(HIx)) + geom_histogram() +
  facet_wrap(~ HIvars, scales = "free") +
  xlab("") + ylab("")

# Figure 8.2
uniqv <- function(x) length(unique(x)) < 20
vcs <- names(HI)[sapply(HI, uniqv)]
par(mfrow = n2mfrow(length(vcs)))
relativeWeight <- with(HI, wght/sum(as.numeric(wght))*100)
for(v in vcs) 
  barplot(tapply(relativeWeight, HI[[v]], sum), main = v)

# Figure 8.3
data(Boston, package="MASS")
par(mfrow=c(1,2))
for (i in c("chas", "rad")) {
  barplot(table(Boston[, i]),
          main=(paste("Barchart of", i)))
}

# Figure 8.4
vs1 <- !(names(Boston) %in% c("chas","rad"))
grs <- n2mfrow(sum(as.numeric(vs1)))
par(mfrow=grs)
for (i in names(Boston)[vs1]) {
  hist(Boston[,i], col="grey70", xlab="", ylab="",
       main=(paste("Histogram of", i)))
}

# Figure 8.5
plot(Boston, pch=16)

# Figure 8.6
data(Boston, package="MASS")
par(mfrow=c(1,1), mar=c(3.1, 1.1, 2.1, 1.1))
MASS::parcoord(Boston)

# Figure 8.7
library(gplots)
heatmap.2(as.matrix(Boston), scale="column", trace="none")
