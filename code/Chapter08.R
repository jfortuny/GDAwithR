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

# Figure 8.8
data(Boston, package="MASS")
par(mar=c(1.1, 1.1, 1.1, 1.1))
palette(rainbow(14, s = 0.6, v = 0.75))
stars(Boston[1:4,], labels=NULL, draw.segments = TRUE)

# Figure 8.9
stars(Boston, labels=NULL, draw.segments = TRUE)

# Figure 8.10
data(foster, package="HSAUR2")
mosaic(~litgen+motgen, data=foster)

# Figure 8.11
ggplot(data=foster, aes(motgen)) + geom_bar() + 
  facet_grid(litgen~ .) + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,6,3)) + 
  labs(title="litter genotype by mother's genotype")

# Figure 8.12
data(barley, package="lattice")
head(barley)
dotplot(site ~ yield |variety , data = barley,
        groups = year, columns=2, pch=16, col=c("red","blue"),
        key = list(text=list(levels(barley$year)),
                   points = list(pch=16, col=c("red", "blue"))),
        xlab = "Barley Yield (bushels/acre) ", ylab=NULL,
        main="Barley Yields by Site for ten Varieties")

# Figure 8.13
data(uniranks, package="GDAdata")
names(uniranks)[c(5, 6, 8, 9, 10, 11, 13)] <- c("AvTeach",
                                                "NSSTeach", "SpendperSt", "StudentStaffR",
                                                "Careers", "VAddScore", "NSSFeedb")
library(reshape2)
ur2 <- melt(uniranks[, c(3, 5:13)], id.vars="UniGroup",
            variable.name="uniV", value.name="uniX")
ggplot(ur2, aes(uniX)) + geom_histogram() + xlab("") +
  ylab("") + facet_grid(UniGroup~uniV, scales = "free_x")
