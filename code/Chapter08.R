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
