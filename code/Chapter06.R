# Chapter 6
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Figure 6.1
data(food, package="MMST")
head(food)
names(food) <- c("Fat", "Food.energy", "Carbohyd", "Protein",
                 "Cholest", "Wt", "Satur.Fat")
ggparcoord(data = food, columns = c(1:7), scale="uniminmax") +
  xlab("") + ylab("")

# Figure 6.2
food1 <- food/food$Wt
head(food1)
ggparcoord(data = food1, 
           columns=c(1:5, 7), 
           scale="uniminmax", 
           alphaLines=0.2) + xlab("") + ylab("")

# Figure 6.3
food1 <- within(food1,
                fatX <- factor(ifelse(Fat > 0.75, 1, 0)))
ggparcoord(data = food1[order(food1$fatX),],
           columns=c(1:5, 7), 
           groupColumn="fatX",
           scale="uniminmax") + xlab("") + ylab("")  +
  theme(legend.position = "none") + 
  coord_flip()

# Figure 6.4
ggplot(food1, aes(Protein, Carbohyd)) + geom_point()

# Figure 6.5
ggparcoord(iris, columns=1:4, groupColumn="Species")

# Figure 6.6
data("USArrests")
head(USArrests)
hcav <- hclust(dist(USArrests), method="ave")
clu3 <- cutree(hcav, k=3)
clus <- factor(clu3)
usa1 <- cbind(USArrests, clus)
head(usa1)
ggparcoord(usa1, columns=1:4, groupColumn="clus",
           scale="uniminmax", mapping = aes(size = 1)) +
  xlab("") +  ylab("") +
  theme(legend.position = "none")

ggparcoord(usa1, columns=1:4, groupColumn="clus",
           scale="globalminmax") +
  xlab("") +  ylab("") +
  theme(legend.position = "none")
ggparcoord(usa1, columns=1:4, groupColumn="clus",
           scale="uniminmax") +
  xlab("") +  ylab("") +
  theme(legend.position = "none")
ggscatmat(usa1, columns = 1:4)

# Figure 6.7
hcav2 <- hclust(dist(scale(USArrests)), method="ave")
clu32 <- cutree(hcav2, k=3)
clus2 <- factor(clu32)
usa2 <- cbind(USArrests, clus2)
ggparcoord(usa2, columns=1:4, groupColumn="clus2",
           scale="uniminmax", mapping = aes(size = 1)) +
  xlab("") +  ylab("") +
  theme(legend.position = "none")

ggparcoord(usa2, columns=1:4, groupColumn="clus2",
           scale="uniminmax") +
  xlab("") +  ylab("") +
  theme(legend.position = "none")

# Figure 6.8
library(reshape2); data(nass.corn, package="agridat")
head(nass.corn)
c1 <- melt(nass.corn, id=c("year", "state"))
head(c1)
c1 <- within(c1, StateV <- interaction(state, variable))
c2 <- dcast(c1, StateV~year)
head(c2)
ggparcoord(subset(c2[1:48,], c2[1:48,147]> 250000),
           columns=2:147, groupColumn="StateV",
           scale="globalminmax") + xlab("Year") + ylab("Acres") + 
  scale_x_discrete(breaks=seq(1865, 2015, 10)) +
  theme(legend.position = "none")

# Figure 6.9
ggparcoord(subset(c2[1:48,], c2[1:48,147]> 250000),
           columns=2:147, groupColumn="StateV",
           scale="globalminmax", boxplot=TRUE, alphaLines=0.5) +
  xlab("Year") + ylab("Acres") +
  scale_x_discrete(breaks=seq(1865, 2015, 10)) +
  theme(legend.position = "none")

# Figure 6.10
data(uniranks, package="GDAdata")
head(uniranks)
names(uniranks)[c(5, 6, 8, 10, 11, 13)] <- c("AvTeach",
                                             "NSSTeach", "SpendperSt", "Careers", "VAddScore", "NSSFeedb")
uniranks1 <- within(uniranks, StaffStu <- 1/(StudentStaffRatio))
ggparcoord(uniranks1, columns=c(5:8, 10:14),
           scale="uniminmax", alphaLines=1/3) +
  xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Figure 6.11
uniranks2 <- within(uniranks1,
                    Rus <- ifelse(UniGroup=="Russell", "Russell", "not"))
ggparcoord(uniranks2[order(uniranks2$Rus, decreasing=TRUE),],
           columns=c(5:8, 10:14), 
           order=c(5,12,8,9,14,6,13,7,11,10),
           groupColumn="Rus", scale="uniminmax") +
  xlab("") + ylab("") +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = c("grey","red"))

# 6.7 Options for parallel coordinate plots
# Alignment
library(reshape2); data(nass.corn, package="agridat")
head(nass.corn)
c1 <- melt(nass.corn, id=c("year", "state"))
head(c1)
c1 <- within(c1, StateV <- interaction(state, variable))
c2 <- dcast(c1, StateV~year)
head(c2)
str(c2)
mz <- as.data.frame(apply(c2[1:48,2:147], 2,
                          function(x) x - mean(x, na.rm = TRUE)))
StateV <- c2[1:48,1]
mzA <- as.data.frame(cbind(StateV, mz))
ggparcoord(mzA, columns = 2:147, scale = "globalminmax",
           groupColumn = "StateV") + 
  xlab("Year") + ylab("Acres") +
  scale_x_discrete(breaks=seq(1865,2015,10)) +
  theme(legend.position = "none")
