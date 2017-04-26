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
