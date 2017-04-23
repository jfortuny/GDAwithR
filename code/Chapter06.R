# Chapter 5
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
ggparcoord(data = food1, columns=c(1:5, 7), 
           scale="uniminmax", alphaLines=0.2) + xlab("") + ylab("")
