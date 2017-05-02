# Chapter07
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Figure 7.1
library(vcd)
doubledecker(Survived ~ Sex, data = Titanic,
             gp = gpar(fill = c("grey90", "red")))
doubledecker(Survived ~ Class, data = Titanic,
             gp = gpar(fill = c("grey90", "red")))

# Figure 7.2
doubledecker(Survived ~ Sex + Class, data = Titanic,
             gp = gpar(fill = c("grey90", "red")))
