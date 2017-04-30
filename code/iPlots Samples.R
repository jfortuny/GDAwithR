# iPlots testing
source('~/R Work/GDAwithR/code/StartLibraries.R')
library(iplots)

# Mosaic Plot
library(MASS)
data("Cars93")
head(Cars93)
imosaic(Cars93[, c("AirBags", "Cylinders", "Origin")])

# Barcharts
ibar(Cars93$Cylinders)

# Parallel Coordinate Plots
ipcp(Cars93[c(4:8,12:15,17,19:25)])

# Parallel Box Plot
ibox(Cars93[4:6])

# Boxplot y by x
ibox(Cars93$Horsepower, Cars93$Cylinders)

# Scatterplot
iplot(Cars93$Horsepower, Cars93$MPG.city)
l <- lowess(Cars93$Horsepower,Cars93$ MPG.city)
ilines(l)
iobj.rm()
l <- lowess(Cars93$Horsepower,Cars93$ MPG.city, f = 0.5)
ilines(l)
iplot.opt(ptDiam=5, col=unclass(Cars93$Cylinders))

# Histograms
ihist(Cars93$Horsepower)

# Selections
iset.selected()
sum(sign(iset.selected()))/length(Cars93$ Horsepower)

iset.select(Cars93$Horsepower >= 240)
Cars93[iset.selected(),]
iset.select(Cars93$Horsepower > 0)

# Color Brush
ibar(Cars93$Cylinders)
iplot(Cars93$MPG.city, Cars93$MPG.highway)

# iObjects
iplot(Cars93$EngineSize, Cars93$Horsepower)
# select some points
subs <- iset.selected()
iabline(lm(Horsepower~EngineSize, data = Cars93[subs,]))

# Conventions
