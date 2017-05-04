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

# 7.3
data(Alligator, package="vcdExtra")
head(Alligator)
Alg1a <- aggregate(count~food, data=Alligator, sum)
ggplot(data = Alg1a, aes(food, count)) + geom_bar(stat = "identity")

# Figure 7.3
data(Alligator, package="vcdExtra")
head(Alligator)
Alg1a <- Alligator
levels(Alg1a$lake) <- abbreviate(levels(Alg1a$lake),3)
levels(Alg1a$size) <- abbreviate(levels(Alg1a$size),3)
levels(Alg1a$food) <- abbreviate(levels(Alg1a$food),2)
par(mfrow=c(2,2), mar=c(4,4,0.1,0.1))
mosaicplot(xtabs(count~lake, data = Alg1a), main="")
mosaicplot(xtabs(count~lake + sex, data = Alg1a), main="")
mosaicplot(xtabs(count~lake + sex + size, data = Alg1a), main="")
mosaicplot(xtabs(count~lake + sex + size + food, data = Alg1a), main="")

# Figure 7.4
pairs(xtabs(count ~ ., Alligator))

# Figure 7.5
doubledecker(xtabs(count ~ lake + sex, data = Alligator),
             gp = gpar(fill = c("grey90", "steelblue")))
doubledecker(xtabs(count ~ food + size, data = Alligator),
             gp = gpar(fill = c("grey90", "tomato")))

# Figure 7.6
Alg2 <- aggregate(count~sex+food+size, data = Alligator, sum)
ggplot(data = Alg2, aes(food, count, fill=sex)) + 
  geom_bar(stat = "identity") +
  facet_grid(size~sex) +
  theme(legend.position = "none")

# Figure 7.7
ggplot(Alligator, aes(food, count, fill=sex, color=lake)) +
  geom_bar(stat = "identity") +
  facet_grid(lake ~ sex + size) +
  theme(legend.position="none")

# Figure 7.8
library(seriation)
data(Zoo, package = "seriation")
str(Zoo)
mosaic(table(select(Zoo, c(hair, eggs:backbone))))
# Fluctuation Diagram
fluctile(table(select(Zoo, c(hair, eggs:backbone))),
         label=FALSE)

# Figure 7.9
data(Arthritis)
str(Arthritis)
Arthritis <- within(Arthritis,
                    Treatment1 <- factor(Treatment, 
                                         levels=levels(Treatment)[c(2,1)]))
mosaic(Improved ~ Treatment1 + Sex, data = Arthritis,
       direction = c("v", "v", "h"),
       zero_size = 0)

# Figure 7.10
Allig3 <- aggregate(count~food + lake, data = Alligator, sum)
head(Allig3)
ggplot(Allig3, aes(food, count)) +
  geom_bar(stat="identity") +
  facet_grid(lake ~ .)

# Figure 7.11
# Top
data(housing, package="MASS")
head(housing)
mosaic(xtabs(Freq ~ Cont + Type + Infl + Sat, data = housing),
       direction = c("h", "v", "v", "h"), 
       gp = gpar(fill = c("grey", "grey","red")),
       spacing = spacing_highlighting)
# Bottom
rmb(formula = ~Type+Cont+Infl+Sat, data = housing, cat.ord = 3,
    spine = TRUE, freq.trans = "const")

# Figure 7.12
# Top
data(housing, package="MASS")
rmb(formula = ~Type+Infl+Cont+Sat, data = housing,
    col.vars = c(FALSE,TRUE,TRUE,FALSE),
    label.opt = list(abbrev = 3, yaxis=FALSE))
# Bottom
rmb(formula = ~Type+Infl+Cont+Sat,  data = housing,
    eqwidth=TRUE, col.vars = c(FALSE,TRUE,TRUE,FALSE),
    label.opt = list(abbrev = 3, yaxis=FALSE))
