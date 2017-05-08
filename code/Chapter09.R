# Chapter09
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Figure 9.1
data(CHAIN, package="mi")
head(CHAIN)
par(mar=c(1.1, 4.1, 1.1, 2.1))
mi::missing.pattern.plot(CHAIN, y.order=TRUE, xlab="", main="")

# Figure 9.2
visna(CHAIN, sort="b")

# Figure 9.3
data(oly12, package="VGAMdata")
oly12a <- oly12
names(oly12a) <- abbreviate(names(oly12), 3)
visna(oly12a, sort="b")

# Figure 9.4
data(freetrade, package="Amelia")
freetrade <- within(freetrade, land1 <-
                      reorder(country, tariff, function(x) sum(is.na(x))))
fluctile(xtabs(is.na(tariff) ~ land1 + year, data=freetrade))

# Figure 9.5
data(Pima.tr2, package="MASS")
visna(Pima.tr2, sort="b")
