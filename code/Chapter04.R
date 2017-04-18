# Chapter 4
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Figure 4.1
data(btw2009, package = "flexclust")
summary(btw2009)
btw2009 <- within(btw2009, stateA <- state)
btw2009 <- within(btw2009, levels(stateA) <- c("BW", "BY", "BE",
                                               "BB", "HB", "HH", "HE", "MV", "NI", "NW",
                                               "RP", "SL", "SN", "ST", "SH", "TH"))
Voters <- with(btw2009, size <- tapply(eligible, stateA, sum))
Bundesland <- rownames(Voters)
btw9s <- data.frame(Bundesland, Voters)
btw9s$EW <- c("West")
btw9s[c("BB", "BE", "MV", "SN", "ST", "TH"), "EW"] <- "East"
ls <- with(btw9s, Bundesland[order(EW, -Voters)])
btw9s <- within(btw9s, State1 <- factor(Bundesland, levels=ls))
b1 <- ggplot(btw9s, aes(Bundesland, Voters/1000000)) +
  geom_bar(stat="identity") +
  ylab("Voters (millions)")
b2 <- ggplot(btw9s, aes(reorder(Bundesland, -Voters),
                        Voters/1000000)) + geom_bar(stat="identity")  +
  xlab("Bundesland") + ylab("Voters (millions)")
b3 <- ggplot(btw9s, aes(State1, Voters/1000000)) +
  geom_bar(stat="identity")  + xlab("Bundesland") +
  ylab("Voters (millions)")
plot_grid(b1, b2, b3, nrow = 3, ncol = 1)

# Figure 4.2
data(Fleiss93, package="meta")
Fleiss93 <- within(Fleiss93, {
  total <- n.e + n.c
  st <- reorder(study, -(total)) })
ggplot(Fleiss93, aes(st, total)) + geom_bar(stat="identity") +
  xlab("") + ylab("") + ylim(0,20000)

# Combining small analyses
Fleiss93 <- within(Fleiss93, {st1 <- as.character(study)})
Fleiss93$st1[Fleiss93$total < 2000] <- "Rest"
ggplot(Fleiss93, aes(st1, total)) + geom_bar(stat="identity") +
  xlab("") + ylab("") + ylim(0,20000)

# Figure 4.3
data(anorexia, package="MASS")
ggplot(anorexia, aes(Treat)) + geom_bar() +  xlab("Treatment")
# in tabular form
with(anorexia, table(Treat))

# Figure 4.4
Titanic1 <- data.frame(Titanic)
p <- ggplot(Titanic1, aes(weight=Freq)) +
  ylab("") + ylim(0,2250)
cs <- p + aes(Class) + geom_bar(fill="blue")
sx <- p + aes(Sex) + geom_bar(fill="green")
ag <- p + aes(Age) + geom_bar(fill="tan2")
su <- p + aes(Survived) + geom_bar(fill="red")
#grid.arrange(cs, sx, ag, su, nrow=1, widths=c(3, 2, 2, 2))
plot_grid(cs, sx, ag, su, nrow = 1, ncol = 4)

# Figure 4.5
Party <- c("Fine Gael", "Labour", "Fianna Fail",
           "Sinn Fein", "Indeps", "Green", "Don't know")
nos <- c(181, 51, 171, 119, 91, 4, 368)
IrOP <- data.frame(Party, nos)
IrOP <- within(IrOP, {
  percwith <- nos/sum(nos)
  percnot <- nos/sum(nos[-7])})
par(mfrow=c(2,1), mar = c(2.1, 2.1, 2.1, 2.1))
with(IrOP, pie(percwith, labels=Party, clockwise=TRUE,
               col=c("blue", "red", "darkgreen", "black",
                     "grey", "lightgreen", "white"), radius=1))
with(IrOP, pie(percnot[-7], labels=Party, clockwise=TRUE,
               col=c("blue", "red", "darkgreen", "black",
                     "grey", "lightgreen"), radius=1))
par(mfrow=c(1,1))

# Figure 4.6
data("BEPS", package="effects")
a1 <- ggplot(BEPS, aes(factor(Hague))) +
  geom_bar(fill="blue") + ylab("") + 
  xlab("Hague (Conservative)") + ylim(0, 900)
a2 <- ggplot(BEPS, aes(factor(Blair))) +
  geom_bar(fill="red") + ylab("") + 
  xlab("Blair (Labour)") + ylim(0, 900)
a3 <- ggplot(BEPS, aes(factor(Kennedy))) +
  geom_bar(fill="yellow") + ylab("") + 
  xlab("Kennedy (Liberal)") + ylim(0, 900)
#grid.arrange(a1, a2, a3, nrow=1)
plot_grid(a1, a2, a3, nrow = 1)

# Figure 4.7
b1 <- ggplot(BEPS, aes(factor(political.knowledge))) +
  geom_bar(fill="tan2")  + coord_flip() + ylab("") +
  xlab("Knowledge of policies on Europe")
b2 <- ggplot(BEPS, aes(factor(Europe))) +
  geom_bar(fill="lightgreen") + ylab("") + 
  xlab("Attitudes to European integration")
#grid.arrange(b1, b2, nrow=1, widths=c(4, 8))
plot_grid(b1, b2, nrow = 1, ncol = 2)

# Figure 4.8
data(survey, package="MASS")
s1 <- ggplot(survey, aes(Sex)) + geom_bar() + ylab("")
s2 <- ggplot(survey, aes(W.Hnd)) + geom_bar() +
  xlab("Writing hand") + ylab("")
s3 <- ggplot(survey, aes(Fold)) + geom_bar() + 
  xlab("Folding arms: arm on top") + ylab("")
s4 <- ggplot(survey, aes(Clap))  + geom_bar() + 
  xlab("Clapping: hand on top") + ylab("")
survey <- within(survey,
                 ExerN <- factor(Exer,
                                 levels=c("None", "Some", "Freq")))
s5 <- ggplot(survey, aes(ExerN))  + geom_bar() +
  xlab("Exercise") +  ylab("")
s6 <- ggplot(survey, aes(M.I))  + geom_bar() +
  xlab("Height units") + ylab("")
survey <- within(survey,
                 SmokeN <- factor(Smoke,
                                  levels=c("Never", "Occas", "Regul", "Heavy")))
s7 <- ggplot(survey, aes(SmokeN))  + geom_bar() + 
  xlab("Smoking") + ylab("")
#grid.arrange(s1, s2, s3, s4, s5, s6, s7, ncol=3)
plot_grid(s1,s2,s3,s4,s5,s6,s7,nrow = 3,ncol = 3)
