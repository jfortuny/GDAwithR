# Chapter 4

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

