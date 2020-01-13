setwd("~/GitHub/Bluff-Lake-Project/_analysis/Fish")
install.packages("vegan")
install.packages("permute")
install.packages("lattice")
library("vegan")
#Example 1
#rarefy(x, sample, se = FALSE, MARGIN = 1)
#rrarefy(x, sample)
#drarefy(x, sample)
#rarecurve(x, step = 1, sample, xlab = "Sample Size", ylab = "Species",
#          label = TRUE, col, lty, ...)
#rareslope(x, sample)

#Example 2
#data(BCI)
#S <- specnumber(BCI) # observed number of species
#(raremax <- min(rowSums(BCI)))
#Srare <- rarefy(BCI, raremax)
#plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
#abline(0, 1)
#rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

library(vegan)
spp<-read.csv("SPP.csv")
S <- specnumber(spp) # observed number of species
(raremax <- min(rowSums(spp)))
Srare <- rarefy(spp, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

data(spp)
sp1 <- specaccum(spp)
sp2 <- specaccum(spp, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)
