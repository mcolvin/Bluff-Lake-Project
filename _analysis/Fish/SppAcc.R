setwd("~/GitHub/Bluff-Lake-Project/_analysis/Fish")
library(vegan)
Zone1<-read.csv("_dat/Accumulation/SpeciesZone1.csv")
Zone2<-read.csv("_dat/Accumulation/SpeciesZone2.csv")
Zones<-read.csv("_dat/Accumulation/SpeciesZonesCombined.csv")
Fyke<-read.csv("_dat/Accumulation/SpeciesFykeNN.csv")
Gillnet<-read.csv("_dat/Accumulation/SpeciesGillnetNN.csv")


  
sp1 <- specaccum(Gillnet, "random", permutations = 1000)
sp1
summary(sp1)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp1, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp2, "arrhenius")
coef(mod1)
fitted(mod1)
plot(mod1, col="grey")
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp1, "arrh")
coef(mods)
boxplot(sp2, col = "white", border = "black", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)
