setwd("~/GitHub/Bluff-Lake-Project/_analysis/Fish")
library(vegan)
library(ggplot2)

Zone1<-read.csv("_dat/Accumulation/SpeciesZone1.csv")
Zone2<-read.csv("_dat/Accumulation/SpeciesZone2.csv")
Zones<-read.csv("_dat/Accumulation/SpeciesZonesCombined.csv")
Fyke<-read.csv("_dat/Accumulation/SpeciesFykeNN.csv")
FykeZ1<-read.csv("_dat/Accumulation/SpeciesFykeNNZ1.csv")
FykeZ2<-read.csv("_dat/Accumulation/SpeciesFykeNNZ2.csv")
Gillnet<-read.csv("_dat/Accumulation/SpeciesGillnetNN.csv")
GillnetZ1<-read.csv("_dat/Accumulation/SpeciesGillnetNNZ1.csv")
GillnetZ2<-read.csv("_dat/Accumulation/SpeciesGillnetNNZ2.csv")
set.seed(123)

#Sampling Days----
#Number of Days Sampled, suite of gears (3 fykes, 1 gillnet)
sp9 <- specaccum(Zones, "random", permutations = 1000, gamma="boot")
specpool(Zones, smallsample = T)
plot(sp9, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp9, col="yellow", add=TRUE, pch="+")

#Zone 1----
#Number of Days Sampled, suite of gears (3 fykes, 1 gillnet)
sp3 <- specaccum(Zone1, "random", permutations = 1000, gamma="boot")
specpool(Zone1, smallsample = T)
plot(sp3, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp3, col="yellow", add=TRUE, pch="+")
#Fykes by net night Zone 1
sp4 <- specaccum(FykeZ1, "random", permutations = 1000, gamma="boot")
specpool(FykeZ1, smallsample = T)
plot(sp4, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp4, col="yellow", add=TRUE, pch="+")
#Gillnets by net night Zone 1
sp5 <- specaccum(GillnetZ1, "random", permutations = 1000, gamma="boot")
specpool(GillnetZ1, smallsample = T)
plot(sp5, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp5, col="yellow", add=TRUE, pch="+")

#Zone 2----
#Number of Days Sampled, suite of gears (3 fykes, 1 gillnet)
sp6 <- specaccum(Zone2, "random", permutations = 1000, gamma="boot")
specpool(Zone2, smallsample = T)
plot(sp6, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp6, col="yellow", add=TRUE, pch="+")
#Fykes by net night Zone 2
sp7 <- specaccum(FykeZ2, "random", permutations = 1000, gamma="boot")
specpool(FykeZ2, smallsample = T)
plot(sp7, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp7, col="yellow", add=TRUE, pch="+")
#Gillnets by net night Zone 2
sp8 <- specaccum(GillnetZ2, "random", permutations = 1000, gamma="boot")
specpool(GillnetZ2, smallsample = T)
plot(sp8, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp8, col="yellow", add=TRUE, pch="+")


#Gillnet----
#Gillnets by net night, all zones combined
sp1 <- specaccum(Gillnet, "random", permutations = 1000, gamma="boot")
specpool(Gillnet, smallsample = T)
summary(sp1)
plot(sp1,  col="black", lwd=2, add=T)
boxplot(sp1, col="blue", add=TRUE, pch="+")
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp1, "arrh")
plot(mods, col="black", lty=1)
boxplot(sp1, col = "white", border = "black", lty=1, cex=0.3, add= TRUE)
pool <- poolaccum(Gillnet,permutations = 999, minsize = 3)
summary(pool, display = "jack2")
plot(pool)

#Fyke----
#Fyke nets by net night, all zones combined
sp2 <- specaccum(Fyke, "random", permutations = 1000, gamma="boot")
specpool(Fyke, smallsample = T)
plot(sp2, col="blue", lwd=2)
boxplot(sp2, col="yellow", add=TRUE, pch="+")

#Plots comparing gears
dat <- data.frame(Sites=sp1$sites, Richness=sp1$richness, SD=sp1$sd)
dat2<- data.frame(Sites=sp2$sites, Richness=sp2$richness, SD=sp2$sd)
ggplot() +   theme_classic()+ geom_line(aes(x = dat$Sites, y=dat$Richness))+ 
  geom_errorbar(aes(x=dat$Sites,ymin=(dat$Richness-2*dat$SD),
                    ymax=(dat$Richness+2*dat$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat2$Sites, y=dat2$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat2$Sites,ymin=(dat2$Richness-2*dat2$SD),
                    ymax=(dat2$Richness+2*dat2$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Nets Deployed", y="Number of Species")+ylim(0,20)

#Plots comparing Zones
dat3 <- data.frame(Sites=sp3$sites, Richness=sp3$richness, SD=sp3$sd)
dat4 <- data.frame(Sites=sp4$sites, Richness=sp4$richness, SD=sp4$sd)
dat5 <- data.frame(Sites=sp5$sites, Richness=sp5$richness, SD=sp5$sd)
dat6 <- data.frame(Sites=sp6$sites, Richness=sp6$richness, SD=sp6$sd)
dat7 <- data.frame(Sites=sp7$sites, Richness=sp7$richness, SD=sp7$sd)
dat8 <- data.frame(Sites=sp8$sites, Richness=sp8$richness, SD=sp8$sd)
dat9 <- data.frame(Sites=sp9$sites, Richness=sp9$richness, SD=sp9$sd)
#Number of Days Sampled, suite of gears (3 fykes, 1 gillnet)
ggplot() +   theme_classic()+ geom_line(aes(x = dat9$Sites, y=dat9$Richness))+ 
  geom_errorbar(aes(x=dat9$Sites,ymin=(dat9$Richness-2*dat9$SD),
                    ymax=(dat9$Richness+2*dat9$SD)), color="black", width=0.5)+
  labs(x="Days Sampled", y="Number of Species")+ylim(0,25)
#Number of Days Sampled, suite of gears (3 fykes, 1 gillnet)
ggplot() +   theme_classic()+ geom_line(aes(x = dat3$Sites, y=dat3$Richness))+ 
  geom_errorbar(aes(x=dat3$Sites,ymin=(dat3$Richness-2*dat3$SD),
                    ymax=(dat3$Richness+2*dat3$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat6$Sites, y=dat6$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat6$Sites,ymin=(dat6$Richness-2*dat6$SD),
                    ymax=(dat6$Richness+2*dat6$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Days Sampled", y="Number of Species")+ylim(0,20)
#Fykes by Zone
ggplot() +   theme_classic()+ geom_line(aes(x = dat4$Sites, y=dat4$Richness))+ 
  geom_errorbar(aes(x=dat4$Sites,ymin=(dat4$Richness-2*dat4$SD),
                    ymax=(dat4$Richness+2*dat4$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat7$Sites, y=dat7$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat7$Sites,ymin=(dat7$Richness-2*dat7$SD),
                    ymax=(dat7$Richness+2*dat7$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Nets Deployed", y="Number of Species")+xlim(0,12.5)
#Gillnets by Zone
ggplot() +   theme_classic()+ geom_line(aes(x = dat5$Sites, y=dat5$Richness))+ 
  geom_errorbar(aes(x=dat5$Sites,ymin=(dat5$Richness-2*dat5$SD),
                    ymax=(dat5$Richness+2*dat5$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat8$Sites, y=dat8$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat8$Sites,ymin=(dat8$Richness-2*dat8$SD),
                    ymax=(dat8$Richness+2*dat8$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Nets Deployed", y="Number of Species")
#Zone 1 by gear
ggplot() +   theme_classic()+ geom_line(aes(x = dat5$Sites, y=dat5$Richness))+ 
  geom_errorbar(aes(x=dat5$Sites,ymin=(dat5$Richness-2*dat5$SD),
                    ymax=(dat5$Richness+2*dat5$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat4$Sites, y=dat4$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat4$Sites,ymin=(dat4$Richness-2*dat4$SD),
                    ymax=(dat4$Richness+2*dat4$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Nets Deployed", y="Number of Species")+xlim(0,12.5)
#Zone 2 by gear
ggplot() +   theme_classic()+ geom_line(aes(x = dat8$Sites, y=dat8$Richness))+ 
  geom_errorbar(aes(x=dat8$Sites,ymin=(dat8$Richness-2*dat8$SD),
                    ymax=(dat8$Richness+2*dat8$SD)), color="black", width=0.5)+
  geom_line(aes(x = dat7$Sites, y=dat7$Richness), lty=2)+ 
  geom_errorbar(aes(x=dat7$Sites,ymin=(dat7$Richness-2*dat7$SD),
                    ymax=(dat7$Richness+2*dat7$SD)), 
                lty= 2,color="black", width=0.5)+
  labs(x="Nets Deployed", y="Number of Species")+xlim(0,12.5)


