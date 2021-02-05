setwd("~/GitHub/Bluff-Lake-Project/_analysis")

library(tidyverse)

dat<-read.csv("_do-outputs/combos-fast.csv")
dat$value <- paste(dat$tempC, dat$DO_dusk, sep="_")


ggplot(dat, aes( elevation,Vol6.5/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 6.5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10)

ggplot(dat, aes( elevation,Vol6/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 6)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

ggplot(dat, aes( elevation,Vol5.5/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 5.5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10)

ggplot(dat, aes( elevation,Vol5/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

ggplot(dat, aes( elevation,Vol4.5/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 4.5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

ggplot(dat, aes( elevation,Vol4/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 4)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

ggplot(dat, aes( elevation,Vol3.5/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 3.5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

ggplot(dat, aes( elevation,Vol3/1000000, color=value)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 3)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

prob<-dat%>%group_by(tempC,elevation)%>%summarize(mean=mean(Vol4.5))


ggplot(prob, aes(elevation,mean/1000000, group=tempC, color=tempC)) + geom_line() + 
  labs(y = "Volume", x = "Elevation", title = 4.5)+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10) 

