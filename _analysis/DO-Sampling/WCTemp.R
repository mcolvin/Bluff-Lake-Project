library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

Dark<-read.csv("DO-Sampling/_dat/Data/DO_Map_June13_14_2020.csv")
Dark$Time1<-as.POSIXct(Dark$Time1, format="%m/%d/%Y %H:%M")
Dark$Temp1<-Dark$Temp1..C.
Dark$Group<-paste(Dark$Site,Dark$Time,sep="-")


ggplot(Dark, aes(x =as.factor(Group), y=Temp1))+
  geom_boxplot() + theme_classic()+ 
  labs(y= "Water Column Temp (C) at 0.6 m", x = "Collection Site-Collection Time")
ggplot(Dark, aes(x =as.factor(Group), y=DO.2..mg.L.))+
  geom_boxplot() + theme_classic()+ 
  labs(y= "Water Column DO (mg/L) at 0.6 m", x = "Collection Site-Collection Time")

Day<-Dark%>%group_by(Group)%>%summarize(T1=mean(Temp1))

#Water column DO
WC<-read.csv("DO-Sampling/_dat/Data/June13_14_2020profile.csv")
WC$Time1<-as.POSIXct(WC$Time1, format="%m/%d/%Y %H:%M")
WC$Time2<-strftime(WC$Time1, "%m/%d/%Y")
WC$ï..WP<-as.factor(WC$ï..WP)
WC$Time<-as.factor(WC$Time)
WC$Site<-as.factor(WC$Site)
ggplot(WC, aes(x =Depth.measured, y=DO...mg.L., 
               group=as.factor(ï..WP), color=Site))+
  facet_wrap(~Time2)+
  geom_line(size=1) + theme_classic()+ 
  labs(y= "Water Column DO (mg/L)", x = "Collection Depth")
#Water column Temp
ggplot(WC, aes(x =Depth.measured, y=Temp..C., 
               group=as.factor(ï..WP), color=Site))+
  facet_wrap(~Time2)+
  geom_line(size=1) + theme_classic()+ 
  labs(y= "Water Column Temp (C)", x = "Collection Depth")

Profile<-read.csv("DO-Sampling/_dat/Data/RepeatedProfile.csv")
ggplot(Profile, aes(x =Depth, y=DO, 
               group=as.factor(ï..Trial)))+
  geom_line(size=1) + theme_classic()+ 
  labs(y= "Water Column DO (mg/L)", x = "Collection Depth")
ggplot(Profile, aes(x =Depth, y=Temp, 
                    group=as.factor(ï..Trial)))+
  geom_line(size=1) + theme_classic()+ 
  labs(y= "Water Column Temp (C)", x = "Collection Depth")
