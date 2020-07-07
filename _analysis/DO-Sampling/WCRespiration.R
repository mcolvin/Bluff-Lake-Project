library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

#Water Respiration by Sampling Location----
Dark<-read.csv("DO-Sampling/_dat/Data/CombinedResp.csv")
Dark$StartTime<-as.POSIXct(Dark$StartTime, format="%m/%d/%Y %H:%M")
Dark$EndTime<-as.POSIXct(Dark$EndTime, format="%m/%d/%Y %H:%M")
WCresp<-Dark%>%dplyr::group_by(ï..Site, Collection.Depth)%>%dplyr::summarize(mWCresp=(sum(WCresp)/n()),var=var(WCresp, na.rm=T))
WCresp$CIhigh<-WCresp$mWCresp+1.96*sqrt(WCresp$var)
WCresp$CIlow<-WCresp$mWCresp-1.96*sqrt(WCresp$var)

ggplot(Dark, aes(x =as.factor(ï..Site), y=WCresp))+
  geom_boxplot() + theme_classic()+ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site") 
ggplot(Dark, aes(x =as.factor(Zone), y=WCresp))+
  geom_boxplot() + theme_classic()+ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")
ggplot(Dark, aes(x=c("Bluff Lake"),y=WCresp))+
  geom_boxplot() + theme_classic()+ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")

ggplot(WCresp, aes(x =as.factor(ï..Site), y=mWCresp))+
  geom_point(aes(shape = as.character(Collection.Depth))) + 
  theme_classic()+ylim(0,0.12)+theme(legend.position = c(0.9, .9))+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site", shape="Collection Depth", linetype="Collection Depth") +
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, linetype=as.character(Collection.Depth)), color="black", width=0.1)
#Water Respiration by Sampling Location (Ignoring Collection Depth)----
WCresp<-Dark%>%dplyr::group_by(ï..Site)%>%dplyr::summarize(mWCresp=(sum(WCresp)/n()),var=var(WCresp, na.rm=T))
WCresp$CIhigh<-WCresp$mWCresp+1.96*sqrt(WCresp$var)
WCresp$CIlow<-WCresp$mWCresp-1.96*sqrt(WCresp$var)

ggplot(WCresp, aes(x =as.factor(ï..Site), y=mWCresp))+geom_point()+ 
  theme_classic()+ ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")+
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), color="black", width=0.1)

#Water Respiration by Zone----
WCresp<-Dark%>%dplyr::group_by(Zone)%>%dplyr::summarize(mWCresp=(sum(WCresp)/n()),var=var(WCresp, na.rm=T))
WCresp$CIhigh<-WCresp$mWCresp+1.96*sqrt(WCresp$var)
WCresp$CIlow<-WCresp$mWCresp-1.96*sqrt(WCresp$var)

ggplot(WCresp, aes(x =as.factor(Zone), y=mWCresp))+geom_point()+ 
  theme_classic()+ ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")+
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), color="black", width=0.1)

#Water Respiration remove Site 21----
Dark2<-filter(Dark,ï..Site!=21)
WCresp<-Dark2%>%dplyr::group_by(Zone)%>%dplyr::summarize(mWCresp=(sum(WCresp)/n()),var=var(WCresp, na.rm=T))
WCresp$CIhigh<-WCresp$mWCresp+1.96*sqrt(WCresp$var)
WCresp$CIlow<-WCresp$mWCresp-1.96*sqrt(WCresp$var)

ggplot(WCresp, aes(x =as.factor(Zone), y=mWCresp))+geom_point()+ 
  theme_classic()+ ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")+
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), color="black", width=0.1)

t.test(WCresp~Zone, Dark)

#Water Respiration
WCresp<-Dark%>%dplyr::summarize(mWCresp=mean(WCresp),var=var(WCresp, na.rm=T), Lake=c("Bluff Lake"))
WCresp$CIhigh<-WCresp$mWCresp+1.96*sqrt(WCresp$var)
WCresp$CIlow<-WCresp$mWCresp-1.96*sqrt(WCresp$var)

ggplot(WCresp, aes(x =as.factor(Lake), y=mWCresp))+geom_point()+ 
  theme_classic()+ ylim(0,0.12)+
  labs(y= "Water Column Respiration mg/L/hr", x = "Collection Site")+
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), color="black", width=0.1)
