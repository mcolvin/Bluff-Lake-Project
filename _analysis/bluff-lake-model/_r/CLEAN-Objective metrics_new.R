dat <- read.csv("_dat/CompleteMap.csv")
library(lubridate)
library(tidyverse)
library(scales)
library(ggplot2)
library(egg)
# ---- Lake Volume
# one point=4m^2
# volume<-NA
# boards<-c(0:17)
# elevation<-round(66.568+(0.2032*boards),2)
# for(i in 1:length(elevation)){
#   Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
#   Z <- c((elevation[i]-Z))
#   volume[i]<-sum(Z*4)
# }
# 
# data<-data.frame(volume, elevation)
# ggplot(data,aes(elevation, (volume/1000000))) + geom_line()+labs(y = bquote('Water Volume'~('million'~m^3)), x = "Water Surface Elevation (m)")+ theme_classic()
# 
# EL_Vol<-approxfun(elevation, volume, rule=2)
# Vol_EL<-approxfun(volume, elevation, rule=2)
# 


#
#dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
# ---- Waterbirds
# areas must be less than 20 cm in depth
WB<-NA
# boards<-c(0:17)
#elevation<-round(66.568+(0.2032*boards),2)
for(i in 1:length(elevation)){
  Z <- length(which(dat$POINT_Z > (elevation[i]-.20)))
  Z <- Z*4
  WB[i]<-sum(Z)/10000
}
data<-data.frame(WB, elevation)
WB2<-ggplot(data, aes(elevation, WB)) + geom_line() + 
  labs(y = "Hectares <0.20m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
  annotate(geom="text", x=64.5, y=425,size=3,label="B")

# ---- Waterfowl
# DED/m^2=0.4374
# Dry Areas
WF<-NA
#boards<-c(0:17)
#elevation<-round(66.568+(0.2032*boards),2)
for(i in 1:length(elevation)){
  Z <- length(which(dat$POINT_Z > (elevation[i])))
  Z <- Z*4*0.4374 #convert to DED per area
  WF[i]<-sum(Z)/1000000
}
data<-data.frame(WF, elevation)
WF2<-ggplot(data, aes(elevation, WF)) + geom_line() + 
  labs(y = "Duck Energy Days (million)", x = "Water Surface Elevation (m)")+   
  theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
  annotate(geom="text", x=64.5, y=2,size=3,label="A")

# ---- Fish
# Areas greater than 1 meter in depth
Fish<-read.csv("_dat/combos-fast2.csv")
Fish<-Fish%>%dplyr::group_by(elevation)%>%dplyr::summarize(Fish5=mean(Vol5))


Fish2<-ggplot(Fish, aes(elevation,Fish5/1000000)) + geom_line() + 
  labs(y = bquote('Water Volume'~('million'~m^3)), x = "Elevation")+   
  theme_classic()+theme(legend.position = "none")+ylim(0,10)+theme(axis.title.x=element_blank(), text = element_text(size=8))+
  annotate(geom="text", x=64.5, y=10,size=3,label="E") 

#### ---- Anglers
# For now, following fish model, Areas greater than 1 meter in depth
Adat<-read.csv("_dat/Anglers.csv")
Anglers<-NA
# boards<-c(0:17)
# elevation<-round(66.568+(0.2032*boards),2)
for(i in 1:length(elevation)){
  Z <- length(which(Adat$POINT_Z < (elevation[i]-1)))
  Z <- Z*4 
  Anglers[i]<-sum(Z)/10000
}
data<-data.frame(Anglers, elevation)
Bank<-ggplot(data, aes(elevation, Anglers)) + geom_line() + 
  labs(y = "Hectares >1m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
                          annotate(geom="text", x=64.5, y=7,size=3,label="C")

# Need to deliniate shorelines and establish "end" of boat ramp
#areas >.5m in depth
Bdat<-read.csv("_dat/Boat.csv")
Ramp<-NA
# boards<-c(0:17)
# elevation<-round(66.568+(0.2032*boards),2)
for(i in 1:length(elevation)){
  Z <- length(which(Bdat$POINT_Z < (elevation[i]-0.5)))
  Z <- Z*4 #convert to DED per area
  Ramp[i]<-sum(Z)
}
data<-data.frame(Ramp, elevation)
Boat<-ggplot(data, aes(elevation, Ramp)) + geom_line() + 
  labs(y = "Square meters >0.5m", x = "Water Surface Elevation (m)")+   
  theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
  annotate(geom="text", x=64.5, y=225,size=3,label="D")




grid.arrange(WF2, WB2, Bank, Boat,Fish2, ncol=3,
             bottom="Water Surface Elevation (m)")

dataframe<-data.frame(elevation, WB, WF, Anglers, Ramp)
dataframe<-cbind(dataframe, Fish=Fish$Fish5)


#incooporate timing to objective utility
DOY<-c(1:365)
datalist <- list()
for(i in 1:length(DOY)){
  dataframe$DOY<-DOY[i]
  datalist[[i]]<-dataframe
}
big_data1 <- do.call(rbind, datalist)
#write.csv(big_data1, "~/GitHub/Bluff-Lake-Project/_analysis/RawMetrics.csv")

dates<-data.frame(matrix(ncol = 2, nrow =365))
dates$DOY<-c(1:365)
dates$Month<-NA
for(i in 1:365){
  date<-as.Date(i, origin = "2016-01-01")
  Month<-month(as.POSIXlt(date, format="%d/%m/%Y"))
  dates$Month[i]<-Month
}
dates<-dates[,-c(1:2)]
#boating season
dates$Ramp<-ifelse(dates$DOY>=60 & dates$DOY<=304, 1, 0)
#fishing season weight-Boat
Bt<-read.csv("_dat/BoatAngEF.csv")
Total<-Bt%>%dplyr::group_by(Year)%>%summarise(sum(Total))
Total<-merge(Bt,Total)
Bt<-Total%>%dplyr::group_by(Year, Month)%>%summarise(normalized=sum(Total)/`sum(Total)`)
Bt<-Bt%>%dplyr::group_by(Month)%>%summarise(BTmean=mean(normalized))
Month<-c(1,2,11,12)
BTmean<-c(0,0,0,0)
d1<-data.frame(Month, BTmean)
Bt<-rbind(Bt,d1)
Bt$BTmean<-Bt$BTmean/sum(Bt$BTmean)
dates<-merge(dates,Bt)
#fishing season weight-Bank
Bk<-read.csv("_dat/BankAngEF.csv")
#sub in missing dates
Month<-c(1,1,2,2,11,11,12,12)
Total<-c(200,200,400,400,200,200,200,200)
Year<-c(2019,2020,2019,2020,2019,2020,2019,2020)
d1<-data.frame(Month, Total,Year)
Bk<-data.frame(Month=Bk$Month,Total=Bk$Total,Year=Bk$Year)
Bk<-rbind(Bk,d1)
Total<-Bk%>%dplyr::group_by(Year)%>%summarise(sum(Total))
Bk<-merge(Bk,Total)
Bk<-Bk%>%dplyr::group_by(Year, Month)%>%summarise(normalized=sum(Total)/`sum(Total)`)
Bk<-Bk%>%dplyr::group_by(Month)%>%summarise(BKmean=mean(normalized))
Bk$BKmean<- Bk$BKmean/sum(Bk$BKmean)
dates<-merge(dates,Bk)
#waterbird Seasonality
range <- c(1:12)
mean <- 6
sd <-3
dist <- dnorm(range, mean = mean, sd = sd)
df <- data.frame("Month" = range, "WB" = dist)
df$WB<-df$WB/sum(df$WB)
plot(df$WB~df$Month, main="waterbird")
dates<-merge(dates,df)
#Mississippi Growing Season
dates$WF<-ifelse(dates$DOY>=60 & dates$DOY<=304, 1, 0)



#Plot
library(scales)
dates$Ramp<-rescale(dates$Ramp, c(0,1))
dates$BTmean<-rescale(dates$BTmean, c(0,1))
dates$BKmean<-rescale(dates$BKmean, c(0,1))
dates$WF<-rescale(dates$WF, c(0,1))
dates$WB<-rescale(dates$WB, c(0,1))

names(dates)[3]<-"RampS"
names(dates)[4]<-"BoatS"
names(dates)[5]<-"BankS"
names(dates)[6]<-"WB_S"
names(dates)[7]<-"WF_S"

big_data1<-merge(big_data1, dates, by="DOY")


#Metric Functions
WBM<-approxfun(big_data1$elevation, big_data1$WB, rule=2,yleft=0,yright=1)
WFM <- approxfun(big_data1$elevation, big_data1$WF, rule=2,yleft=0,yright=1)
FishM <- approxfun(big_data1$elevation, big_data1$Fish, rule=2,yleft=0,yright=1)
AnglersM <- approxfun(big_data1$elevation, big_data1$Anglers, rule=2,yleft=0,yright=1)
RampM <- approxfun(big_data1$elevation, big_data1$Ramp, rule=2,yleft=0,yright=1)

# big_data1<-big_data1[order(big_data1$elevation),]
# 
# ggplot(big_data1,aes(DOY, WB, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, WF, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Fish, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Anglers, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Ramp, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# 
# 
# # dataframe2<-data.frame(elevation, WB, WF, Fish, Anglers, Ramp)
# # #incooporate timing to objective utility
# # DOY<-c(1:365)
# # datalist <- list()
# # for(i in 1:length(DOY)){
# #   dataframe2$DOY<-DOY[i]
# #   datalist[[i]]<-dataframe2
# # }
# # big_data2 <- do.call(rbind, datalist)
# # write.csv(big_data2, "~/GitHub/Bluff-Lake-Project/_analysis/RescaleMetrics.csv")
# 
# 
# #Form overall utility
# W<- c(.20,.23,.27,.3)
# big_data1$Utility<-(W[1]*((big_data1$Ramp*.5) + (big_data1$Anglers*.5))) + 
#   (W[2]*big_data1$Fish) + (W[3]*big_data1$WB) + (W[4]*big_data1$WF)
# 
# #no utility for empty lake
# big_data1$volume<-EL_2_Vol(big_data1$elevation)
# big_data1$Utility<-ifelse(big_data1$elevation<66.568, 0, big_data1$Utility)
# 
# ggplot(big_data1,aes(DOY, Utility, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "Day of Year")+ theme_classic()

