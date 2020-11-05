dat <- read.csv("Depth-Mapping/_dat/Bathymetry/CompleteMap.csv")
library(tidyverse)
library(scales)
# ---- Lake Volume
# one point=4m^2
volume<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2032*boards)
for(i in 1:length(elevation)){
  Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
  Z <- c((elevation[i]-Z))
  Z <- Z*4
  volume[i]<-sum(Z)
}

data<-data.frame(volume, elevation)
ggplot(data,aes(elevation, (volume/1000000))) + geom_line()+labs(y = bquote('Water Volume'~('million'~m^3)), x = "Water Surface Elevation (m)")+ theme_classic()

EL_Vol<-approxfun(elevation, volume, rule=2)
Vol_EL<-approxfun(volume, elevation, rule=2)



#
dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
# ---- Waterbirds
# areas must be less than 20 cm in depth
WB<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- length(which(dat$POINT_Z > (elevation[i]-.20)))
  Z <- Z*4
  WB[i]<-sum(Z)/10000
}
data<-data.frame(WB, elevation)
ggplot(data, aes(elevation, WB)) + geom_line() + 
  labs(y = "Hectares <0.20m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()

# ---- Waterfowl
# DED/m^2=0.4374
# Dry Areas
WF<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- length(which(dat$POINT_Z > (elevation[i])))
  Z <- Z*4*0.4374 #convert to DED per area
  WF[i]<-sum(Z)/1000000
}
data<-data.frame(WF, elevation)
ggplot(data, aes(elevation, WF)) + geom_line() + 
  labs(y = "Duck Energy Days (million)", x = "Water Surface Elevation (m)")+   
  theme_classic()

# ---- Fish
# Areas greater than 1 meter in depth
Fish<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- length(which(dat$POINT_Z < (elevation[i])))
  Z <- Z*4 #convert to DED per area
  Fish[i]<-sum(Z)/10000
}
data<-data.frame(Fish, elevation)
ggplot(data, aes(elevation, Fish)) + geom_line() + 
  labs(y = "Hectares >0m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()

#### ---- Anglers
# For now, following fish model, Areas greater than 1 meter in depth
Adat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Anglers.csv")
Anglers<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- length(which(Adat$POINT_Z < (elevation[i]-1)))
  Z <- Z*4 #convert to DED per area
  Anglers[i]<-sum(Z)/10000
}
data<-data.frame(Anglers, elevation)
ggplot(data, aes(elevation, Anglers)) + geom_line() + 
  labs(y = "Hectares >1m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()

# Need to deliniate shorelines and establish "end" of boat ramp
#areas >.5m in depth
Bdat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Boat.csv")
Ramp<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- length(which(Bdat$POINT_Z < (elevation[i]-0.5)))
  Z <- Z*4 #convert to DED per area
  Ramp[i]<-sum(Z)
}
data<-data.frame(Ramp, elevation)
ggplot(data, aes(elevation, Ramp)) + geom_line() + 
  labs(y = "Square meters >0.5m in depth", x = "Water Surface Elevation (m)")+   
  theme_classic()
dataframe<-data.frame(elevation, WB, WF, Fish, Anglers, Ramp)


#incooporate timing to objective utility
DOY<-c(1:365)
datalist <- list()
for(i in 1:length(DOY)){
  dataframe$DOY<-DOY[i]
  datalist[[i]]<-dataframe
}
big_data1 <- do.call(rbind, datalist)
write.csv(big_data1, "~/GitHub/Bluff-Lake-Project/_analysis/RawMetrics.csv")

#Incorporate Seasonal Weighting
dates<-data.frame(matrix(ncol = 2, nrow =365))
dates$DOY<-c(1:365)
dates$Month<-NA
for(i in 1:365){
  date<-as.Date(i, origin = "2016-01-01")
  Month<-month(as.POSIXlt(date, format="%d/%m/%Y"))
  dates$Month[i]<-Month
}
dates<-dates[,-c(1:2)]
big_data1<-merge(dates,big_data1)
#boating season
big_data1$Ramp<-ifelse(big_data1$DOY>=60 & big_data1$DOY<=304, big_data1$Ramp, 0)
#fishing season weight-Boat
Bt<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/BoatAngEF.csv")
Bt<-Bt%>%dplyr::group_by(Month)%>%dplyr::summarise(BTmean=mean(Total))
Month<-c(1,2,11,12)
BTmean<-c(0,0,0,0)
d1<-data.frame(Month, BTmean)
Bt<-rbind(Bt,d1)
Bt$BTmean<-Bt$BTmean/sum(Bt$BTmean)
mult<-merge(big_data1,Bt)
big_data1$Ramp<-mult$Ramp*mult$BTmean
#fishing season weight-Bank
Bk<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/BankAngEF.csv")
Bk<-Bk%>%dplyr::group_by(Month)%>%dplyr::summarise(BKmean=mean(Total))
#sub in missing months
Month<-c(1,2,11,12)
BKmean<-c(200,400,200,200)
d1<-data.frame(Month, BKmean)
Bk<-rbind(Bk,d1)
Bk$BKmean<- Bk$BKmean/sum(Bk$BKmean)
mult<-merge(mult,Bk)
big_data1$Anglers<-mult$Anglers*mult$BKmean
#waterbird Seasonality
range <- c(1:365)
mean <- 196
sd <-80
dist <- dnorm(range, mean = mean, sd = sd)+0.003
df <- data.frame("DOY" = range, "distribution" = dist)
df$distribution<-df$distribution/sum(df$distribution)
plot(df$distribution~df$DOY, main="waterbird")
mult<-merge(big_data1,df)
big_data1$WB<-mult$distribution*big_data1$WB
#Mississippi Growing Season
big_data1$WF<-ifelse(big_data1$DOY>=60 & big_data1$DOY<=304, big_data1$WF, 0)

#Rescale Metrics
WB<- rescale(WB, to=c(0,1))
WBM<-approxfun(elevation, WB, rule=2,yleft=0,yright=1)
plot(elevation, WB, xlab="Water Surface Elevation", ylab="Hectares (<20cm)", main = "Waterbird Habitat", type="l", col="blue")
WF<-rescale(WF, to=c(0,1))
WFM <- approxfun(elevation, WF, rule=2,yleft=0,yright=1)
Fish <-rescale(Fish, to=c(0,1))
FishM <- approxfun(elevation, Fish, rule=2,yleft=0,yright=1)
Anglers<-rescale(Anglers, to=c(0,1))
AnglersM <- approxfun(elevation, Anglers, rule=2,yleft=0,yright=1)
Ramp<-rescale(Ramp, to=c(0,1))
RampM <- approxfun(elevation, Ramp, rule=2,yleft=0,yright=1)
dataframe2<-data.frame(elevation, WB, WF, Fish, Anglers, Ramp)
#incooporate timing to objective utility
DOY<-c(1:365)
datalist <- list()
for(i in 1:length(DOY)){
  dataframe2$DOY<-DOY[i]
  datalist[[i]]<-dataframe2
}
big_data2 <- do.call(rbind, datalist)
write.csv(big_data2, "~/GitHub/Bluff-Lake-Project/_analysis/RescaleMetrics.csv")


#Form overall utility
W<- c(.25,.25,.25,.25)
big_data2$Utility<-(W[1]*((big_data2$Ramp*.5) + (big_data2$Anglers*.5))) + 
  (W[2]*big_data2$Fish) + (W[3]*big_data2$WB) + (W[4]*big_data2$WF)

#no utility for empty lake
big_data2$volume<-EL_Vol(big_data2$elevation)
big_data2$Utility<-ifelse(big_data2$volume<25861, 0, big_data2$Utility)







