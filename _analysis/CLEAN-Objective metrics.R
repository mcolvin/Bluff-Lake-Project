dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")

# ---- Lake Volume
# one point=4m^2
volume<-NA
boards<-c(0:17)
elevation<-66.45402+(0.2083*boards)
for(i in 1:length(elevation)){
  Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
  Z <- c((elevation[i]-Z))
  Z <- Z*4
  volume[i]<-sum(Z)
}
#actual board elevations <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
data<-data.frame(volume, elevation)
ggplot(data,aes(elevation, (volume/1000000))) + geom_line()+labs(y = bquote('Water Volume'~('million'~m^3)), x = "Water Surface Elevation (m)")+ theme_classic()

EL_Vol<-approxfun(elevation, volume, rule=2)
Vol_EL<-approxfun(volume, elevation, rule=2)




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

#Incorporate Seasonality
big_data$Ramp<-ifelse(big_data$DOY>=60 & big_data$DOY<=304, big_data$Ramp, 1)
big_data$Anglers
big_data$WB
big_data$WF


W<- c(.25,.25,.25,.25)
dat2$Utility<-(W[1]*((dat2$RampUt*.5) + (dat2$AngUt*.5))) + (W[2]*dat2$FishUt) + (W[3]*dat2$WBUt) + (W[4]*dat2$WFUt)

#no utility for empty lake
dat2$Utility<-ifelse(dat2$volume<=100447696, 0, dat2$Utility)







