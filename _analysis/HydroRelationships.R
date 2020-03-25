# ---- Making Plots
#each board is assumed 22 cm
#base elevation 66.45402
# one point=6.25m^2
setwd("~/GitHub/Bluff-Lake-Project/_analysis")
dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2.csv")

# ---- Waterbirds
# areas must be less than 20 cm in depth
Z <- length(which(dat$POINT_Z > (66.45402-.20))) #add board hight and required depth
Z <- Z*1.56 #convert to area
B <- length(which(dat$POINT_Z > (66.45402+.22-.20))) #add board hight and required depth
B <- B*1.56 #convert to area
BB <- length(which(dat$POINT_Z > (66.45402+.44-.20))) #add board hight and required depth
BB <- BB*1.56 #convert to area
BBB <- length(which(dat$POINT_Z > (66.45402+.66-.20))) #add board hight and required depth
BBB <- BBB*1.56 #convert to area
BBBB <- length(which(dat$POINT_Z > (66.45402+.88-.20))) #add board hight and required depth
BBBB <- BBBB*1.56 #convert to area
BBBBB <- length(which(dat$POINT_Z > (66.45402+1.1-.20))) #add board hight and required depth
BBBBB <- BBBBB*1.56 #convert to area
BBBBBB <- length(which(dat$POINT_Z > (66.45402+1.32-.20))) #add board hight and required depth
BBBBBB <- BBBBBB*1.56 #convert to area
BBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.54-.20))) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56 #convert to area
BBBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.76-.20))) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56 #convert to area
BBBBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.98-.20))) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
boards <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
plot(elevation, boards, xlab="Water Surface Elevation", ylab="Hectares (<20cm)", main = "Waterbird Habitat", type="l", col="red")

# ---- Waterfowl
# DED/m^2=0.4374
# Dry Areas
Z <- sum(dat$POINT_Z > (66.45402)) #add board hight and required depth
Z <- Z*1.56*0.4374 #convert to area
B <- sum(dat$POINT_Z > (66.45402+.22)) #add board hight and required depth
B <- B*1.56*0.4374 #convert to area
BB <- sum(dat$POINT_Z > (66.45402+.44)) #add board hight and required depth
BB <- BB*1.56*0.4374 #convert to area
BBB <- sum(dat$POINT_Z > (66.45402+.66)) #add board hight and required depth
BBB <- BBB*1.56*0.4374 #convert to area
BBBB <- sum(dat$POINT_Z > (66.45402+.88)) #add board hight and required depth
BBBB <- BBBB*1.56*0.4374 #convert to area
BBBBB <- sum(dat$POINT_Z > (66.45402+1.1)) #add board hight and required depth
BBBBB <- BBBBB*1.56*0.4374 #convert to area
BBBBBB <- sum(dat$POINT_Z > (66.45402+1.32)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56*0.4374 #convert to area
BBBBBBB <- sum(dat$POINT_Z > (66.45402+1.54)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56*0.4374 #convert to area
BBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.76)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56*0.4374 #convert to area
BBBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.98)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56*0.4374 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
boards <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/1000000
plot(elevation, boards, xlab="Water Surface Elevation", ylab="DEDs (millions)", main = "Duck Energy Days")

# ---- Fish
# Areas greater than 1 meter in depth
Z <- sum(dat$POINT_Z < (66.45402+1)) #add board hight and required depth
Z <- Z*1.56 #convert to area
B <- sum(dat$POINT_Z < (66.45402+1+.22)) #add board hight and required depth
B <- B*1.56 #convert to area
BB <- sum(dat$POINT_Z < (66.45402+1+.44)) #add board hight and required depth
BB <- BB*1.56 #convert to area
BBB <- sum(dat$POINT_Z < (66.45402+1+.66)) #add board hight and required depth
BBB <- BBB*1.56 #convert to area
BBBB <- sum(dat$POINT_Z < (66.45402+1+.88)) #add board hight and required depth
BBBB <- BBBB*1.56 #convert to area
BBBBB <- sum(dat$POINT_Z < (66.45402+1+1.1)) #add board hight and required depth
BBBBB <- BBBBB*1.56 #convert to area
BBBBBB <- sum(dat$POINT_Z < (66.45402+1+1.32)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56 #convert to area
BBBBBBB <- sum(dat$POINT_Z < (66.45402+1+1.54)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56 #convert to area
BBBBBBBB <- sum(dat$POINT_Z < (66.45402+1+1.76)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56 #convert to area
BBBBBBBBB <- sum(dat$POINT_Z < (66.45402+1+1.98)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
boards <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
plot(elevation, boards, xlab="Water Surface Elevation", ylab="Hectares (>1m)", main = "Fish Habitat")

#### ---- Anglers
# For now, following fish model, Areas greater than 1 meter in depth
# Need to deliniate shorelines and establish "end" of boat ramp

# ---- Lake Volume
# one point=6.25m^2
Z <- c(dat$POINT_Z < (66.45402+0))
Z <- c((66.45402+0-Z))
Z <- Z*1.56
Z <- sum(Z)
B <- c(dat$POINT_Z < (66.45402+.22)) 
B <- c((66.45402+.22-B))
B <- B*1.56
B <- sum(B) #convert to volume
BB <- c(dat$POINT_Z < (66.45402+.44)) #add board hight and required depth
BB <- c((66.45402+.44-BB))
BB <- BB*1.56 
BB <- sum(BB) 
BBB <- c(dat$POINT_Z < (66.45402+.66)) #add board hight and required depth
BBB <- c((66.45402+.66-BBB))
BBB <- BBB*1.56
BBB <- sum(BBB) 
BBBB <- c(dat$POINT_Z < (66.45402+.88)) #add board hight and required depth
BBBB <- c((66.45402+.88-BBBB))
BBBB <- BBBB*1.56 
BBBB <- sum(BBBB) 
BBBBB <- c(dat$POINT_Z < (66.45402+1.1)) #add board hight and required depth
BBBBB <- c((66.45402+1.1-BBBBB))
BBBBB <- BBBBB*1.56 
BBBBB <- sum(BBBBB) 
BBBBBB <- c(dat$POINT_Z < (66.45402+1.32)) #add board hight and required depth
BBBBBB <- c((66.45402+1.32-BBBBBB))
BBBBBB <- BBBBBB*1.56 
BBBBBB <- sum(BBBBBB) 
BBBBBBB <- c(dat$POINT_Z < (66.45402+1.54)) #add board hight and required depth
BBBBBBB <- c((66.45402+1.54-BBBBBBB))
BBBBBBB <- BBBBBBB*1.56 
BBBBBBB <- sum(BBBBBBB) 
BBBBBBBB <- c(dat$POINT_Z < (66.45402+1.76)) #add board hight and required depth
BBBBBBBB <- c((66.45402+1.76-BBBBBBBB))
BBBBBBBB <- BBBBBBBB*1.56 
BBBBBBBB <- sum(BBBBBBBB) 
BBBBBBBBB <- c(dat$POINT_Z < (66.45402+1.98)) #add board hight and required depth
BBBBBBBBB <- c((66.45402+1.98-BBBBBBBBB))
BBBBBBBBB <- BBBBBBBBB*1.56 
BBBBBBBBB <- sum(BBBBBBBBB) 
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
boards <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)
plot(elevation, boards, xlab="Water Surface Elevation", ylab="Volume (m^3)", main = "Bluff Lake Volume")
lm<-lm(boards~elevation)

# ---- Lake Elevation and Gauge Data
Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEL.csv")
Gauge <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon.csv")
#Cypress
Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
library(dplyr)
library(lubridate)
Elevation$Date.Time<-as.POSIXct(Elevation$Date.Time, format="%m/%d/%Y")
Elevation$Date.Time<-round_date(Elevation$Date.Time, "day")
Elevation<-Elevation %>% dplyr::group_by(Elevation$Date.Time) %>% 
  dplyr::summarise(mean(WSElevation))
month<-Elevation$`Elevation$Date.Time`
Elevation$month<- lubridate::month(as.POSIXct(month, format="%m/%d/%Y"), label= T)
Gauge$Date.Time<-as.POSIXct(Gauge$Date.Time, format="%m/%d/%Y")
Gauge$month <- lubridate::month(as.POSIXct(Gauge$Date.Time, format="%Y-%m-%d"), label= T)
Gauge<-subset(Gauge, Gauge$Date.Time %in% Elevation$`Elevation$Date.Time`)
Elevation <- subset(Elevation,  Elevation$`Elevation$Date.Time`  %in% Gauge$Date.Time)
par(mfrow=c(3,3))

plot(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms, main="Bluff Lake Levee Gauge")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="May",
       col="red")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Jun",
       col="blue")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Jul",
       col="green")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Aug",
       col="purple")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Sep",
       col="orange")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="OCt",
       col="yellow")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Nov",
       col="black")
points(Elevation$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Dec",
       col="grey")
# May 
ElevationMay <- subset(Elevation, Elevation$month == "May")
GaugeMay <- subset(Gauge, Gauge$month=="May")
plot(ElevationMay$`mean(WSElevation)`~GaugeMay$Discharge_cms, main="May Cypress Gauge")
# June 
ElevationJune <- subset(Elevation, Elevation$month == "Jun")
GaugeJune <- subset(Gauge, Gauge$month=="Jun")
plot(ElevationJune$`mean(WSElevation)`~GaugeJune$Discharge_cms, main="June Cypress Gauge")
# July 
ElevationJul <- subset(Elevation, Elevation$month == "Jul")
GaugeJul <- subset(Gauge, Gauge$month=="Jul")
plot(ElevationJul$`mean(WSElevation)`~GaugeJul$Discharge_cms, main="July Cypress Gauge")
#August
ElevationAug <- subset(Elevation, Elevation$month == "Aug")
GaugeAug <- subset(Gauge, Gauge$month=="Aug")
plot(ElevationAug$`mean(WSElevation)`~GaugeAug$Discharge_cms, main="August Cypress Gauge")
#September
ElevationSep <- subset(Elevation, Elevation$month == "Sep")
GaugeSep <- subset(Gauge, Gauge$month=="Sep")
plot(ElevationSep$`mean(WSElevation)`~GaugeSep$Discharge_cms, main="September Cypress Gauge")
#October
ElevationOct <- subset(Elevation, Elevation$month == "Oct")
GaugeOct <- subset(Gauge, Gauge$month=="Oct")
plot( ElevationOct$`mean(WSElevation)`~GaugeOct$Discharge_cms, main="October Cypress Gauge")
#November
ElevationNov <- subset(Elevation, Elevation$month == "Nov")
GaugeNov <- subset(Gauge, Gauge$month=="Nov")
plot( ElevationNov$`mean(WSElevation)`~GaugeNov$Discharge_cms, main="November Cypress Gauge")
#December
ElevationDec <- subset(Elevation, Elevation$month == "Dec")
GaugeDec <- subset(Gauge, Gauge$month=="Dec")
plot(ElevationDec$`mean(WSElevation)`~GaugeDec$Discharge_cms, main="December Cypress Gauge")


#Gauge
Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEle.csv")
Gauge <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon.csv")
Elevation <- subset(Elevation, Elevation$ï..Location == "Gauge")
Elevation<-Elevation %>% group_by(Elevation$Date.Time,) %>% 
  summarise(mean(WSElevation))
month<-Elevation$`Elevation$Date.Time`
Elevation$month<- month(as.POSIXlt(month, format="%m/%d/%Y"), label= T)
Gauge$month <- month(as.POSIXlt(Gauge$datetime, format="%m/%d/%Y"), label= T)
Gauge<-subset(Gauge, Gauge$datetime %in% Elevation$`Elevation$Date.Time`)
Elevation2 <- subset(Elevation,  Elevation$`Elevation$Date.Time`  %in% Gauge$datetime)

par(mfrow=c(3,3))
plot( Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms, 
      main="Bluff Lake Levee Gauge")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="May",
       col="red")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Jun",
       col="blue")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Jul",
       col="green")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Aug",
       col="purple")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Sep",
       col="orange")
points(Gauge$Discharge_cms~ Elevation2$`mean(WSElevation)`,
       subset=Gauge$month=="OCt",
       col="yellow")
points(Gauge$Discharge_cms~ Elevation2$`mean(WSElevation)`,
       subset=Gauge$month=="Nov",
       col="black")
points(Gauge$Discharge_cms~ Elevation2$`mean(WSElevation)`,
       subset=Gauge$month=="Dec",
       col="grey")
# May 
ElevationMay <- subset(Elevation2, Elevation2$month == "May")
GaugeMay <- subset(Gauge, Gauge$month=="May")
plot(ElevationMay$`mean(WSElevation)`~GaugeMay$Discharge_cms, main="May Levee Gauge")
# June 
ElevationJune <- subset(Elevation2, Elevation2$month == "Jun")
GaugeJune <- subset(Gauge, Gauge$month=="Jun")
plot(ElevationJune$`mean(WSElevation)`~GaugeJune$Discharge_cms, main="June Levee Gauge")
# July 
ElevationJul <- subset(Elevation2, Elevation2$month == "Jul")
GaugeJul <- subset(Gauge, Gauge$month=="Jul")
plot(ElevationJul$`mean(WSElevation)`~GaugeJul$Discharge_cms, main="July Levee Gauge")
#August
ElevationAug <- subset(Elevation2, Elevation2$month == "Aug")
GaugeAug <- subset(Gauge, Gauge$month=="Aug")
plot(ElevationAug$`mean(WSElevation)`~GaugeAug$Discharge_cms, main="August Levee Gauge")
#September
ElevationSep <- subset(Elevation2, Elevation2$month == "Sep")
GaugeSep <- subset(Gauge, Gauge$month=="Sep")
plot(ElevationSep$`mean(WSElevation)`~GaugeSep$Discharge_cms, main="September Levee Gauge")
#October
ElevationOct <- subset(Elevation2, Elevation2$month == "Oct")
GaugeOct <- subset(Gauge, Gauge$month=="Oct")
plot( ElevationOct$`mean(WSElevation)`~GaugeOct$Discharge_cms, main="October Levee Gauge")
#November
ElevationNov <- subset(Elevation2, Elevation2$month == "Nov")
GaugeNov <- subset(Gauge, Gauge$month=="Nov")
plot( ElevationNov$`mean(WSElevation)`~GaugeNov$Discharge_cms, main="November Levee Gauge")
#December
ElevationDec <- subset(Elevation2, Elevation2$month == "Dec")
GaugeDec <- subset(Gauge, Gauge$month=="Dec")
plot(ElevationDec$`mean(WSElevation)`~GaugeDec$Discharge_cms, main="December Levee Gauge")

# Time Series Plot
# There were strange patterns in the plots elevation~discharge plots (above). Dischare was increasing as water levels within Bluff Lake decreased or remained the same. We wanted to determine if there was a lag in "peaking" between Bluff Lake and the Gauge.
#January
#JAN
par(mfrow=c(1,1))
dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon15min.csv")
dat2$Date.Time <- as.POSIXct(dat2$Date.Time, format="%m/%d/%Y %H:%M")
dat2$month<- lubridate::month(as.POSIXct(dat2$Date.Time, format="%Y/%m/%d %H:%M"), label= T)
dat2Jan<-subset(dat2, dat2$month=="Jan")
plot(dat2Jan$ï..CFS~dat2Jan$Date.Time, type='l',col="red")
#ggplot(data = dat2Jan, aes(x = ï..datetime, y = CFS))+
 # geom_line(color = "#00BF7D", size = 2)

Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEL.csv")
Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
Elevation$Date.Time <- (as.POSIXct(Elevation$Date.Time, format="%m/%d/%Y %H:%M"))
Elevation$month<- lubridate::month(as.POSIXct(Elevation$Date.Time, format="%Y/%m/%d %H:%M"), label= T)
ElevationJan<-subset(Elevation, Elevation$month=="Jan")
par(new=TRUE)
ElevationJan<-ElevationJan[order(ElevationJan$Date.Time),]
plot(WSElevation~Date.Time,ElevationJan, main="January",type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topright", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#ggplot(data = ElevationJan, aes(x = Date.Time, y = WSElevation))+
 # geom_line(color = "#6BB100", size = 2)
#DECEMBER
dat2Dec<-subset(dat2, dat2$month=="Dec")
plot(dat2Dec$CFS~dat2Dec$ï..datetime, main="December",type='l',col="red")
ElevationDec<-subset(Elevation, Elevation$month=="Dec")
par(new=TRUE)
ElevationDec<-ElevationDec[order(ElevationDec$Date.Time),]
plot(WSElevation~Date.Time,ElevationDec, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topleft", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))

#November
dat2Nov<-subset(dat2, dat2$month=="Nov")
plot(dat2Nov$CFS~dat2Nov$ï..datetime, main="November",type='l',col="red")
ElevationNov<-subset(Elevation, Elevation$month=="Nov")
par(new=TRUE)
ElevationNov<-ElevationNov[order(ElevationNov$Date.Time),]
plot(WSElevation~Date.Time,ElevationNov, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
abline(abline(v=as.POSIXct("2019-11-09")))
legend("topright", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#October
dat2Oct<-subset(dat2, dat2$month=="Oct")
plot(dat2Oct$CFS~dat2Oct$ï..datetime, main="October",type='l',col="red")
ElevationOct<-subset(Elevation, Elevation$month=="Oct")
par(new=TRUE)
ElevationOct<-ElevationOct[order(ElevationOct$Date.Time),]
plot(WSElevation~Date.Time,ElevationOct, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topleft", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#September
dat2Sep<-subset(dat2, dat2$month=="Sep")
plot(dat2Sep$ï..CFS~dat2Sep$Date.Time, main="September",type='l',col="red", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
ElevationSep<-subset(Elevation, Elevation$month=="Sep")
par(new=TRUE)
ElevationSep<-ElevationSep[order(ElevationSep$Date.Time),]
plot(WSElevation~Date.Time,ElevationSep, type="l")
legend("topright", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#August
dat2Aug<-subset(dat2, dat2$month=="Aug")
plot(dat2Aug$CFS~dat2Aug$ï..datetime, main="August",type='l',col="red", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
ElevationAug<-subset(Elevation, Elevation$month=="Aug")
par(new=TRUE)
ElevationAug<-ElevationAug[order(ElevationAug$Date.Time),]
plot(WSElevation~Date.Time,ElevationAug, type="l")
abline(abline(v=as.POSIXct("2019-08-07")))
legend("topleft", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#July
dat2Jul<-subset(dat2, dat2$month=="Jul")
plot(dat2Jul$CFS~dat2Jul$ï..datetime, main="July",type='l',col="red")
ElevationJul<-subset(Elevation, Elevation$month=="Jul")
par(new=TRUE)
ElevationJul<-ElevationJul[order(ElevationJul$Date.Time),]
plot(WSElevation~Date.Time,ElevationJul, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topleft", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#June
dat2Jun<-subset(dat2, dat2$month=="Jun")
plot(dat2Jun$CFS~dat2Jun$ï..datetime, main="June",type='l',col="red")
ElevationJun<-subset(Elevation, Elevation$month=="Jun")
par(new=TRUE)
ElevationJun<-ElevationJun[order(ElevationJun$Date.Time),]
plot(WSElevation~Date.Time,ElevationJun, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topright", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))
#May
dat2May<-subset(dat2, dat2$month=="May")
plot(dat2May$CFS~dat2May$ï..datetime, main="May",type='l',col="red")
ElevationMay<-subset(Elevation, Elevation$month=="May")
par(new=TRUE)
ElevationMay<-ElevationMay[order(ElevationMay$Date.Time),]
plot(WSElevation~Date.Time,ElevationMay, type="l", 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
legend("topright", c("Discharge", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))

#Links between water surface elevation and Macon gauge discharge
par(mfrow=c(1,1))
library(tidyverse)
library(lubridate)
library(plyr)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon15min.csv")
dat2$Date.Time <- as.POSIXct(dat2$Date.Time, format="%m/%d/%Y %H:%M")
dat2$month<- lubridate::month(as.POSIXct(dat2$Date.Time, format="%Y/%m/%d %H:%M"), label = T)
dat2$Date.Time<-round_date(dat2$Date.Time, "30 mins")
CMS<-ddply(dat2, c("Date.Time","month"), summarize,
           meanCMS=mean(ï..CFS))

Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEL.csv")
Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
Elevation$Date.Time <- (as.POSIXct(Elevation$Date.Time, format="%m/%d/%Y %H:%M"))
Elevation$month<- lubridate::month(as.POSIXct(Elevation$Date.Time, format="%Y/%m/%d %H:%M"), label= T)
data<-merge(Elevation, CMS)

M1<-lm(WSElevation~meanCMS, data)
summary(M1)
plot(M1)
plot(WSElevation~meanCMS, data)

M2<-lm(WSElevation~meanCMS+month,data)
summary(M2)
plot(M2)
plot(WSElevation~meanCMS+month, data)

M3<-lm(WSElevation~meanCMS+month+ meanCMS*month,data)
summary(M3)
plot(M3)
plot(WSElevation~meanCMS, data)
data$FitLM3<- M3$fitted.values
plot(FitLM3~Date.Time, data, type="l", col="red", ylim=c(68.6,69.8), 
     main="Linear Model 4")
par(new=T)
plot(WSElevation~Date.Time, data, type="l", col="blue", ylim=c(68.6,69.8))
legend("topleft", c("Predicted", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))

#try GAM
library(mgcv)
library(data.table)

matrix_gam <- data.table(Ele = data$WSElevation,
                         CMS = data$meanCMS,
                         Month = data$month)

gam_1 <- gam(Ele ~ s(CMS, bs = "cr") +
  s(Month, bs = "ps", k = 8),
             data = matrix_gam,
             family = gaussian)
summary(gam_1)

gam_2 <- gam(Ele ~ s(CMS, Month),
             data = matrix_gam,
             family = gaussian)
summary(gam_2)

gam_3 <- gam(Ele ~ te(CMS, Month,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)
summary(gam_3)
summary(gam_3)$s.table

as.data.frame(data)
data$FitG3<- gam_3$fitted.values

plot(FitG3~Date.Time, data, type="l", col="red", ylim=c(68.6,69.8), main="GAM Model 3")
par(new=T)
plot(WSElevation~Date.Time, data, type="l", col="blue", ylim=c(68.6,69.8))
legend("topleft", c("Predicted", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))


#I don't know what's going on after this point, but specifying knots for the model helps... but edf shoot through the roof. Out of 4,5, & 6 gam_4 socres the best (AIC)
gam_4 <- gam(Ele ~ te(CMS, Month,
                       k = c(48, 8),
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)
summary(gam_4)
summary(gam_4)$s.table

data$FitG4<- gam_4$fitted.values
plot(FitG4~Date.Time, data, type="l", col="red", ylim=c(68.6,69.8), 
     main="GAM Model 4")
par(new=T)
plot(WSElevation~Date.Time, data, type="l", col="blue", ylim=c(68.6,69.8))
legend("topleft", c("Predicted", "Lake Elevation"),
       col = c("red", "black"), lty = c(1, 1))

gam_5 <- gam(Ele ~ s(CMS, bs = "cr", k = 48) +
               s(Month, bs = "ps", k = 8) +
               ti(CMS, Month,
                  k = c(48, 7),
                  bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)
summary(gam_5)$r.sq
summary(gam_5)$s.table

gam_6 <- gam(Ele ~ t2(CMS, Month,
                       k = c(48, 7),
                       bs = c("cr", "ps"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)
summary(gam_6)$r.sq
summary(gam_6)$s.table

AIC(gam_4, gam_5, gam_6)

#group data by day
data$Date.Time<-round_date(data$Date.Time, "day")
data<-data %>% dplyr::group_by(data$Date.Time) %>% 
  dplyr::summarise_all(funs(mean))
data2<-data
#differce between days elevation
data2$DeltaEle<-c(NA, diff(data$WSElevation))
#convert Elevation to volume
data2$Vol<-(56446953+(577084*data2$WSElevation))
#change in volume
data2$DeltaVol<-c(NA,diff(data2$Vol))
#difference between days CMS
data2$DeltaCMS<-c(NA,diff(data$meanCMS))
#add month back
data2$month<- lubridate::month(as.POSIXct(data2$Date.Time, format="%m/%d/%Y"), label= T)

LM<-lm(DeltaVol~DeltaCMS+month, data2)
summary(LM)
plot(DeltaVol~DeltaCMS, data2)

LM<-lm(DeltaVol~meanCMS+month, data2)
summary(LM)
plot(DeltaVol~meanCMS, data2)

#Wellll....clearly that didn't work, what if we get rid of the negative numbers?
data3<-data2
data3$DeltaVol<-abs(data2$DeltaVol)
data3$DeltaCMS<-abs(data2$DeltaCMS)
data3$DeltaEle<-abs(data2$DeltaEle)

LM<-lm(DeltaVol~DeltaCMS+month, data3)
summary(LM)
plot(DeltaVol~DeltaCMS+month, data3)

LM<-lm(DeltaVol~meanCMS+month, data3)
summary(LM)
plot(DeltaVol~meanCMS+month, data3)

#Still garbage...actually get rid of Negatives?
data4<-data2
data4 <- filter(data4, DeltaVol>0)
data4 <- filter(data4, DeltaCMS>0)
# LM<-lm(DeltaVol~DeltaCMS+month, data4)
# summary(LM)
# plot(DeltaVol~DeltaCMS+month, data4)
# LM<-lm(DeltaVol~meanCMS, data4)
# summary(LM)
# plot(DeltaVol~meanCMS, data4, col="blue", main="Change in Lake Volume vs Discharge")
# par(new=T)
# plot(LM$fitted.values~data4$meanCMS, col="red")
# legend("bottomleft", c("Predicted", "Actual"),
#        col = c("red", "blue"), lty = c(1, 1))
Posgam <- gam(DeltaVol ~ s(meanCMS),
             data = data4,
             family = gaussian)
summary(Posgam)
plot(DeltaVol~meanCMS, data4, col="blue", main="Change in Lake Volume vs Discharge")
par(new=T)
plot(Posgam$fitted.values~data4$meanCMS, col="red")
legend("topleft", c("Predicted", "Actual"),
       col = c("red", "blue"), lty = c(1, 1))
data5<-data2
data5 <- filter(data5, DeltaVol<0)
data5 <- filter(data5, DeltaCMS<0)
Neggam <- gam(DeltaVol ~ s(meanCMS),
              data = data5,
              family = gaussian)
summary(Neggam)
plot(DeltaVol~meanCMS, data5, col="blue", main="Change in Lake Volume vs Discharge")
par(new=T)
plot(Neggam$fitted.values~data5$meanCMS, col="red")
legend("bottomleft", c("Predicted", "Actual"),
       col = c("red", "blue"), lty = c(1, 1))

#DeltaVol
#for positive 10140.42+728.45*meanCMS
# #for negative -3364.40+-298.40*meanCMS
# data2$DeltaCMS<-c(NA,diff(data$meanCMS))
# nrow<-nrow(data)
# data2$DeltaVol<-for(row in 1:nrow(data2)){
#   if (data2$DeltaCMS>0){
#     data2$meanCMS*728.45+10140.42
#   } else {
#     data2$meanCMS*-298.40+-3364.4
#   }
# }
# volume_{next day} = volume_{previous day} + [change in volume]-[Paddlefish release]
data4$NextDay_P0<- data4$Vol+Posgam$fitted.values-(0)
data4$NextDay_P1<- data4$Vol+Posgam$fitted.values-(28800)
data4$NextDay_P2<- data4$Vol+Posgam$fitted.values-(28800*2)
data4$NextDay_P3<- data4$Vol+Posgam$fitted.values-(28800*3)
data4$NextDay_P4<- data4$Vol+Posgam$fitted.values-(28800*4)
data4$NextDay_P5<- data4$Vol+Posgam$fitted.values-(28800*5)
data4$NextDay_P6<- data4$Vol+Posgam$fitted.values-(28800*6)
data4$NextDay_P7<- data4$Vol+Posgam$fitted.values-(28800*7)
data4$NextDay_P8<- data4$Vol+Posgam$fitted.values-(28800*8)
data4$NextDay_P9<- data4$Vol+Posgam$fitted.values-(28800*9)
data4$NextDay_P10<- data4$Vol+Posgam$fitted.values-(28800*10)
data4$NextDay_P11<- data4$Vol+Posgam$fitted.values-(28800*11)
data4$NextDay_P12<- data4$Vol+Posgam$fitted.values-(28800*12)
data4$NextDay_P13<- data4$Vol+Posgam$fitted.values-(28800*13)
data4$NextDay_P14<- data4$Vol+Posgam$fitted.values-(28800*14)
data4$NextDay_P15<- data4$Vol+Posgam$fitted.values-(28800*15)

data5$NextDay_P0<- data5$Vol+Neggam$fitted.values-(0)
data5$NextDay_P1<- data5$Vol+Neggam$fitted.values-(28800)
data5$NextDay_P2<- data5$Vol+Neggam$fitted.values-(28800*2)
data5$NextDay_P3<- data5$Vol+Neggam$fitted.values-(28800*3)
data5$NextDay_P4<- data5$Vol+Neggam$fitted.values-(28800*4)
data5$NextDay_P5<- data5$Vol+Neggam$fitted.values-(28800*5)
data5$NextDay_P6<- data5$Vol+Neggam$fitted.values-(28800*6)
data5$NextDay_P7<- data5$Vol+Neggam$fitted.values-(28800*7)
data5$NextDay_P8<- data5$Vol+Neggam$fitted.values-(28800*8)
data5$NextDay_P9<- data5$Vol+Neggam$fitted.values-(28800*9)
data5$NextDay_P10<- data5$Vol+Neggam$fitted.values-(28800*10)
data5$NextDay_P11<- data5$Vol+Neggam$fitted.values-(28800*11)
data5$NextDay_P12<- data5$Vol+Neggam$fitted.values-(28800*12)
data5$NextDay_P13<- data5$Vol+Neggam$fitted.values-(28800*13)
data5$NextDay_P14<- data5$Vol+Neggam$fitted.values-(28800*14)
data5$NextDay_P15<- data5$Vol+Neggam$fitted.values-(28800*15)

data6<-rbind(data5, data4)
data6<-data6[order(data6$Date.Time),]
plot(NextDay_P0~Date.Time,data6, ylab="Next Day Volume", xlab="Date", main="Results of 1 CMS increment 8 hour discharge", ylim=c(min(NextDay_P15), max(NextDay_P1)), type = "l", col="black")
lines(NextDay_P1~Date.Time,data6, col="red")
lines(NextDay_P2~Date.Time,data6, col="green")
lines(NextDay_P3~Date.Time,data6, col="blue")
lines(NextDay_P4~Date.Time,data6, col="yellow")
lines(NextDay_P5~Date.Time,data6, col="purple")
lines(NextDay_P6~Date.Time,data6, col="grey")
lines(NextDay_P7~Date.Time,data6, col="brown")
lines(NextDay_P8~Date.Time,data6, col="orange")
lines(NextDay_P9~Date.Time,data6, col="cyan")
lines(NextDay_P10~Date.Time,data6, col="turquoise")
lines(NextDay_P11~Date.Time,data6, col="chartreuse")
lines(NextDay_P12~Date.Time,data6, col="blue4")
lines(NextDay_P13~Date.Time,data6, col="red2")
lines(NextDay_P14~Date.Time,data6, col="purple3")
lines(NextDay_P15~Date.Time,data6, col="green4")
abline(96036526,0)
abline(95761000,0)


data4$NextDay_P0<- data4$Vol+Posgam$fitted.values-(0)
data4$NextDay_P1<- data4$Vol+Posgam$fitted.values-(28800*11.3)
data4$NextDay_P2<- data4$Vol+Posgam$fitted.values-(28800*22.6)
data4$NextDay_P3<- data4$Vol+Posgam$fitted.values-(28800*33.9)

data5$NextDay_P0<- data5$Vol+Neggam$fitted.values-(0)
data5$NextDay_P1<- data5$Vol+Neggam$fitted.values-(28800*11.3)
data5$NextDay_P2<- data5$Vol+Neggam$fitted.values-(28800*22.6)
data5$NextDay_P3<- data5$Vol+Neggam$fitted.values-(28800*33.9)

data6<-rbind(data5, data4)
data6<-data6[order(data6$Date.Time),]
plot(NextDay_P0~Date.Time,data6, ylab="Next Day Volume", xlab="Date", main="Results of 11.3 CMS increment 8 hour discharge", ylim=c(min(NextDay_P3), max(NextDay_P0)), type = "l", col="black")
lines(NextDay_P1~Date.Time,data6, col="purple")
lines(NextDay_P2~Date.Time,data6, col="green")
lines(NextDay_P3~Date.Time,data6, col="blue")
abline(96036526,0)
abline(95761000,0)
abline(95581226,0)
abline(95458173,0)
abline(95390602,0)
abline(95370439,0, col="red")


write.csv(data6, "Paddlefish.csv")

data6