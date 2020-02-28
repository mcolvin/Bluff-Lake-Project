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
plot(elevation, boards, xlab="Water Surface Elevation", ylab="Hectares (<20cm)", main = "Waterbird Habitat")

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

# ---- Paddlefish
# I DONT KNOW HOW TO DO THIS

# ---- Anglers
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

# ---- Lake Elevation and Gauge Data
Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEle.csv")
Gauge <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon.csv")
#Cypress
Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
library(dplyr)
library(lubridate)
Elevation<-Elevation %>% group_by(Elevation$Date.Time) %>% 
  summarise(mean(WSElevation))
month<-Elevation$`Elevation$Date.Time`
Elevation$month<- month(as.POSIXlt(month, format="%m/%d/%Y"), label= T)
Gauge$month <- month(as.POSIXlt(Gauge$datetime, format="%m/%d/%Y"), label= T)
Gauge<-subset(Gauge, Gauge$datetime %in% Elevation$`Elevation$Date.Time`)
#Elevation2 <- subset(Elevation,  Elevation$`Elevation$Date.Time`  %in% Gauge$datetime)
par(mfrow=c(3,3))

plot(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms, main="Bluff Lake Levee Gauge")
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
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="OCt",
       col="yellow")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Nov",
       col="black")
points(Elevation2$`mean(WSElevation)`~Gauge$Discharge_cms,
       subset=Gauge$month=="Dec",
       col="grey")
# May 
ElevationMay <- subset(Elevation2, Elevation2$month == "May")
GaugeMay <- subset(Gauge, Gauge$month=="May")
plot(ElevationMay$`mean(WSElevation)`~GaugeMay$Discharge_cms, main="May Cypress Gauge")
# June 
ElevationJune <- subset(Elevation2, Elevation2$month == "Jun")
GaugeJune <- subset(Gauge, Gauge$month=="Jun")
plot(ElevationJune$`mean(WSElevation)`~GaugeJune$Discharge_cms, main="June Cypress Gauge")
# July 
ElevationJul <- subset(Elevation2, Elevation2$month == "Jul")
GaugeJul <- subset(Gauge, Gauge$month=="Jul")
plot(ElevationJul$`mean(WSElevation)`~GaugeJul$Discharge_cms, main="July Cypress Gauge")
#August
ElevationAug <- subset(Elevation2, Elevation2$month == "Aug")
GaugeAug <- subset(Gauge, Gauge$month=="Aug")
plot(ElevationAug$`mean(WSElevation)`~GaugeAug$Discharge_cms, main="August Cypress Gauge")
#September
ElevationSep <- subset(Elevation2, Elevation2$month == "Sep")
GaugeSep <- subset(Gauge, Gauge$month=="Sep")
plot(ElevationSep$`mean(WSElevation)`~GaugeSep$Discharge_cms, main="September Cypress Gauge")
#October
ElevationOct <- subset(Elevation2, Elevation2$month == "Oct")
GaugeOct <- subset(Gauge, Gauge$month=="Oct")
plot( ElevationOct$`mean(WSElevation)`~GaugeOct$Discharge_cms, main="October Cypress Gauge")
#November
ElevationNov <- subset(Elevation2, Elevation2$month == "Nov")
GaugeNov <- subset(Gauge, Gauge$month=="Nov")
plot( ElevationNov$`mean(WSElevation)`~GaugeNov$Discharge_cms, main="November Cypress Gauge")
#December
ElevationDec <- subset(Elevation2, Elevation2$month == "Dec")
GaugeDec <- subset(Gauge, Gauge$month=="Dec")
plot(ElevationDec$`mean(WSElevation)`~GaugeDec$Discharge_cms, main="December Cypress Gauge")


#Gauge
Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEle.csv")
Gauge <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon.csv")
Elevation <- subset(Elevation, Elevation$ï..Location == "Gauge")
Elevation<-Elevation %>% group_by(Elevation$Date.Time) %>% 
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

