library(scales)
dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9boards.csv")

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
plot(elevation, boards, xlab="Water Surface Elevation", ylab="Volume (m^3)", main = "Bluff Lake Volume", type="l", col="red", ylim=c(100500000,102000000), xlim=c(66.5,68.5))
gam_1 <- gam(boards ~ s(elevation, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
summary(gam_1)
gam_2 <- gam(elevation ~ s(volume, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
summary(gam_2)
par(new=T)
plot(gam_1$fitted.values~elevation, ylim=c(100500000,102000000), xlim=c(66.5,68.5), col="blue", type="l")

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
WB <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
WB<-rescale(WB, to=c(0,1))
plot(elevation, WB, xlab="Water Surface Elevation", ylab="Hectares (<20cm)", main = "Waterbird Habitat", type="l", col="red")
WBM <- gam(WB ~ s(elevation, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
par(new=T)
plot(WBM$fitted.values~elevation, col="blue", type="l")
# ---- Waterfowl
# DED/m^2=0.4374
# Dry Areas
Z <- sum(dat$POINT_Z > (66.45402)) #add board hight and required depth
Z <- Z*1.56*0.4374 #convert to DED per area
B <- sum(dat$POINT_Z > (66.45402+.22)) #add board hight and required depth
B <- B*1.56*0.4374 
BB <- sum(dat$POINT_Z > (66.45402+.44)) #add board hight and required depth
BB <- BB*1.56*0.4374 
BBB <- sum(dat$POINT_Z > (66.45402+.66)) #add board hight and required depth
BBB <- BBB*1.56*0.4374 
BBBB <- sum(dat$POINT_Z > (66.45402+.88)) #add board hight and required depth
BBBB <- BBBB*1.56*0.4374 
BBBBB <- sum(dat$POINT_Z > (66.45402+1.1)) #add board hight and required depth
BBBBB <- BBBBB*1.56*0.4374 
BBBBBB <- sum(dat$POINT_Z > (66.45402+1.32)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56*0.4374 
BBBBBBB <- sum(dat$POINT_Z > (66.45402+1.54)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56*0.4374 
BBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.76)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56*0.4374 
BBBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.98)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56*0.4374 
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
WF <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/1000000
WF<-rescale(WF, to=c(0,1))
plot(elevation, WF, xlab="Water Surface Elevation", ylab="DEDs (millions)", main = "Duck Energy Days", type="l", col="red")
WFM <- gam(WF ~ s(elevation, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
par(new=T)
plot(WFM$fitted.values~elevation, col="blue", type="l")
# ---- Fish
# Areas greater than 1 meter in depth
Z <- sum(dat$POINT_Z < (66.45402-1)) #add board hight and required depth
Z <- Z*1.56 #convert to area
B <- sum(dat$POINT_Z < (66.45402-1+.22)) #add board hight and required depth
B <- B*1.56 #convert to area
BB <- sum(dat$POINT_Z < (66.45402-1+.44)) #add board hight and required depth
BB <- BB*1.56 #convert to area
BBB <- sum(dat$POINT_Z < (66.45402-1+.66)) #add board hight and required depth
BBB <- BBB*1.56 #convert to area
BBBB <- sum(dat$POINT_Z < (66.45402-1+.88)) #add board hight and required depth
BBBB <- BBBB*1.56 #convert to area
BBBBB <- sum(dat$POINT_Z < (66.45402-1+1.1)) #add board hight and required depth
BBBBB <- BBBBB*1.56 #convert to area
BBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.32)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56 #convert to area
BBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.54)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56 #convert to area
BBBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.76)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56 #convert to area
BBBBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.98)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
Fish <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
Fish <-rescale(Fish, to=c(0,1))
plot(elevation, Fish, xlab="Water Surface Elevation", ylab="Hectares (>1m)", main = "Fish Habitat", type="l", col="red")
FishM <- gam(Fish ~ s(elevation, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
par(new=T)
plot(FishM$fitted.values~elevation, col="blue", type="l")
#### ---- Anglers
# For now, following fish model, Areas greater than 1 meter in depth
Adat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Anglers.csv")
Z <- sum(Adat$POINT_Z < (66.45402-1)) #add board hight and required depth
Z <- Z*1.56 #convert to area
B <- sum(Adat$POINT_Z < (66.45402-1+.22)) #add board hight and required depth
B <- B*1.56 #convert to area
BB <- sum(Adat$POINT_Z < (66.45402-1+.44)) #add board hight and required depth
BB <- BB*1.56 #convert to area
BBB <- sum(Adat$POINT_Z < (66.45402-1+.66)) #add board hight and required depth
BBB <- BBB*1.56 #convert to area
BBBB <- sum(Adat$POINT_Z < (66.45402-1+.88)) #add board hight and required depth
BBBB <- BBBB*1.56 #convert to area
BBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.1)) #add board hight and required depth
BBBBB <- BBBBB*1.56 #convert to area
BBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.32)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56 #convert to area
BBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.54)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56 #convert to area
BBBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.76)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56 #convert to area
BBBBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.98)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
Anglers <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
Anglers<-rescale(Anglers, to=c(0,1))
plot(elevation, Anglers, xlab="Water Surface Elevation", ylab="Hectares (>1m)", main = "Shoreline Fishing", type="l", col="red")
AnglersM <- gam(Anglers ~ s(elevation, bs = "cr"),
             data = matrix_gam,
             family = gaussian)
par(new=T)
plot(AnglersM$fitted.values~elevation, col="blue", type="l")
# Need to deliniate shorelines and establish "end" of boat ramp
#areas >.5m in depth
Bdat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Boat.csv")
Z <- sum(Bdat$POINT_Z < (66.45402)) #add board hight and required depth
Z <- Z*1.56 #convert to area
B <- sum(Bdat$POINT_Z < (66.45402+.22-0.5)) #add board hight and required depth
B <- B*1.56 #convert to area
BB <- sum(Bdat$POINT_Z < (66.45402+.44-0.5)) #add board hight and required depth
BB <- BB*1.56 #convert to area
BBB <- sum(Bdat$POINT_Z < (66.45402+.66-0.5)) #add board hight and required depth
BBB <- BBB*1.56 #convert to area
BBBB <- sum(Bdat$POINT_Z < (66.45402+.88-0.5)) #add board hight and required depth
BBBB <- BBBB*1.56 #convert to area
BBBBB <- sum(Bdat$POINT_Z < (66.45402+1.1-0.5)) #add board hight and required depth
BBBBB <- BBBBB*1.56 #convert to area
BBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.32-0.5)) #add board hight and required depth
BBBBBB <- BBBBBB*1.56 #convert to area
BBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.54-0.5)) #add board hight and required depth
BBBBBBB <- BBBBBBB*1.56 #convert to area
BBBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.76-0.5)) #add board hight and required depth
BBBBBBBB <- BBBBBBBB*1.56 #convert to area
BBBBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.98-0.5)) #add board hight and required depth
BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402, 
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
Ramp <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)
Ramp<-rescale(Ramp, to=c(0,1))
plot(elevation, Ramp, xlab="Water Surface Elevation", ylab="Wetted Area (m^2)", main = "Ramp Access", type="l", col="red")
RampM <- gam(Ramp ~ s(elevation, bs = "cr"),
                data = matrix_gam,
                family = gaussian)
par(new=T)
plot(RampM$fitted.values~elevation, col="blue", type="l")

#Utilities
dat2$WFUt<-predict(WFM, dat2)
dat2$WBUt<-predict(WBM, dat2)
dat2$FishUt<-predict(FishM, dat2)
dat2$AngUt<-predict(AnglersM, dat2)
dat2$RampUt<-predict(RampM, dat2)

dat2$WFUt <- ifelse(dat2$WFUt< 0, 0, dat2$WFUt)
dat2$WBUt<- ifelse(dat2$WBUt < 0, 0, dat2$WBUt)
dat2$FishUt <- ifelse(dat2$FishUt  < 0, 0, dat2$FishUt )
dat2$AngUt <- ifelse(dat2$AngUt < 0, 0, dat2$AngUt)
dat2$RampUt <- ifelse(dat2$RampUt < 0, 0, dat2$RampUt)

dat2$WFUt <- ifelse(dat2$WFUt > 1 , 1, dat2$WFUt)
dat2$WBUt<- ifelse(dat2$WBUt > 1 , 1, dat2$WBUt)
dat2$FishUt <- ifelse(dat2$FishUt  > 1 , 1, dat2$FishUt )
dat2$AngUt <- ifelse(dat2$AngUt > 1 , 1, dat2$AngUt)
dat2$RampUt <- ifelse(dat2$RampUt > 1 , 1, dat2$RampUt)

dat2$Utility<- dat2$RampUt+dat2$AngUt+dat2$FishUt+dat2$WBUt+dat2$WFUt


#colMeans(matrix(dat2$Utility, nrow=7))

write.csv(dat2, "~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/Weekly11.3cms.csv")

