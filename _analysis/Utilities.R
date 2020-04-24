library(scales)
library(mgcv)
library(lubridate)
library(dplyr)
library(tidyverse)
# dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
# dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9Boards.csv")
# 
# # ---- Lake Volume
# # one point=6.25m^2
# Z <- c(dat$POINT_Z < (66.45402+0))
# Z <- c((66.45402+0-Z))
# Z <- Z*1.56
# Z <- sum(Z)
# B <- c(dat$POINT_Z < (66.45402+.22))
# B <- c((66.45402+.22-B))
# B <- B*1.56
# B <- sum(B) #convert to volume
# BB <- c(dat$POINT_Z < (66.45402+.44)) #add board hight and required depth
# BB <- c((66.45402+.44-BB))
# BB <- BB*1.56
# BB <- sum(BB)
# BBB <- c(dat$POINT_Z < (66.45402+.66)) #add board hight and required depth
# BBB <- c((66.45402+.66-BBB))
# BBB <- BBB*1.56
# BBB <- sum(BBB)
# BBBB <- c(dat$POINT_Z < (66.45402+.88)) #add board hight and required depth
# BBBB <- c((66.45402+.88-BBBB))
# BBBB <- BBBB*1.56
# BBBB <- sum(BBBB)
# BBBBB <- c(dat$POINT_Z < (66.45402+1.1)) #add board hight and required depth
# BBBBB <- c((66.45402+1.1-BBBBB))
# BBBBB <- BBBBB*1.56
# BBBBB <- sum(BBBBB)
# BBBBBB <- c(dat$POINT_Z < (66.45402+1.32)) #add board hight and required depth
# BBBBBB <- c((66.45402+1.32-BBBBBB))
# BBBBBB <- BBBBBB*1.56
# BBBBBB <- sum(BBBBBB)
# BBBBBBB <- c(dat$POINT_Z < (66.45402+1.54)) #add board hight and required depth
# BBBBBBB <- c((66.45402+1.54-BBBBBBB))
# BBBBBBB <- BBBBBBB*1.56
# BBBBBBB <- sum(BBBBBBB)
# BBBBBBBB <- c(dat$POINT_Z < (66.45402+1.76)) #add board hight and required depth
# BBBBBBBB <- c((66.45402+1.76-BBBBBBBB))
# BBBBBBBB <- BBBBBBBB*1.56
# BBBBBBBB <- sum(BBBBBBBB)
# BBBBBBBBB <- c(dat$POINT_Z < (66.45402+1.98)) #add board hight and required depth
# BBBBBBBBB <- c((66.45402+1.98-BBBBBBBBB))
# BBBBBBBBB <- BBBBBBBBB*1.56
# BBBBBBBBB <- sum(BBBBBBBBB)
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# boards <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)
# plot(elevation, boards, xlab="Water Surface Elevation", ylab="Volume (m^3)", main = "Bluff Lake Volume", type="l", col="red", ylim=c(100500000,102000000), xlim=c(66.5,68.5))
# gam_1 <- gam(boards ~ s(elevation, bs = "cr"),
#              family = gaussian)
# summary(gam_1)
# gam_2 <- gam(elevation ~ s(volume, bs = "cr"),
#              family = gaussian)
# summary(gam_2)
# par(new=T)
# plot(gam_1$fitted.values~elevation, ylim=c(100500000,102000000), xlim=c(66.5,68.5), col="blue", type="l")
# 
# # ---- Waterbirds
# # areas must be less than 20 cm in depth
# Z <- length(which(dat$POINT_Z > (66.45402-.20))) #add board hight and required depth
# Z <- Z*1.56 #convert to area
# B <- length(which(dat$POINT_Z > (66.45402+.22-.20))) #add board hight and required depth
# B <- B*1.56 #convert to area
# BB <- length(which(dat$POINT_Z > (66.45402+.44-.20))) #add board hight and required depth
# BB <- BB*1.56 #convert to area
# BBB <- length(which(dat$POINT_Z > (66.45402+.66-.20))) #add board hight and required depth
# BBB <- BBB*1.56 #convert to area
# BBBB <- length(which(dat$POINT_Z > (66.45402+.88-.20))) #add board hight and required depth
# BBBB <- BBBB*1.56 #convert to area
# BBBBB <- length(which(dat$POINT_Z > (66.45402+1.1-.20))) #add board hight and required depth
# BBBBB <- BBBBB*1.56 #convert to area
# BBBBBB <- length(which(dat$POINT_Z > (66.45402+1.32-.20))) #add board hight and required depth
# BBBBBB <- BBBBBB*1.56 #convert to area
# BBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.54-.20))) #add board hight and required depth
# BBBBBBB <- BBBBBBB*1.56 #convert to area
# BBBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.76-.20))) #add board hight and required depth
# BBBBBBBB <- BBBBBBBB*1.56 #convert to area
# BBBBBBBBB <- length(which(dat$POINT_Z > (66.45402+1.98-.20))) #add board hight and required depth
# BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# WB <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
# WB<-rescale(WB, to=c(0,1))
# plot(elevation, WB, xlab="Water Surface Elevation", ylab="Hectares (<20cm)", main = "Waterbird Habitat", type="l", col="red")
# WBM <- gam(WB ~ s(elevation, bs = "cr"),
#              family = gaussian)
# par(new=T)
# plot(WBM$fitted.values~elevation, col="blue", type="l")
# # ---- Waterfowl
# # DED/m^2=0.4374
# # Dry Areas
# Z <- sum(dat$POINT_Z > (66.45402)) #add board hight and required depth
# Z <- Z*1.56*0.4374 #convert to DED per area
# B <- sum(dat$POINT_Z > (66.45402+.22)) #add board hight and required depth
# B <- B*1.56*0.4374
# BB <- sum(dat$POINT_Z > (66.45402+.44)) #add board hight and required depth
# BB <- BB*1.56*0.4374
# BBB <- sum(dat$POINT_Z > (66.45402+.66)) #add board hight and required depth
# BBB <- BBB*1.56*0.4374
# BBBB <- sum(dat$POINT_Z > (66.45402+.88)) #add board hight and required depth
# BBBB <- BBBB*1.56*0.4374
# BBBBB <- sum(dat$POINT_Z > (66.45402+1.1)) #add board hight and required depth
# BBBBB <- BBBBB*1.56*0.4374
# BBBBBB <- sum(dat$POINT_Z > (66.45402+1.32)) #add board hight and required depth
# BBBBBB <- BBBBBB*1.56*0.4374
# BBBBBBB <- sum(dat$POINT_Z > (66.45402+1.54)) #add board hight and required depth
# BBBBBBB <- BBBBBBB*1.56*0.4374
# BBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.76)) #add board hight and required depth
# BBBBBBBB <- BBBBBBBB*1.56*0.4374
# BBBBBBBBB <- sum(dat$POINT_Z > (66.45402+1.98)) #add board hight and required depth
# BBBBBBBBB <- BBBBBBBBB*1.56*0.4374
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# WF <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/1000000
# WF<-rescale(WF, to=c(0,1))
# plot(elevation, WF, xlab="Water Surface Elevation", ylab="DEDs (millions)", main = "Duck Energy Days", type="l", col="red")
# WFM <- gam(WF ~ s(elevation, bs = "cr"),
#              family = gaussian)
# par(new=T)
# plot(WFM$fitted.values~elevation, col="blue", type="l")
# # ---- Fish
# # Areas greater than 1 meter in depth
# Z <- sum(dat$POINT_Z < (66.45402-1)) #add board hight and required depth
# Z <- Z*1.56 #convert to area
# B <- sum(dat$POINT_Z < (66.45402-1+.22)) #add board hight and required depth
# B <- B*1.56 #convert to area
# BB <- sum(dat$POINT_Z < (66.45402-1+.44)) #add board hight and required depth
# BB <- BB*1.56 #convert to area
# BBB <- sum(dat$POINT_Z < (66.45402-1+.66)) #add board hight and required depth
# BBB <- BBB*1.56 #convert to area
# BBBB <- sum(dat$POINT_Z < (66.45402-1+.88)) #add board hight and required depth
# BBBB <- BBBB*1.56 #convert to area
# BBBBB <- sum(dat$POINT_Z < (66.45402-1+1.1)) #add board hight and required depth
# BBBBB <- BBBBB*1.56 #convert to area
# BBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.32)) #add board hight and required depth
# BBBBBB <- BBBBBB*1.56 #convert to area
# BBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.54)) #add board hight and required depth
# BBBBBBB <- BBBBBBB*1.56 #convert to area
# BBBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.76)) #add board hight and required depth
# BBBBBBBB <- BBBBBBBB*1.56 #convert to area
# BBBBBBBBB <- sum(dat$POINT_Z < (66.45402-1+1.98)) #add board hight and required depth
# BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# Fish <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
# Fish <-rescale(Fish, to=c(0,1))
# plot(elevation, Fish, xlab="Water Surface Elevation", ylab="Hectares (>1m)", main = "Fish Habitat", type="l", col="red")
# FishM <- gam(Fish ~ s(elevation, bs = "cr"),
#              family = gaussian)
# par(new=T)
# plot(FishM$fitted.values~elevation, col="blue", type="l")
# #### ---- Anglers
# # For now, following fish model, Areas greater than 1 meter in depth
# Adat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Anglers.csv")
# Z <- sum(Adat$POINT_Z < (66.45402-1)) #add board hight and required depth
# Z <- Z*1.56 #convert to area
# B <- sum(Adat$POINT_Z < (66.45402-1+.22)) #add board hight and required depth
# B <- B*1.56 #convert to area
# BB <- sum(Adat$POINT_Z < (66.45402-1+.44)) #add board hight and required depth
# BB <- BB*1.56 #convert to area
# BBB <- sum(Adat$POINT_Z < (66.45402-1+.66)) #add board hight and required depth
# BBB <- BBB*1.56 #convert to area
# BBBB <- sum(Adat$POINT_Z < (66.45402-1+.88)) #add board hight and required depth
# BBBB <- BBBB*1.56 #convert to area
# BBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.1)) #add board hight and required depth
# BBBBB <- BBBBB*1.56 #convert to area
# BBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.32)) #add board hight and required depth
# BBBBBB <- BBBBBB*1.56 #convert to area
# BBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.54)) #add board hight and required depth
# BBBBBBB <- BBBBBBB*1.56 #convert to area
# BBBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.76)) #add board hight and required depth
# BBBBBBBB <- BBBBBBBB*1.56 #convert to area
# BBBBBBBBB <- sum(Adat$POINT_Z < (66.45402-1+1.98)) #add board hight and required depth
# BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# Anglers <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
# Anglers<-rescale(Anglers, to=c(0,1))
# plot(elevation, Anglers, xlab="Water Surface Elevation", ylab="Hectares (>1m)", main = "Shoreline Fishing", type="l", col="red")
# AnglersM <- gam(Anglers ~ s(elevation, bs = "cr"),
#              family = gaussian)
# par(new=T)
# plot(AnglersM$fitted.values~elevation, col="blue", type="l")
# # Need to deliniate shorelines and establish "end" of boat ramp
# #areas >.5m in depth
# Bdat<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/Boat.csv")
# Z <- sum(Bdat$POINT_Z < (66.45402)) #add board hight and required depth
# Z <- Z*1.56 #convert to area
# B <- sum(Bdat$POINT_Z < (66.45402+.22-0.5)) #add board hight and required depth
# B <- B*1.56 #convert to area
# BB <- sum(Bdat$POINT_Z < (66.45402+.44-0.5)) #add board hight and required depth
# BB <- BB*1.56 #convert to area
# BBB <- sum(Bdat$POINT_Z < (66.45402+.66-0.5)) #add board hight and required depth
# BBB <- BBB*1.56 #convert to area
# BBBB <- sum(Bdat$POINT_Z < (66.45402+.88-0.5)) #add board hight and required depth
# BBBB <- BBBB*1.56 #convert to area
# BBBBB <- sum(Bdat$POINT_Z < (66.45402+1.1-0.5)) #add board hight and required depth
# BBBBB <- BBBBB*1.56 #convert to area
# BBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.32-0.5)) #add board hight and required depth
# BBBBBB <- BBBBBB*1.56 #convert to area
# BBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.54-0.5)) #add board hight and required depth
# BBBBBBB <- BBBBBBB*1.56 #convert to area
# BBBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.76-0.5)) #add board hight and required depth
# BBBBBBBB <- BBBBBBBB*1.56 #convert to area
# BBBBBBBBB <- sum(Bdat$POINT_Z < (66.45402+1.98-0.5)) #add board hight and required depth
# BBBBBBBBB <- BBBBBBBBB*1.56 #convert to area
# elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
#                67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
# Ramp <- c(Z, B, BB, BBB, BBBB, BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)
# Ramp<-rescale(Ramp, to=c(0,1))
# plot(elevation, Ramp, xlab="Water Surface Elevation", ylab="Wetted Area (m^2)", main = "Ramp Access", type="l", col="red")
# RampM <- gam(Ramp ~ s(elevation, bs = "cr"),
#                 family = gaussian)
# par(new=T)
# plot(RampM$fitted.values~elevation, col="blue", type="l")
# 
# 
# #Utilities
# dat2$elevation<-predict(gam_2, dat2)
# dat2$WFUt<-predict(WFM, dat2)
# dat2$WBUt<-predict(WBM, dat2)
# dat2$FishUt<-predict(FishM, dat2)
# dat2$AngUt<-predict(AnglersM, dat2)
# dat2$RampUt<-predict(RampM, dat2)
# 
# dat2$WFUt <- ifelse(dat2$WFUt< 0, 0, dat2$WFUt)
# dat2$WBUt<- ifelse(dat2$WBUt < 0, 0, dat2$WBUt)
# dat2$FishUt <- ifelse(dat2$FishUt  < 0, 0, dat2$FishUt )
# dat2$AngUt <- ifelse(dat2$AngUt < 0, 0, dat2$AngUt)
# dat2$RampUt <- ifelse(dat2$RampUt < 0, 0, dat2$RampUt)
# 
# dat2$Utility<- (dat2$RampUt)+(dat2$AngUt)+(dat2$FishUt)+(dat2$WBUt)+(dat2$WFUt)+(1)
# 
# write.csv(dat2, "~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9Boards.csv")
# 
# 
# #weekly average
# dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9Boards.csv")
# dat2<-dat2 %>%
#   filter(!is.na(Utility))
# dat2$Date <- as.Date(dat2$Date)
# dat2$Date<-lubridate::round_date(dat2$Date, "7 days")
# dat2<-dat2 %>%
#   group_by(Date) %>%
#   summarize(mUtility = mean(Utility, na.rm = T),
#             mWFUt = mean(WFUt, na.rm = T),
#             mWBUt= mean(WBUt, na.rm = T),
#             mFishUt = mean(FishUt, na.rm = T),
#             mAngUt = mean(AngUt, na.rm = T),
#             mRampUt = mean(RampUt, na.rm = T))
# 
# dat2$month<-lubridate::month(dat2$Date)
# dat2$day<-lubridate::day(dat2$Date)
# dat2$month_day<-paste(dat2$month,dat2$day,sep="-")
# dat2<-dat2 %>%
#   group_by(month,day) %>%
#   summarize(mUtility = mean(mUtility, na.rm = T),
#             mWFUt = mean(mWFUt, na.rm = T),
#             mWBUt= mean(mWBUt, na.rm = T),
#             mFishUt = mean(mFishUt, na.rm = T),
#             mAngUt = mean(mAngUt, na.rm = T),
#             mRampUt = mean(mRampUt, na.rm = T))
# 
# 
# ggplot(dat2, aes(day, mUtility, group = month, color = month)) +
#   geom_line( )+
#   labs(title="Monthly Utility of 11.3cms Paddlefish Discharge",
#        y="Utility",
#        x="Day of Month"
#   )
# write.csv(dat2, "~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9BoardsUtilities.csv")

#Make Monthly Plots
One<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_0cms_discharge/9BoardsUtilities.csv")
One$Discharge<- rep("0 cms",nrow(One))
Two<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_2.825cms_discharge/9BoardsUtilities.csv")
Two$Discharge<- rep("2.825 cms",nrow(Two))
Three<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_5.65cms_discharge/9BoardsUtilities.csv")
Three$Discharge<- rep("5.65 cms",nrow(Three))
Four<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_8.475cms_discharge/9BoardsUtilities.csv")
Four$Discharge<- rep("8.475 cms",nrow(Four))
Five<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_11.3cms_discharge/9BoardsUtilities.csv")
Five$Discharge<- rep("11.3 cms",nrow(Five))
# Six<-read.csv("~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_16.95cms_discharge/9BoardsUtilities.csv")
# Six$Discharge<- rep(6,nrow(Six))

data<-rbind(One,Two,Three,Four,Five)
Jan<-subset(data, month==1)
ggplot(Jan, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="January- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month")+
  scale_colour_discrete(
            breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
            labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Feb<-subset(data, month==2)
ggplot(Feb, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="Feb- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Mar<-subset(data, month==3)
ggplot(Mar, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="March- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Apr<-subset(data, month==4)
ggplot(Apr, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="April- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
May<-subset(data, month==5)
ggplot(May, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="May- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Jun<-subset(data, month==6)
ggplot(Jun, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="June- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Jul<-subset(data, month==7)
ggplot(Jul, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="July- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Aug<-subset(data, month==8)
ggplot(Aug, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="August- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Sep<-subset(data, month==9)
ggplot(Sep, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="September- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Oct<-subset(data, month==10)
ggplot(Oct, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="October- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Nov<-subset(data, month==11)
ggplot(Nov, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="November- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))
Dec<-subset(data, month==12)
ggplot(Dec, aes(day, mUtility, group = Discharge, color = Discharge)) +
  geom_line( )+
  labs(title="December- Utility of Paddlefish Discharges",
       y="Utility",
       x="Day of Month"
  )+
  scale_colour_discrete(
    breaks=c("0 cms", "2.825 cms", "5.65 cms", "8.475 cms", "11.3 cms"),
    labels=c("0", "2.825", "5.65", "8.475", "11.3"))