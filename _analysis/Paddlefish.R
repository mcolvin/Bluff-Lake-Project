library(tidyverse)
library(lubridate)
# 
# dat<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DailyMacon01011945_01012020.csv")
# dat$ï..datetime <- as.POSIXct(dat$ï..datetime, format="%m/%d/%Y")
# dat$month<- lubridate::month(as.POSIXct(dat$ï..datetime, format="%Y/%m/%d"), label = T)
# dat$year<-lubridate::year(dat$ï..datetime)
# dat$day<-lubridate::day(dat$ï..datetime)
# dat$DeltaCMS<-c(NA,diff(dat$Discharge_cms))
# dat$meanCMS<-dat$Discharge_cms
# 
# #####
# dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon15min.csv")
# dat2$Date.Time <- as.POSIXct(dat2$Date.Time, format="%m/%d/%Y %H:%M")
# dat2$month<- lubridate::month(as.POSIXct(dat2$Date.Time, format="%Y/%m/%d %H:%M"), label = T)
# dat2$Date.Time<-round_date(dat2$Date.Time, "30 mins")
# CMS<-ddply(dat2, c("Date.Time","month"), summarize,
#            meanCMS=mean(ï..CFS))
# Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEL.csv")
# Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
# Elevation$Date.Time <- (as.POSIXct(Elevation$Date.Time, format="%m/%d/%Y %H:%M"))
# Elevation$month<- lubridate::month(as.POSIXct(Elevation$Date.Time, format="%Y/%m/%d %H:%M"), label= T)
# data<-merge(Elevation, CMS)
# 
# M3<-lm(WSElevation~meanCMS+month+ meanCMS*month,data)
# summary(M3)
# 
# #try GAM
# library(mgcv)
# library(data.table)
# #group data by day
# data$Date.Time<-round_date(data$Date.Time, "day")
# data<-data %>% dplyr::group_by(data$Date.Time) %>% 
#   dplyr::summarise_all(funs(mean))
# data2<-data
# #differce between days elevation
# data2$DeltaEle<-c(NA, diff(data$WSElevation))
# #convert Elevation to volume
# data2$Vol<-(56446953+(577084*data2$WSElevation))
# #change in volume
# data2$DeltaVol<-c(NA,diff(data2$Vol))
# #difference between days CMS
# data2$DeltaCMS<-c(NA,diff(data$meanCMS))
# #add month back
# data2$month<- lubridate::month(as.POSIXct(data2$Date.Time, format="%m/%d/%Y"), label= T)
# #Get rid of Negatives
# data4<-data2
# data4 <- filter(data4, DeltaVol>0)
# data4 <- filter(data4, DeltaCMS>0)
# Posgam <- gam(DeltaVol ~ s(meanCMS),
#               data = data4,
#               family = gaussian)
# summary(Posgam)
# data5<-data2
# data5 <- filter(data5, DeltaVol<0)
# data5 <- filter(data5, DeltaCMS<0)
# Neggam <- gam(DeltaVol ~ s(meanCMS),
#               data = data5,
#               family = gaussian)
# summary(Neggam)
# 
# ####Now use GAM to predict change in volume given discharge
# datP <- filter(dat, DeltaCMS>0)
# datP$DeltaVol<-predict(Posgam, datP)
# datN <- filter(dat, DeltaCMS<0)
# datN$DeltaVol<-predict(Posgam, datN)
# data<-rbind(datP, datN)
# 
# ####Now we need a few different starting board elevations/volumes
# startVol<-c(94620866,94917089,95187732,95354223,
#             95370440,95390603,95458173,95581226,95761001,96036563)
# # volume_{next day} = volume_{previous day} + [change in volume]-[Paddlefish release]

library(tidyverse)
library(lubridate)

fish=read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/PADDLE_DISCHARGE.csv")

str(fish)

fish$Date<-as.POSIXct(strptime(as.character(fish$Date),"%Y-%m-%d"))
fish$month<-as.numeric(format(fish$Date, "%m"))
fish$year<-as.numeric(format(fish$Date, "%Y"))
str(fish)
fish<-fish %>% mutate(Date=ymd(Date)) #make this the proper format
str(fish)

a=fish %>% dplyr::group_by(year, month) %>% dplyr::summarise(Date=max(Date)) #get the max date for each month-year

fish2=data.frame()#create an empty data frame

for(i in 1:nrow(a)){
  b=data.frame(Date=seq(ymd(paste(a$year[i], a$month[i], "01", sep="-")), a$Date[i], 1))
  fish2=rbind(fish2, b)
} #makes sure there is each day in each month in each year


fish3=left_join(fish2, fish) 
fish3$Date2<-as.POSIXct(strptime(as.character(fish3$Date),"%Y-%m-%d"))
fish3$month<-as.numeric(format(fish3$Date2, "%m"))
fish3$year<-as.numeric(format(fish3$Date2, "%Y"))
fish3$day<-as.numeric(format(fish3$Date2, "%d"))

fish3=mutate(fish3, fishD=ifelse(day==1|day==15, 325600, 0)) #for the 1st and 15th create the discharges

#we are stuck here
fish3=mutate(fish3, volChange=ifelse(day==1, 94620866+DeltaVol-fishD, DeltaVol-fishD)) #do the calculations from the first of each month
fish3=na.omit(fish3) #get rid of those extra rows you made
fish3=fish3 %>% dplyr::group_by(year, month) %>% dplyr::mutate(current=cumsum(volChange)) #get the cumulative sum
