library(tidyverse)
library(mgcv)
library(suncalc)
library(openxlsx)
library(lubridate)
library(data.table)

setwd("~/GitHub/Bluff-Lake-Project/_analysis")

data<-read.xlsx("bluff-lake-model/_dat/Level_logger.xlsx")
names(data)<-c("ID","location","date_time","pressure", "temp_c","baro","water_level","wse")
# open xlsx convertToDateTime fails on big datasets...
data$dt <- as.POSIXct(data$date_time*3600*24, tz="GMT", origin = "1899-12-30")
data$dt<- as.POSIXct(strftime(data$dt, format = "%Y/%m/%d %H:%M"))
data$dt<-round_date(data$dt, "30 mins")
data<-as.data.table(data)
data$year<-as.numeric(format(data$dt, "%Y"))
data$doy<-strftime(data$dt, format = "%j")
data$hour <- hour(data$dt)
data$minute<-minute(data$dt)
data$days<-strftime(data$dt, format = "%Y-%m-%d")
data$days<-as.Date(data$days)
lon<- c(-88.783631)
lat<- c(33.279118)
# data2<-subset(data,data$days>="2019-05-07"&data$days<="2020-05-07")
# data3<-subset(data,data$location=="Intake")
# data<-rbind(data2,data3)

days<-unique(data$days)
dates<-as.data.frame(days)
dates$dusk<-NA
dates$dawn<-NA


for(i in 1:nrow(dates)){
  sunset<-getSunlightTimes(date = dates$days[i], lat = lat, lon = lon, data = NULL,
                           keep = c("dawn", "dusk"), tz = "America/Chicago")
  dates$dusk[i]<-as.character(sunset$dusk)
  dates$dawn[i]<-as.character(sunset$dawn)
}

dates$dusk<-as.POSIXct(dates$dusk)
dates$dawn<-as.POSIXct(dates$dawn)
dates$days<-as.Date(dates$days)

dates$dusk<-round_date(dates$dusk, "30 mins")
dates$dawn<-round_date(dates$dawn, "30 mins")


merge<-merge(dates, data)

mergeSub<-subset(merge, merge$dusk==merge$dt)
mergeSub$doy<-as.numeric(mergeSub$doy)
mergeSub$value <- paste(mergeSub$location, mergeSub$year, sep="_")

ggplot(mergeSub, aes(doy,temp_c, color=value)) + geom_line() + 
  labs(y = "Temp", x = "DOY")+   
  theme_classic()
temp<-approxfun(data$temp_c~data$doy)


