library(tidyverse)
library(mgcv)
data<-read.xlsx("_dat/Level_logger.xlsx")
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

ggplot(data, aes(doy,temp_c, group=year, color=location)) + geom_line() + 
  labs(y = "Temp", x = "DOY")+   
  theme_classic()
temp<-approxfun(data$temp_c~data$doy)
fit<-temp(data$doy)

