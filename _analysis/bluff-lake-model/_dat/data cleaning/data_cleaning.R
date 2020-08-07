library(data.table)
library(lubridate)
data<-read.csv("_dat/Level_logger.csv")
data$logger<-data$ï..ï..Location
data<-as.data.table(data)
data$Date.Time<-as.POSIXct(data$Date.Time, format="%m/%d/%y %H:%M")
data$year<-as.numeric(format(data$Date.Time, "%Y"))
data$doy<-strftime(data$Date.Time, format = "%j")
data$decimal.time <- hour(data$Date.Time) + minute(data$Date.Time)/60
tmp<-dcast(data, year+doy+decimal.time~logger,
           value.var="WSE", fun.aggregate =mean)
n<-dim(tmp)[1]
tmp<-tmp[1:(n-1),]

