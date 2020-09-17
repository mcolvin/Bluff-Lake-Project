library(data.table)
library(lubridate)
data<-read.xlsx("_dat/Level_logger.xlsx")
names(data)<-c("location","date_time","pressure", "temp_c","baro","water_level","wse")
# open xlsx convertToDateTime fails on big datasets...
data$dt <- as.POSIXct(data$date_time*3600*24, tz="GMT", origin = "1900-01-01")
data$dt<-round_date(data$dt, "15 mins")
data<-as.data.table(data)
data$year<-as.numeric(format(data$dt, "%Y"))
data$doy<-strftime(data$dt, format = "%j")
data$decimal.time <- hour(data$dt) + minute(data$dt)/60
tmp<-dcast(data, dt+year+doy+decimal.time~location,
           value.var="wse", fun.aggregate =mean)
n<-dim(tmp)[1]
tmp<-tmp[1:(n-1),]

write.csv(tmp,"_dat/Level_logger_wide.csv")
