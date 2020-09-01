library(lubridate)
library(data.table)
library(dplyr)
data<-read.csv("Aquacam_Scores.csv")
data2<-read.csv("Level_logger_wide.csv")
data$location<-data$ï..Location
data<-as.data.table(data)
data$Start.Date.Time<-as.POSIXct(data$Start.Date.Time, format="%m/%d/%Y %H:%M")
data$End.Date.Time<-as.POSIXct(data$End.Date.Time, format="%m/%d/%Y %H:%M")
data2$End.Date.Time<-as.POSIXct(data2$Date.Time, format="%Y-%m-%d %H:%M")
data2$Start.Date.Time<-data2$End.Date.Time
data2<-data2[,6:10]

data3<- merge(data, data2, by="Start.Date.Time")

data2$Cypress2<-data2$Cypress
data2$Gauge2<-data2$Gauge
data2$Intake2<-data2$Intake
data2$End.Date.Time.x<-data2$End.Date.Time

data3<- merge(data3, data2, by="End.Date.Time.x")

data4<-read.csv("discharge_hourly.csv")
data4$dateTime<-as.POSIXct(data4$dateTime, format="%Y-%m-%d %H:%M:%S")
data4$STdateTime<-strftime(data4$dateTime, "%Y-%m-%d %H")
data4$ENDdateTime<-data4$STdateTime
data3$STdateTime<-strftime(data3$Start.Date.Time.x, "%Y-%m-%d %H")
data3$ENDdateTime<-strftime(data3$End.Date.Time.x, "%Y-%m-%d %H")

data3<- merge(data3, data4, by="STdateTime")
data4$discharge2<-data4$discharge
data4$ENDdateTime.x<-data4$ENDdateTime
data3<-merge(data3, data4, by="ENDdateTime.x")

LocationScores<-data3 %>% group_by(location, Score) %>% summarise(mean(Cypress.x), mean(Cypress2), mean(discharge.x), mean(discharge2))

