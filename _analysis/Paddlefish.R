library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(dplyr)
library(mgcv)
# #
#  dat<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DailyMacon01011945_01012020.csv")
#  dat$ï..datetime <- as.POSIXct(dat$ï..datetime, format="%m/%d/%Y")
#  names(dat)[1]<-"Date"
#  #add in missing dates
#  Date<-seq(as.Date("1945/1/1"), as.Date("2019/12/31"), "days")
#  Date<-as.data.frame(Date)
#  Date$Date<-as.POSIXct(Date$Date, format="%Y/%m/%d")
#  Date$Date<-round_date(Date$Date, "day")
#  library(dplyr)
#  dat<-left_join(Date, dat, by = "Date")
#  dat$Discharge_cfs<-na.approx(dat$Discharge_cfs)
#  dat$Discharge_cms<-dat$Discharge_cfs*0.028316847
# 
#  dat$month<- lubridate::month(as.POSIXct(dat$Date, format="%Y/%m/%d"), label = T)
#  dat$year<-lubridate::year(dat$Date)
#  dat$day<-lubridate::day(dat$Date)
#  dat$DeltaCMS<-c(NA,diff(dat$Discharge_cms))
#  dat$meanCMS<-dat$Discharge_cms
# 
# #
# #####
#  dat2<- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/DischargeDataMacon15min.csv")
#  dat2$Date.Time <- as.POSIXct(dat2$Date.Time, format="%m/%d/%Y %H:%M")
#  dat2$month<- lubridate::month(as.POSIXct(dat2$Date.Time, format="%Y/%m/%d %H:%M"), label = T)
#  dat2$Date.Time<-round_date(dat2$Date.Time, "30 mins")
#  CMS<-dplyr::ddply(dat2, c("Date.Time","month"), summarize,
#             meanCMS=mean(ï..CFS))
#  Elevation <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Level_loggersEL.csv")
#  Elevation <- subset(Elevation, Elevation$ï..Location == "Cypress")
#  Elevation$Date.Time <- (as.POSIXct(Elevation$Date.Time, format="%m/%d/%Y %H:%M"))
#  Elevation$month<- lubridate::month(as.POSIXct(Elevation$Date.Time, format="%Y/%m/%d %H:%M"), label= T)
#  data<-merge(Elevation, CMS)
# 
#  M3<-lm(WSElevation~meanCMS+month+ meanCMS*month,data)
#  summary(M3)
# 
# # #try GAM
#  library(mgcv)
#  library(data.table)
#  #group data by day
#  data$Date.Time<-round_date(data$Date.Time, "day")
#  data<-data %>% dplyr::group_by(data$Date.Time) %>%
#    dplyr::summarise_all(funs(mean))
#  data2<-data
# 
#  #differce between days elevation
#  data2$DeltaEle<-c(NA, diff(data2$WSElevation))
#  #convert Elevation to volume
#  data2$elevation<-data2$WSElevation
#  data2$Vol<- predict(gam_1, data2)
#   #change in volume
#  data2$DeltaVol<-c(NA,diff(data2$Vol))
#  #difference between days CMS
#  data2$DeltaCMS<-c(NA,diff(data2$meanCMS))
#  #add month back
#  data2$month<- lubridate::month(as.POSIXct(data2$Date.Time, format="%m/%d/%Y"), label= T)
# # #Get rid of Negatives
#  data4<-data2
#  data4 <- filter(data4, DeltaVol>0)
#  data4 <- filter(data4, DeltaCMS>0)
#  Posgam <- gam(DeltaVol ~ s(meanCMS),
#                data = data4,
#                family = gaussian)
#  summary(Posgam)
#  data5<-data2
#  data5 <- filter(data5, DeltaVol<0)
#  data5 <- filter(data5, DeltaCMS<0)
#  Neggam <- gam(DeltaVol ~ s(meanCMS),
#                data = data5,
#                family = gaussian)
# summary(Neggam)
# 
# ####Now use GAM to predict change in volume given discharge
# datP <- filter(dat, DeltaCMS>0)
# datP$DeltaVol<-predict(Posgam, datP)
# datN <- filter(dat, DeltaCMS<0)
# datN$DeltaVol<-predict(Neggam, datN)
# data<-rbind(datN, datP)
# names(data)[1]<-"Date"




# write.csv(data, "~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/PADDLE_DISCHARGE22.csv")

fish=read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/PADDLE_DISCHARGE22.csv")

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
#interpolate missing values
fish3$DeltaVol<-na.spline(fish3$DeltaVol)
fish3$Date2<-as.POSIXct(strptime(as.character(fish3$Date),"%Y-%m-%d"))
fish3$month<-as.numeric(format(fish3$Date2, "%m"))
fish3$year<-as.numeric(format(fish3$Date2, "%Y"))
fish3$day<-as.numeric(format(fish3$Date2, "%d"))

fish3=mutate(fish3, fishD=ifelse(day==1|day==7|day==14|day==21|day==28, 0, 0)) #for the 1st and 15th create the discharges



## COMBINE YEAR AND MONTH TO LOOP OVER 
fish3$yr_month<-paste(fish3$year,fish3$month,sep="-")
fish3<-fish3[order(fish3$Date2),]
## YEAR MONTH COMBINATIONS TO LOOP OVER
combos<-unique(fish3$yr_month)
combos<- combos[-length(combos)]## DROP 2020-01 ONLY 1 DAY THERE

fish3$volume<-NA  ## KEEP TRACK OF VOLUME THAT CHANGES DAILY AS VOL[PREV DAY] + IN - OUT
for(i in 1:length(combos))
    {
    indx<-which(fish3$yr_month==combos[i]) # ROW INDICES FOR THE YEAR MONTH COMBINATION SHOULD BE A VECTOR OF ~ 30 VALUES
    ## FIRST DAY
    first_day_row_indx<- indx[1]
    fish3$volume[first_day_row_indx]<-101976480
    ## INITIAL VOLUME AT THE FIRST OF THE MONTH-
    #100447697 100762656 101047557 101227827 101257957 
    #101294378 101377521 101514389 101703873 101976480
    ## LOOP OVER REMAINING DAYS IN THE MONTH
    for(j in 2:length(indx))
        {
        current_day<- indx[j]
        day_before<- indx[j-1]
        fish3$volume[current_day]<- fish3$volume[day_before]+fish3$DeltaVol[current_day]-fish3$fishD[current_day]
        }
    }

fish3$elevation<-predict(gam_2, fish3)
write.csv(fish3, "~/GitHub/Bluff-Lake-Project/_analysis/noxubee-discharge-states/paddlefish-discharges/weekly_0cms_discharge/9Boards.csv")

theme_set(theme_classic())

Jan<-subset(fish3, month==1)
ggplot(Jan, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="January Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
       ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")
  


Feb<-subset(fish3, month==2)
ggplot(Feb, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="February Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Mar<-subset(fish3, month==3)
ggplot(Mar, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="March Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Apr<-subset(fish3, month==4)
ggplot(Apr, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="April Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

May<-subset(fish3, month==5)
ggplot(May, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="May Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Jun<-subset(fish3, month==6)
ggplot(Jun, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="June Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Jul<-subset(fish3, month==7)
ggplot(Jul, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="July Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Aug<-subset(fish3, month==8)
ggplot(Mar, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="August Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Sep<-subset(fish3, month==9)
ggplot(Sep, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="September Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Oct<-subset(fish3, month==10)
ggplot(Oct, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="October Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Nov<-subset(fish3, month==11)
ggplot(Nov, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="November Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

Dec<-subset(fish3, month==12)
ggplot(Dec, aes(day, volume, group = year, color = year)) + 
  geom_line( )+
  labs(title="December Time Series of Lake Volume", 
       y="Volume m^3",
       x="Day of Month"
  ) +
  ylim(99000000, 103500000)+
  geom_hline(yintercept=101976479)+
  geom_hline(yintercept=101703873)+
  geom_hline(yintercept=101514389)+
  geom_hline(yintercept=101377521)+
  geom_hline(yintercept=101294378)+
  geom_hline(yintercept=101257987)+
  geom_hline(yintercept=101227827)+
  geom_hline(yintercept=101047557)+
  geom_hline(yintercept=100762655)+
  geom_hline(yintercept=100447696, col="red")

