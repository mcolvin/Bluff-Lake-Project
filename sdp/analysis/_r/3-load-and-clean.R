

data(lake_meta)
data(drawdown_dat)
drawdown_cycle<-subset(drawdown_dat,cycle==1)
#plot(board_elevation~start_date,drawdown_cycle,type="s")
drawdown_cycle[,drawdown_cycle_id:=.I]

#----------------------------------------------------------------------
# 
#---NOXUBEE RIVER DAILY DISCHARGE DATA

# PULL DISCHARGE AND GAUGE DATA FROM USGS PORTAL
## PARAMETER CODE 00065 Gage height, feet 00060; Discharge, cubic feet per second
discharge_daily<-fread("_dat/discharge_daily.csv")
discharge_daily[,date:=as.Date(datetime)]
tmp<-as.numeric(Sys.Date()-as.Date(max(discharge_daily$date)))
if(tmp>15)# pull data again if more than 15 days have passed since last pull
    {
    current_date<- Sys.Date()
    path<-paste0("https://waterdata.usgs.gov/nwis/dv?cb_00060=on&cb_00065=on&format=rdb&site_no=02448000&referred_module=sw&period=&begin_date=1938-09-21&end_date=",current_date)
    discharge_daily <- read.table(url(path),sep="\t",skip = 32,header=FALSE)
    names(discharge_daily)<- c("agency_cd","site_no","datetime","gage","78518_00065_00003_cd",
        "discharge","78519_00060_00003_cd")
    discharge_daily<-as.data.table(discharge_daily)
    discharge_daily[,date:=as.Date(datetime)]
    discharge_daily[,year:=as.numeric(format(date,"%Y"))]
    discharge_daily[,doy:=as.numeric(format(date,"%j"))]
    fwrite(discharge_daily,"_dat/discharge_daily.csv")
    }
# scale discharge to watershed area m^3/second
discharge_daily[,Q_bl:=(discharge/lake_meta$bluff_lake)*0.0283168]


#----------------------------------------------------------------------
# 
#  NOXUBEE RIVER DISCHARGE DATA: HOURLY
#
#----------------------------------------------------------------------
# PULL DISCHARGE AND GAUGE DATA FROM USGS PORTAL
## PARAMETER CODE 00065 Gage height, feet 00060; Discharge, cubic feet per second
discharge_hourly<-fread("_dat/discharge_hourly.csv")
discharge_hourly[,date:=as.Date(date)]
tmp<-as.numeric(Sys.Date()-as.Date(max(discharge_hourly$date)))
# pull data again if more than 15 days have passed since last pull
if(tmp>15)
    {    
    discharge_hourly <- dataRetrieval::readNWISuv(siteNumbers = "02448000",
        parameterCd = c("00060","00065"),
        startDate = as.Date("1986-10-10"),
        endDate = Sys.Date(),
        tz="America/Chicago")
    discharge_hourly<-as.data.table(discharge_hourly)        
    names(discharge_hourly)[4]<-"discharge"
    names(discharge_hourly)[6]<-"gage"
    discharge_hourly[,date:=as.Date(dateTime)]
    discharge_hourly[,year:=as.numeric(format(date,"%Y"))]
    discharge_hourly[,doy:=as.numeric(format(date,"%j"))]
    fwrite(discharge_hourly,"_dat/discharge_hourly.csv")
    }
    
## scale discharge to watershed area m^3/second
discharge_hourly[,Q_bl:=(discharge/lake_meta$bluff_lake)*0.0283168]
# round to nearest hour
discharge_hourly[,dateTime:=round_date(dateTime, "1 hour")]
## summarize 15 minute intervals to hour
discharge_hourly<-discharge_hourly[,.(Q_bl=mean(Q_bl),
    discharge=mean(discharge)),by=.(dateTime)]
## extract hour
discharge_hourly[,hour:=hour(dateTime)]
## extract minute
discharge_hourly[,minute:=minute(dateTime)]
discharge_hourly[,doy:=yday(dateTime)]
discharge_hourly[,year:=year(dateTime)]
discharge_hourly[,discharge_cms:=discharge_hourly$discharge*0.0283168]






#----------------------------------------------------------------------
# 
#  WATER LEVEL LOGGER DATA
#
#----------------------------------------------------------------------
# Bluff Lake WSE data begins 05/07/2019
#data<-as.data.table(read.xlsx("_dat/Level_logger2.xlsx",sheet="Level_logger"))
data<-as.data.table(read_excel("_dat/Level_logger2.xlsx",sheet="Level_logger"))
names(data)<-c("ID","location","date_time","pressure", "temp_c","baro","water_level","wse")
#data[,dt:=as.POSIXct(date_time*3600*24, tz="GMT", origin = "1899-12-30")]
#data[,dt:=as.POSIXct(strftime(dt, format = "%Y/%m/%d %H:%M"))]
data[,dt:=round_date(date_time, "30 mins")]
data[,year:=as.numeric(format(dt, "%Y"))]
data[,doy:=as.numeric(strftime(dt, format = "%j"))]
data[,hour:=hour(dt)]
data[,minute:=minute(dt)]

loggers<-dcast(data, dt+year+doy+hour+minute~location,
           value.var="wse", fun.aggregate=mean)
loggers[,doy:=as.numeric(loggers$doy)]

#  merge discharge and wse
# several fields to include: year, doy, hour, wse, discharge (@macon, adjusted to watershed, in cms as units)
lake_info <- merge(loggers, 
    discharge_hourly[,.SD,
        .SDcols=c("year", "doy", "hour", "minute","discharge_cms", "Q_bl")],
    by=c("year", "doy", "hour", "minute"), all.x = TRUE)

# make a field for 'continuous time' which is a fractional day starting at 0 
# for the first row of data an increasing fractionally for each hour and minute 
# (i.e., 5:30 am would be 330 minutes in, 330/1440 = 0.2291667, the same time 
# on the next day would be 1.2291667)
lake_info[,cont_time:=((year-2019)*525600)+(doy*1440)+(hour*60)+
                      (minute)-183300]
lake_info[,Cypress:=ifelse(is.nan(Cypress),NA,Cypress)]
lake_info[,Gauge:=ifelse(is.nan(Gauge),NA,Gauge)]
lake_info[,Intake:=ifelse(is.nan(Intake),NA,Intake)]


#----------------------------------------------------------------------
# 
#  dataset for gam analysis
#
#----------------------------------------------------------------------
#  elevation of water at intake versus Macon
#  run lake_info and discharge_daily from load-and-clean
newdat<- subset(lake_info, dt> as.POSIXct("2019/11/12 18:00")) 
newdat<-subset(newdat, is.na(newdat$Q_bl)==FALSE)
newdat<-subset(newdat, is.na(newdat$Intake)==FALSE)
matrix_gam <- as.data.table(newdat)





#----------------------------------------------------------------------
#
#  make interpolation functions
#
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Functions for converting elevation to volume or volume to elevation 
#----------------------------------------------------------------------
#elevation in meters above sea level
boards<-c(-10:-1,0:17)
elevation<-66.568+(0.2032*boards) #added additional elevation up to ~70m
volume<-rep(NA,length(elevation))
for(i in 1:length(elevation))
    {
    # points less than the elevation
    Z <- subset(bath$elevation, bath$elevation < elevation[i]) 
    # how much water is in the cell (i.e., depth)
    Z <- c((elevation[i]-Z)) 
    # multiply depth by cell area to get cell specific volumne
    Z <- Z*4 # 4 m^2 area of cells
    # sum to get total volume
    volume[i]<-sum(Z)
    }
volume[2]<-0.001  # fix for interpolation function
EL_2_Vol<- approxfun(elevation, volume, rule=2)
Vol_2_EL<- approxfun(volume, elevation, rule=2)

# plot(elevation,volume/1000000) # matches what vrs had in report/thesis

# Function for converting elevation or volume to surface area ----
# surface area in meters squared
# volume in cubic meters
# elevation in meters above sea level
surface <- NA
for(i in 1:length(elevation))
    {
    Z <- subset(bath$elevation, bath$elevation < (elevation[i]))
    surface[i]<-length(Z)*4
    }
surface[2]<-0.001# fix for interpolation function
Vol_2_SA<-approxfun(volume,surface,  rule=2)
EL_2_SA<-approxfun(elevation,surface,  rule=2)


#----------------------------------------------------------------------
# 
#  setup for simulations
#
#----------------------------------------------------------------------
# ---- gam analysis to predict inflow to bluff lake
gam_4 <- gam(Intake ~ te(Q_bl, doy),
     data = matrix_gam,
     family = gaussian)
#summary(gam_4)
#summary(gam_4)$s.table
#    newdat$FitG4<- gam_4$fitted.values
#    newdat<-newdat[order(doy)]
# predict intake elevation for each daily discharge from macon
# discharge_daily$Pred_El<-predict(gam_4, discharge_daily)


# discharge for pfish are done weekly for an 8 hour period
# discharges 
drawdown_cycle[,duration:=end_doy-start_doy]
D_Q<- c(0, 2.8, 5.6, 8.5, 11.3, 14.1, 17) # cubic meters per second
D_Q<-D_Q*60*60 # discharge, cubic meters per hour
release_timing<- as.data.table(expand.grid(doy=c(1:365),hour=c(1:24)))
setorder(release_timing,doy, hour)
release_timing[,id:=.I]
release_timing[,release:=0]
setDT(release_timing)[doy%in% seq(1,356,by=7) & hour %in% c(9:17),release:=1]
releases<-approxfun(release_timing$id,release_timing$release,method = "constant")


#----------------------------------------------------------------------
#
# Functions for utilities
#
#----------------------------------------------------------------------

# ---- Waterbirds
# areas must be less than 20 cm in depth
WB<-NA
boards<-c(0:17)
elevation<-round(66.568+(0.2032*boards),2)
for(i in 1:length(elevation)){
  Z <- length(which(bath$elevation > (elevation[i]-.20)))
  Z <- Z*4
  WB[i]<-sum(Z)/10000
}
utility_data<-data.frame(WB, elevation)

# ---- Waterfowl
# DED/m^2=0.4374
# Dry Areas
WF<-NA

for(i in 1:length(elevation))
    {
    Z <- length(which(bath$elevation > (elevation[i])))
    Z <- Z*4*0.4374 #convert to DED per area
    WF[i]<-sum(Z)/1000000
    }
utility_data$WF<-WF

# ---- Fish
# Areas greater than 1 meter in depth
Fish<-fread("_dat/combos-fast2.csv")
Fish<- Fish[,.(Fish5=mean(Vol5)),by=.(elevation)]
Fish<-approxfun(Fish$elevation,Fish$Fish5,rule=2)
#### ---- Anglers
# For now, following fish model, Areas greater than 1 meter in depth

#Adat<-read.csv("./Depth-Mapping/_dat/Bathymetry/Anglers.csv")
Adat<-fread("_dat/Anglers.csv")
Anglers<-NA
for(i in 1:length(elevation))
    {
    Z <- length(which(Adat$POINT_Z < (elevation[i]-1)))
    Z <- Z*4 
    Anglers[i]<-sum(Z)/10000
    }
utility_data$bank_anglers<-Anglers


# Need to delineate shorelines and establish "end" of boat ramp
#areas >.5m in depth
#Bdat<-read.csv("./Depth-Mapping/_dat/Bathymetry/Boat.csv")
Bdat<-read.csv("_dat/Boat.csv")
Ramp<-NA
for(i in 1:length(elevation))
    {
    Z <- length(which(Bdat$POINT_Z < (elevation[i]-0.5)))
    Z <- Z*4 #convert to DED per area
    Ramp[i]<-sum(Z)
    }
utility_data$boat_anglers<-Ramp
utility_data$Fish<-Fish(utility_data$elevation)

# ---- Scale performance metrics
#utility_data$WB_sc<- rescale(utility_data$WB, c(0,1))
#utility_data$WF_sc<- rescale(utility_data$WF, c(0,1))
#utility_data$Fish_sc<- rescale(utility_data$Fish, c(0,1))
#utility_data$Anglers_sc<- rescale(utility_data$Anglers, c(0,1))
#utility_data$Ramp_sc<- rescale(utility_data$Ramp, c(0,1))

# ---- Performance Metric Functions
WBM<-approxfun(utility_data$elevation, utility_data$WB, rule=2,yleft=0,yright=1)
WFM <- approxfun(utility_data$elevation, utility_data$WF, rule=2,yleft=0,yright=1)
FishM <- approxfun(utility_data$elevation, utility_data$Fish, rule=2,yleft=0,yright=1)
AnglersM <- approxfun(utility_data$elevation, utility_data$Anglers, rule=2,yleft=0,yright=1)
RampM <- approxfun(utility_data$elevation, utility_data$Ramp, rule=2,yleft=0,yright=1)





if(2==3){

#incorporate timing to objective utility
DOY<-c(1:365)
datalist <- list()
for(i in 1:length(DOY)){
  dataframe$DOY<-DOY[i]
  datalist[[i]]<-dataframe
}
big_data1 <- do.call(rbind, datalist)


dates<-data.frame(matrix(ncol = 2, nrow =365))
dates$DOY<-c(1:365)
dates$Month<-NA
for(i in 1:365){
  date<-as.Date(i, origin = "2016-01-01")
  Month<-month(as.POSIXlt(date, format="%d/%m/%Y"))
  dates$Month[i]<-Month
}

dates<-dates[,-c(1:2)]
#boating season
dates$Ramp<-ifelse(dates$DOY>=60 & dates$DOY<=304, 1, 0)
#fishing season weight-Boat

#Bt<-read.csv("./BoatAngEF.csv")
Bt<-read.csv("_dat/BoatAngEF.csv")

Total<-Bt%>%dplyr::group_by(Year)%>%summarise(sum(Total))
Total<-merge(Bt,Total)
Bt<-Total%>%dplyr::group_by(Year, Month)%>%summarise(normalized=sum(Total)/`sum(Total)`)
Bt<-Bt%>%dplyr::group_by(Month)%>%summarise(BTmean=mean(normalized))
Month<-c(1,2,11,12)
BTmean<-c(0,0,0,0)
d1<-data.frame(Month, BTmean)
Bt<-rbind(Bt,d1)
Bt$BTmean<-Bt$BTmean/sum(Bt$BTmean)
dates<-merge(dates,Bt)
#fishing season weight-Bank

Bk<-read.csv("BankAngEF.csv")
#Bk<-read.csv("_dat/BankAngEF.csv")

#sub in missing dates
Month<-c(1,1,2,2,11,11,12,12)
Total<-c(200,200,400,400,200,200,200,200)
Year<-c(2019,2020,2019,2020,2019,2020,2019,2020)
d1<-data.frame(Month, Total,Year)
Bk<-data.frame(Month=Bk$Month,Total=Bk$Total,Year=Bk$Year)
Bk<-rbind(Bk,d1)
Total<-Bk%>%dplyr::group_by(Year)%>%summarise(sum(Total))
Bk<-merge(Bk,Total)
Bk<-Bk%>%dplyr::group_by(Year, Month)%>%summarise(normalized=sum(Total)/`sum(Total)`)
Bk<-Bk%>%dplyr::group_by(Month)%>%summarise(BKmean=mean(normalized))
Bk$BKmean<- Bk$BKmean/sum(Bk$BKmean)
dates<-merge(dates,Bk)
#waterbird Seasonality
range <- c(1:12)
mean <- 6
sd <-3
dist <- dnorm(range, mean = mean, sd = sd)
df <- data.frame("Month" = range, "WB" = dist)
df$WB<-df$WB/sum(df$WB)
plot(df$WB~df$Month, main="waterbird")
dates<-merge(dates,df)
# Mississippi Growing Season
dates$WF<-ifelse(dates$DOY>=60 & dates$DOY<=304, 1, 0)



#Plot
library(scales)
dates$Ramp<-rescale(dates$Ramp, c(0,1))
dates$BTmean<-rescale(dates$BTmean, c(0,1))
dates$BKmean<-rescale(dates$BKmean, c(0,1))
dates$WF<-rescale(dates$WF, c(0,1))
dates$WB<-rescale(dates$WB, c(0,1))

names(dates)[3]<-"RampS"
names(dates)[4]<-"BoatS"
names(dates)[5]<-"BankS"
names(dates)[6]<-"WB_S"
names(dates)[7]<-"WF_S"

big_data1<-merge(big_data1, dates, by="DOY")


#Metric Functions
WBM<-approxfun(big_data1$elevation, big_data1$WB, rule=2,yleft=0,yright=1)
WFM <- approxfun(big_data1$elevation, big_data1$WF, rule=2,yleft=0,yright=1)
FishM <- approxfun(big_data1$elevation, big_data1$Fish, rule=2,yleft=0,yright=1)
AnglersM <- approxfun(big_data1$elevation, big_data1$Anglers, rule=2,yleft=0,yright=1)
RampM <- approxfun(big_data1$elevation, big_data1$Ramp, rule=2,yleft=0,yright=1)


big_data1<-big_data1[order(big_data1$elevation),]

ggplot(big_data1,aes(DOY, WB, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
ggplot(big_data1,aes(DOY, WF, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
ggplot(big_data1,aes(DOY, Fish, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
ggplot(big_data1,aes(DOY, Anglers, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
ggplot(big_data1,aes(DOY, Ramp, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()


# dataframe2<-data.frame(elevation, WB, WF, Fish, Anglers, Ramp)
# #incorporate timing to objective utility
# DOY<-c(1:365)
# datalist <- list()
# for(i in 1:length(DOY)){
#   dataframe2$DOY<-DOY[i]
#   datalist[[i]]<-dataframe2
# }
# big_data2 <- do.call(rbind, datalist)
# write.csv(big_data2, "~/GitHub/Bluff-Lake-Project/_analysis/RescaleMetrics.csv")


#Form overall utility
W<- c(.20,.23,.27,.3)
big_data1$Utility<-(W[1]*((big_data1$Ramp*.5) + (big_data1$Anglers*.5))) + 
  (W[2]*big_data1$Fish) + (W[3]*big_data1$WB) + (W[4]*big_data1$WF)

#no utility for empty lake
big_data1$volume<-EL_2_Vol(big_data1$elevation)
big_data1$Utility<-ifelse(big_data1$elevation<66.568, 0, big_data1$Utility)

ggplot(big_data1,aes(DOY, Utility, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "Day of Year")+ theme_classic()

# big_data1<-big_data1[order(big_data1$elevation),]
# 
# ggplot(big_data1,aes(DOY, WB, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, WF, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Fish, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Anglers, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# ggplot(big_data1,aes(DOY, Ramp, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "DOY")+ theme_classic()
# 
# 
# # dataframe2<-data.frame(elevation, WB, WF, Fish, Anglers, Ramp)
# # #incooporate timing to objective utility
# # DOY<-c(1:365)
# # datalist <- list()
# # for(i in 1:length(DOY)){
# #   dataframe2$DOY<-DOY[i]
# #   datalist[[i]]<-dataframe2
# # }
# # big_data2 <- do.call(rbind, datalist)
# # write.csv(big_data2, "~/GitHub/Bluff-Lake-Project/_analysis/RescaleMetrics.csv")
# 
# 
# #Form overall utility
# W<- c(.20,.23,.27,.3)
# big_data1$Utility<-(W[1]*((big_data1$Ramp*.5) + (big_data1$Anglers*.5))) + 
#   (W[2]*big_data1$Fish) + (W[3]*big_data1$WB) + (W[4]*big_data1$WF)
# 
# #no utility for empty lake
# big_data1$volume<-EL_2_Vol(big_data1$elevation)
# big_data1$Utility<-ifelse(big_data1$elevation<66.568, 0, big_data1$Utility)
# 
# ggplot(big_data1,aes(DOY, Utility, group=elevation, color=elevation)) + geom_line()+labs(y = "Utility", x = "Day of Year")+ theme_classic()


}

