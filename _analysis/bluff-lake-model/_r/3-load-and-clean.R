# Bluff Lake WSE data begins 05/07/2019

start_date<-as.Date("2019-05-07")

#----------------------------------------------------------------------
# 
#  Watershed sizes
#
#----------------------------------------------------------------------
bluff_lake<- 220 # mi^2
bluff_lake<- bluff_lake*2.58999 # km^2
macon<- 768 # mi^2
macon<- macon*2.58999 # km^2

#----------------------------------------------------------------------
# 
#  Widths and elevations for structures
#
#----------------------------------------------------------------------
#emergency overflow measurements (meters)
EOFwidth<-23
EOFheight<-68.597


#Control Structure Measurements (meters)
#Bays with Radial arm gates (5 total)
gate_bay_width<-3.6576 #width for one bay
#Bays with boards (4 total board sections/ 2 bays)
board_bay_width<-1.6764 #width for one board section (2 board sections per bay)


# width of the water control structure in meters
# for bay 1 (boarded), 2, 3, 4, 5, 6, 7 (boarded)
wcs_width<-rep(0,7)
wcs_width[1]<- board_bay_width*2 
wcs_width[2]<- gate_bay_width 
wcs_width[3]<- gate_bay_width 
wcs_width[4]<- gate_bay_width 
wcs_width[5]<- gate_bay_width 
wcs_width[6]<- gate_bay_width 
wcs_width[7]<- board_bay_width*2 


#----------------------------------------------------------------------
# 
#  NOXUBEE RIVER DAILY DISCHARGE DATA
#
#----------------------------------------------------------------------

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
    write.csv(discharge_daily,"_dat/discharge_daily.csv")
    }
    
    
# scale discharge to watershed area m^3/second
discharge_daily[,Q_bl:=(discharge/bluff_lake)*0.0283168]


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
if(tmp>15)# pull data again if more than 15 days have passed since last pull
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
# scale discharge to watershed area m^3/second
discharge_hourly[,Q_bl:=(discharge/bluff_lake)*0.0283168]
discharge_hourly$dateTime<-as_datetime(discharge_hourly$dateTime)
discharge_hourly$dateTime<-round_date(discharge_hourly$dateTime, "30 mins")
discharge_hourly$hour<-hour(discharge_hourly$dateTime)
discharge_hourly$minute<-minute(discharge_hourly$dateTime)

# subset hourly discharge data to the dates that wse logger has data
discharge_hourly<- discharge_hourly[date>=start_date,]

# get the mean discharge
discharge_hourly<- discharge_hourly[,.(Q_bl=mean(Q_bl),discharge=mean(discharge)),
    by=.(year,doy,hour, minute)]




#----------------------------------------------------------------------
# 
#  WATER LEVEL LOGGER DATA
#
#----------------------------------------------------------------------
# Bluff Lake WSE data begins 05/07/2019
data<-read.xlsx("_dat/Level_logger.xlsx")
names(data)<-c("ID","location","date_time","pressure", "temp_c","baro","water_level","wse")
# open xlsx convertToDateTime fails on big datasets...
data$dt <- as.POSIXct(data$date_time*3600*24, tz="GMT", origin = "1900-01-01")
data$dt<- as.POSIXct(strftime(data$dt, format = "%Y/%m/%d %H:%M"))
data$dt<-round_date(data$dt, "30 mins")
data<-as.data.table(data)
data$year<-as.numeric(format(data$dt, "%Y"))
data$doy<-strftime(data$dt, format = "%j")
data$hour <- hour(data$dt)
data$minute<-minute(data$dt)
loggers<-dcast(data, dt+year+doy+hour+minute~location,
           value.var="wse", fun.aggregate =mean)
#loggers<- loggers[,.(water_level=mean(water_level),wse=mean(wse), temp_c=mean(temp_c)),
#                                    by=.(location, year,doy,hour)]

(subset(data,ID==94629))

subset(loggers,year==2020&doy==135 &hour%in%c(4,5,6,7,8,9,10))

#----------------------------------------------------------------------
# 
#  BATHYMETRIC DATA
#
#----------------------------------------------------------------------

bath<- fread("_dat/Bathymetry/CompleteMap.csv")
names(bath)<-c("X","Y","elevation")



#----------------------------------------------------------------------
# 
#  merge discharge and wse
#
#----------------------------------------------------------------------
# several fields to include: year, doy, hour, wse, discharge (@macon, adjusted to watershed, in cms as units)
discharge_hourly$discharge_cms<-discharge_hourly$discharge*0.0283168
discharge_hourly$doy<-as.numeric(discharge_hourly$doy)
loggers$doy<-as.numeric(loggers$doy)




lake_info <- merge(loggers, 
    discharge_hourly[,.SD,
        .SDcols=c("year", "doy", "hour", "minute","discharge_cms", "Q_bl")],
    by=c("year", "doy", "hour", "minute"), all.x = TRUE)

# make a field for 'continuous time' which is a fractional day starting at 0 for the first row of data an increasing fractinally for each hour and minute (i.e., 5:30 am would be 330 minutes in, 330/1440 = 0.2291667, the same time on the next day would be 1.2291667)
lake_info$cont_time<-((lake_info$year-2019)*525600)+(lake_info$doy*1440)+(lake_info$hour*60)+
                      (lake_info$minute)-185760 
lake_info[,Cypress:=ifelse(is.nan(Cypress),NA,Cypress)]
lake_info[,Gauge:=ifelse(is.nan(Gauge),NA,Gauge)]
lake_info[ year==2020&doy==135 &hour%in%c(4,5,6,7,8,9,10),]

#----------------------------------------------------------------------
# 
#  functions to feed the model
#  wse_intake: watersurace elevation at intake
#  wse_lake: lake water surface elevation
#  macon: Noxubee River discharge at Macon
#
#----------------------------------------------------------------------
## data from intake is the limiting value
model_data<-as.data.table(lake_info[!is.na(Intake),])
model_data[,cont_time:=cont_time-min(cont_time)]
# wse_intake
wse_intake<-approxfun(model_data$cont_time,
    model_data$Intake,
    rule=1) # return NAs outside of data
lake_info[ year==2020&doy==135 &hour%in%c(4,5,6,7,8,9,10),]

# wse_lake: average logger data
model_data$wse_lake<-sapply(1:nrow(model_data),
    function(x){mean(na.omit(
      model_data[x]$Cypress,
      model_data[x]$Gauge))})

wse_lake<-approxfun(model_data$cont_time,
    model_data$wse_lake,
    rule=1) # return NAs outside of data
model_data[,time:=1:.N]    
# macon
macon<-approxfun(model_data$cont_time,
    model_data$wse_lake,
    rule=1) # return NAs outside of data
   

if(2==3)
    { # check data streams
    plot(Cypress~cont_time,model_data,type='l',ylab="Water surface elevation",
        ylim=c(67.5,70.5))
    points(Gauge~cont_time,model_data,type='l',col="red")
    points(Intake~cont_time,model_data,type='l',col="green")
    points(wse_lake~cont_time,model_data,type='l',
        col="blue")
    par(new=TRUE)
    plot(discharge_cms~cont_time,model_data,type='l',yaxt="n",ylab='',
        col="lightgrey")
    axis(side=4, at=axTicks(2),labels=TRUE)
    mtext(side=4,"Discharge")
         abline(v=125000)       
    legend("topleft",legend=c("Cypress","Gauge","Intake","Macon gauge"),
        col=c("black","red","green","lightgrey"),lwd=3,bg="white")
}

#  elevation of water at intake versus Macon
#  run lake_info and discharge_daily from load-and-clean
newdat<- subset(lake_info, dt> as.Date("2019/11/12 18:00")) 
matrix_gam <- data.table(newdat)

gam_4 <- gam(Intake ~ te(Q_bl, doy),
             data = matrix_gam,
             family = gaussian)
summary(gam_4)
summary(gam_4)$s.table

newdat$FitG4<- gam_4$fitted.values
lake_info<-lake_info[order(doy)]
newdat<-newdat[order(doy)]
plot(Intake~doy, lake_info, type="l", col="blue", ylim=c(69,70.2), xlim=c(0,365), ylab=NA, xlab=NA)
par(new=T)
plot(FitG4~doy, newdat, type="l", col="red", ylim=c(69,70.2), xlim=c(0,365),
     main="GAM Model Intake ~ Macon + DOY", xlab="Date", 
     ylab="Water Surface Elevation")
legend("topright", c("Predicted", "Lake Elevation"),
       col = c("red", "blue"), lty = c(1, 1))        

discharge_daily<-fread("_dat/discharge_daily.csv")
discharge_daily$Pred_El<-predict(gam_4, discharge_daily)
