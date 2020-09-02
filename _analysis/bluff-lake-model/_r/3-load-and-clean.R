# Bluff Lake WSE data begins 05/07/2019

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
GateBayWidth<-3.6576 #width for one bay
#Bays with boards (4 total board sections/ 2 bays)
BoardBayWidth<-1.6764 #width for one board section (2 board sections per bay)

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
discharge_hourly[,hour:=as.numeric(format(dateTime,"%H"))]

# get the mean discharge
discharge_hourly<- discharge_hourly[,.(n=.N,Q_bl=mean(Q_bl),discharge=mean(discharge)),
    by=.(year,doy,hour)]


#----------------------------------------------------------------------
# 
#  WATER LEVEL LOGGER DATA
#
#----------------------------------------------------------------------
# Bluff Lake WSE data begins 05/07/2019
loggers<-read.xlsx("_dat/Level_logger.xlsx")
names(loggers)<-c("id","location","date_time","pressure","temp_c","barom",
    "water_level","elevation")
# open xlsx convertToDateTime fails on big datasets...
loggers$dt <- as.POSIXct(loggers$date_time*3600*24, tz="GMT", origin = "1900-01-01")
loggers<-as.data.table(loggers)


#----------------------------------------------------------------------
# 
#  BATHYMETRIC DATA
#
#----------------------------------------------------------------------

bath<- fread("_dat/Bathymetry/CompleteMap.csv")
names(bath)<-c("X","Y","elevation")




