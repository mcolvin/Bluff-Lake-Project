
#----------------------------------------------------------------------
# 
#  NOXUBEE RIVER DAILY DISCHARGE DATA
#
#----------------------------------------------------------------------

# PULL DISCHARGE AND GAUGE DATA FROM USGS PORTAL
## PARAMETER CODE 00065 Gage height, feet 00060; Discharge, cubic feet per second
discharge_daily<-fread("_dat/discharge_daily.csv")
discharge_daily[,date:=as.Date(date)]
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
    names(discharge_hourly)[4]<-"gage"
    names(discharge_hourly)[6]<-"discharge"
    discharge_hourly<-as.data.table(discharge_hourly)
    discharge_hourly[,date:=as.Date(dateTime)]
    discharge_hourly[,year:=as.numeric(format(date,"%Y"))]
    discharge_hourly[,doy:=as.numeric(format(date,"%j"))]
    write.csv(discharge_hourly,"_dat/discharge_hourly.csv")
    }

#----------------------------------------------------------------------
# 
#  WATER LEVEL LOGER DATA
#
#----------------------------------------------------------------------

# Cypress boardwalk
cypress<- read.xlsx("_dat/Cypress_loggerMay19-June20.xlsx",sheet="Level_loggers")
cypress$Date.Time<-convertToDateTime(cypress$Date.Time)
cypress<- as.data.table(cypress)

# WCS2_Intake_loggerMay19-June20
WCS2<- read.xlsx("_dat/WCS2_Intake_loggerMay19-June20.xlsx",sheet="Reg_Intake_20566057_2020_0124")
WCS2$Date.Time<-convertToDateTime(WCS2$Date.Time)
WCS2<- as.data.table(WCS2)


#----------------------------------------------------------------------
# 
#  Watershed sizes
#
#----------------------------------------------------------------------
bluff_lake<- 220 # mi^2
bluff_lake<- bluff_lake*2.58999 # mi^2
macon<- 768 # mi^2
macon<- macon*2.58999 # mi^2



#----------------------------------------------------------------------
# 
#  BATHYMETRIC DATA
#
#----------------------------------------------------------------------

bath<- fread("_dat/Bathymetry/CompleteMap.csv")
names(bath)<-c("X","Y","elevation")





