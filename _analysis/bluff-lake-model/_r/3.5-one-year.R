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
discharge_hourly$dateTime<-round_date(discharge_hourly$dateTime, "1 hour")

discharge_hourly<- discharge_hourly%>%group_by(dateTime)%>%summarise(doy=mean(doy), Q_bl=mean(Q_bl), discharge=mean(discharge))
  
discharge_hourly$hour<-hour(discharge_hourly$dateTime)
discharge_hourly$minute<-minute(discharge_hourly$dateTime)
discharge_hourly$doy<-yday(discharge_hourly$dateTime)
discharge_hourly$year<-year(discharge_hourly$dateTime)


years<-c(2014:2019)
# subset hourly discharge data to year of concern
datalist <- list()
for(i in 1:length(years)){
  discharge_year<- subset(discharge_hourly, discharge_hourly$year==years[i])
  #Sub in any missing data 
  dateTime<-seq(discharge_year$dateTime[1],discharge_year$dateTime[1]+days(364), "30 min")
  dateTime<-as.data.frame(dateTime)
  dateTime$doy<-as.numeric(format(dateTime$dateTime,"%j"))
  dateTime$hour<-hour(dateTime$dateTime)
  dateTime$minute<-minute(dateTime$dateTime)
  discharge_year<-left_join(dateTime, discharge_year, by = c("dateTime"="dateTime",
                                                                  "doy"="doy", "hour"="hour", 
                                                                  "minute"="minute"))
  discharge_year$discharge_cms<-discharge_year$discharge*0.0283168
  discharge_year$doy<-as.numeric(discharge_year$doy)
  # make a field for 'continuous time' which is a fractional day starting at 0 for the first row of data an increasing fractinally for each hour and minute (i.e., 5:30 am would be 330 minutes in, 330/1440 = 0.2291667, the same time on the next day would be 1.2291667)
  discharge_year<-unique(discharge_year)
  discharge_year$cont_time<-(discharge_year$doy*1440)+(discharge_year$hour*60)+
    (discharge_year$minute)-1440
  #check for missing values
  discharge_year$gap<-c(NA, with(discharge_year, cont_time[-1] - 
                                   cont_time[-nrow(discharge_year)]))
  #fill in missing data
  discharge_year$discharge<-na.approx(discharge_year$discharge)
  datalist[[i]] <- discharge_year
  }
  big_data <- do.call(rbind, datalist)
