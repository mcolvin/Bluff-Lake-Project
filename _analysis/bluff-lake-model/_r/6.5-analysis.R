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






#----------------------------------------------------------------------
# 
#  lake hydrodynanamic model
#
#----------------------------------------------------------------------
wse_dyn<-function(t,x,parms)
{    
  #----------------------------------------------------------------------
  # 
  #  parameters and conversions
  #
  #----------------------------------------------------------------------
  # lake volume in m^3
  V<-x[1]
  # convert lake volume to water surface elevation
  wse<- Vol_2_EL(V)
  # lake surface area
  sa<- Vol_2_SA(V)
  # acceleration due to gravity m/sec^2
  acc_due_to_gravity<- 9.81
  # wse at intake
  ele_intake<-predict(gam_4, newdata=data.frame(Q_bl=Q_blfun(t), doy=DOYfun(t)))
  # wse lake
  ele_lake<-  wse# wse_lake(t)
  
  #----------------------------------------------------------------------
  # 
  #  water releasing over the WCS
  #
  #----------------------------------------------------------------------
  # board elevation
  WCS1_wse<-c(68.39712,68.39712,68.39712,68.39712,68.39712,
              68.39712,68.39712) # seven bays
  # wcs_width<-rep(1.6764,8)
  # water control structure head
  # set head to zero when wse<=board
  wcs_head<- sapply(WCS1_wse,function(x)
  {
    max(0,ele_lake-WCS1_wse)    
  })
  # amount of water flowing out of each bay of the wcs
  wcs_out<- weir(g=9.81,w=wcs_width[1], h=wcs_head[1])+
    weir(g=9.81,w=wcs_width[2], h=wcs_head[2])+
    0*weir(g=9.81,w=wcs_width[3], h=wcs_head[3])+
    0*weir(g=9.81,w=wcs_width[4], h=wcs_head[4])+
    0*weir(g=9.81,w=wcs_width[5], h=wcs_head[5])+
    0*weir(g=9.81,w=wcs_width[6], h=wcs_head[6])+
    weir(g=9.81,w=wcs_width[7], h=wcs_head[7])
  wcs_out<-(wcs_out*60*30)*0.8        
  # emergency spillway
  #emergency overflow measurements (meters)
  EOFwidth<-23
  EOFheight<-68.698
  EOF_head<-max(0,ele_lake-EOFheight) 
  EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
  EOF_out<-EOF_out*60*30     
  
  #----------------------------------------------------------------------
  # 
  #  water coming in (intake)
  #
  #----------------------------------------------------------------------
  # board elevation
  intake_board_wse<- c(68.800,68.800) # meters bay 1 and 2
  intake_width<- c(1.6764,1.6764) # meters bay 1 and 2
  intake_head<- c(max(0,ele_intake-intake_board_wse[1]),
                  max(0,ele_intake-intake_board_wse[2]))
  # water inputs to intake (cms)
  intake_in<-weir(g=9.81,h=intake_head[1],w=intake_width[1])+ # bay 1
    weir(g=9.81,h=intake_head[2],w=intake_width[2]) # bay 2
  # convert to cubic meters per 30 minutes
  intake_in<-intake_in*60*30
  
  #----------------------------------------------------------------------
  # 
  #  change in volume
  #
  #----------------------------------------------------------------------
  V<-V+(intake_in-(wcs_out+EOF_out))# iteration needs the actual volume, not dV
  return(list(V=V,wse=wse,
              ele_lake=ele_lake,
              sa=sa,
              intake_in=intake_in,
              EOF_out=EOF_out,
              wcs_out=wcs_out))
}




years<-c(2014:2019)
# subset hourly discharge data to year of concern
datalist <- list()
datalist2<- list()

for(i in 1:length(years)){
  discharge_year<- subset(discharge_hourly, discharge_hourly$year==years[i])
  # get the mean discharge
  discharge_year<-discharge_year[,.(Q_bl=mean(Q_bl),discharge=mean(discharge)),
                                 by=.(dateTime,year,doy,hour, minute)]
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
  discharge_year$discharge<-na.approx(discharge_year$Q_bl)
  
  Q_blfun<- approxfun(discharge_year$cont_time, discharge_year$Q_bl)
  DOYfun<- approxfun(discharge_year$cont_time, discharge_year$doy)

  datalist[[i]] <- discharge_year
  
  ini_values<-EL_2_Vol(wse_lake(1))
  parameters<-NULL # no parameters yet
  solution<- ode(
    y=ini_values, 
    times=discharge_year$cont_time, 
    func=wse_dyn, 
    parms=parameters, 
    method="iteration")
  colnames(solution)[2] <- "V"
  solution<-as.data.table(solution)
  
  solution<-cbind(solution,discharge_year)
  solution$EL<-Vol_2_EL(solution$V)
  
  datalist2[[i]] <- solution
  
  plot(EL~doy,solution,ylab="Lake volume",las=1,
       main=years[i], ylim=c(68,69.5))
}
HydroYears <- do.call(rbind, datalist)
Solution   <- do.call(rbind, datalist2)


