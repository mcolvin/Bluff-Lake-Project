source("_r/1-global.R")
source("_r/2-functions.R")
source("_r/3-load-and-clean.R")
source("_r/6-analysis.R")
source("_r/CLEAN-Objective metrics_new.R")


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

discharge_hourly<- discharge_hourly %>% dplyr::group_by(dateTime) %>% dplyr::summarise(doy=mean(doy), Q_bl=mean(Q_bl), discharge=mean(discharge))

discharge_hourly$hour<-hour(discharge_hourly$dateTime)
discharge_hourly$minute<-minute(discharge_hourly$dateTime)
discharge_hourly$doy<-yday(discharge_hourly$dateTime)
discharge_hourly$year<-year(discharge_hourly$dateTime)


#1993 and 2012 do not start on 1/1
years<-c(1990:1992,1994:2011, 2013:2020)
# subset hourly discharge data to year of concern
datalist <- list()
datalist2<- list()

for(i in 1:length(years)){
  discharge_year<- subset(discharge_hourly, discharge_hourly$year==years[1]) #years[i]

  #Sub in any missing data 
  dateTime<-seq(from=discharge_year$dateTime[1],discharge_year$dateTime[1]+days(364), "1 hour")
  dateTime<-as.data.frame(dateTime)
  dateTime$doy<-as.numeric(format(dateTime$dateTime,"%j"))
  dateTime$hour<-hour(dateTime$dateTime)
  dateTime$minute<-minute(dateTime$dateTime)
  discharge_year<-left_join(dateTime, discharge_year, by = c("dateTime"="dateTime",
                                                             "doy"="doy", "hour"="hour", 
                                                             "minute"="minute"))
  discharge_year<-discharge_year[!is.na(discharge_year$year), ]
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
  discharge_year$discharge<-na.approx(discharge_year$Q_bl, na.rm = F)


  discharge_year$period<-ifelse(test = discharge_year$doy>=1 & discharge_year$doy<=14, yes= "1", no=ifelse(discharge_year$doy>=15 & discharge_year$doy<=181, yes="2", no=ifelse(discharge_year$doy>=182 & discharge_year$doy<=195, yes="3", no=ifelse(discharge_year$doy>=196 & discharge_year$doy<=212, yes="4", no=ifelse(discharge_year$doy>=213 & discharge_year$doy<=226, yes="5", no=ifelse(discharge_year$doy>=227 & discharge_year$doy<=243, yes="6", no=ifelse(discharge_year$doy>=244 & discharge_year$doy<=334, yes="7", no=ifelse(discharge_year$doy>=335 & discharge_year$doy<=348, yes="8", no="9"))))))))
  
  
  discharge_year <- discharge_year %>% dplyr::group_by(year,period) %>%
    dplyr::mutate(HOP = seq_along(period))
  
  #create hourly discharges
  datalist3 <- list()
  discharges<- c(0, 2.8, 5.6, 8.5, 11.3, 14.1, 17)
  
  for(x in 1:length(discharges)){
    
    #add in p-fish discharges
    discharge_year<-mutate(discharge_year, Dweek=ifelse(test= HOP>=1 & HOP<=167, yes="1", no=ifelse(HOP>=168 & HOP<=335, yes="2", no=ifelse(HOP>=336 & HOP<=503, yes="3", no=ifelse(HOP>=504 & HOP<=671, yes="4", no=ifelse(HOP>=672 & HOP<=839, yes="5", no=ifelse(HOP>=840 & HOP<=1007, yes= "6", no=ifelse(HOP>=1008 & HOP<=1175, yes="7", no=ifelse(HOP>=1176 & HOP<=1343, yes="8", no=ifelse(HOP>=1344 & HOP<=1511, yes="9", no=ifelse(HOP>=1512 & HOP<=1679, yes="10", no=ifelse(HOP>=1680 & HOP<=1847, yes="11", no=ifelse(HOP>=1848 & HOP<=2015, yes="12", no=ifelse(HOP>=2016 & HOP<=2183, yes="13", no=ifelse(HOP>=2184 & HOP<=2351, yes="14", no=ifelse(HOP>=2352 & HOP<=2519, yes="15", no=ifelse(HOP>=2520 & HOP<=2687, yes="16", no=ifelse(HOP>=2688 & HOP<=2855, yes="17", no=ifelse(HOP>=2856 & HOP<=3023, yes="18", no=ifelse(HOP>=3024 & HOP<=3191, yes="19", no=ifelse(HOP>=3192 & HOP<=3359, yes="20", no=ifelse(HOP>=3360 & HOP<=3527, yes="21", no=ifelse(HOP>=3528 & HOP<=3695, yes="22", no=ifelse(HOP>=3696 & HOP<=3863, yes="23", no="24"))))))))))))))))))))))))
    
    discharge_hourly2<-discharge_year%>%group_by(period,Dweek)%>%slice(c(1:8))
    discharge_hourly2$PfD<-discharges[x]*60*60
    discharge_year3<-left_join(discharge_year,discharge_hourly2)
    discharge_year3$PfD[is.na(discharge_year3$PfD)] <- 0
    discharge_year3$WCS_strategy<-discharges[x]
    datalist3[[x]] <- discharge_year3
  }
  discharge_hourly4 <- rbindlist(datalist3)
  #discharge_hourly<-discharge_year3

  datalistz <- list()
  for(z in 1:length(elevation)){
    discharge_hourly4$Board<-elevation[z]
    datalistz[[z]] <- discharge_hourly4
  }
  discharge_hourly4 <- rbindlist(datalistz)
  
  discharge_hourly4$period_dist<-paste(discharge_hourly4$period, discharge_hourly4$WCS_strategy,discharge_hourly4$year, discharge_hourly4$Board, sep="-")
  combos<-unique(discharge_hourly4$period_dist)
  
  
  
  for(k in 1:length(combos)){
  Period<-subset(discharge_hourly4, discharge_hourly4$period_dist==combos[k]) #combos[k]
  
  period<-mean(as.numeric(Period$period))
  
  Board<-mean(as.numeric(Period$Board))
  
  
  
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
    #DOY
    
    doy<-DOYfun(t) #t
    
    # acceleration due to gravity m/sec^2
    acc_due_to_gravity<- 9.81
    # wse at intake
    ele_intake<-predict(gam_4, newdata=data.frame(Q_bl=Q_blfun(t), doy))
    # wse lake
    ele_lake<-  wse# wse_lake(t)
    
    #----------------------------------------------------------------------
    # 
    #  water releasing over the WCS
    #
    #----------------------------------------------------------------------
    # board elevation
    WCS1_wse<-c(Board,Board,Board,Board,Board,
                Board,Board) # seven bays
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
    wcs_out<-(wcs_out*60*60)*0.8        #per hour
    # emergency spillway
    #emergency overflow measurements (meters)
    EOFwidth<-23
    EOFheight<-68.698
    EOF_head<-max(0,ele_lake-EOFheight) 
    EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
    EOF_out<-EOF_out*60*60     #per hour
    PfD<-PfDfun(t)
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
    # convert to cubic meters per 60 minutes
    intake_in<-intake_in*60*60
    
    #----------------------------------------------------------------------
    # 
    #  change in volume
    #
    #----------------------------------------------------------------------
    V<-V+(intake_in-(wcs_out+EOF_out))-PfD # iteration needs the actual volume, not dV
    return(list(V=V,wse=wse,
                ele_lake=ele_lake,
                sa=sa,
                intake_in=intake_in,
                EOF_out=EOF_out,
                wcs_out=wcs_out))
  }
  
  
  #######################################
  
  
  
  Q_blfun<- approxfun(Period$HOP, Period$Q_bl)
  DOYfun<- approxfun(Period$HOP, Period$doy)
  PfDfun<-approxfun(Period$HOP, Period$PfD)
  
  Period$gap<-c(NA, with(Period, HOP[-1] - 
                                   HOP[-nrow(Period)]))
  
  ini_values<-EL_2_Vol(Board)
  parameters<-NULL # no parameters yet
  solution<- ode(
    y=ini_values, 
    times=Period$HOP, 
    func=wse_dyn, 
    parms=parameters, 
    method="iteration")
  colnames(solution)[2] <- "V"
  solution<-as.data.table(solution)
  solution$HOP<-solution$time
  
  solution<-left_join(solution,Period)
  solution$EL<-Vol_2_EL(solution$V)
  solution$V<-ifelse(solution$V<0, 0, solution$V)
  datalist2[[k]] <- solution
  
  plot(EL~time,solution,ylab="Lake elevation",las=1,
       main=combos[k])
  abline(a=66.568,b=0)
  }
  Solution   <- do.call(rbind, datalist2)
  datalist[[1]]<-Solution
}
All_Years   <- do.call(rbind, datalist)
write.csv(All_Years,"_dat/All_Years_All_Elevations_Discharge_Sims.csv")

# Calculating Utilties

All_Years<-read.csv("_dat/All_Years_All_Elevations_Discharge_Sims.csv")
All_Years$elevation<-All_Years$EL
All_Years$WB<-WBM(All_Years$elevation)
All_Years<-All_Years%>%group_by(WCS_strategy, year, period)%>%
  mutate(Avg15days=rollmean(elevation, k=336, fill=EL))
All_Years$WF<-WFM(All_Years$Avg15days)
All_Years$Fish<-FishM(All_Years$elevation)
All_Years$Anglers<-AnglersM(All_Years$elevation)
All_Years$Ramp<-RampM(All_Years$elevation)
All_Years$Paddlefish<-rescale(All_Years$WCS_strategy, to=c(0,1))


#Seasonal Weights
All_Years$DOY<-All_Years$doy
All_Years<-merge(All_Years, dates, by="DOY")

All_Years$WB<-All_Years$WF*All_Years$WF_S
All_Years$WF<-All_Years$WB*All_Years$WB_S
All_Years$Ramp<-All_Years$Ramp*All_Years$BoatS
All_Years$Anglers<-All_Years$Anglers*All_Years$BankS

#Rescale
All_Years$WB<-rescale(All_Years$WF, to=c(0,1))
All_Years$WF<-rescale(All_Years$WB, to=c(0,1))
All_Years$Ramp<-rescale(All_Years$Ramp, to=c(0,1))
All_Years$Anglers<-rescale(All_Years$Anglers, to=c(0,1))
All_Years$Fish<-rescale(All_Years$Anglers, to=c(0,1))

#Weight Utilities acording to CCP to form Cumulative Utility
W<- c(.15,.20,.25,.3,.1)
All_Years$Utility<-(W[5]*All_Years$Paddlefish)+(W[1]*((All_Years$Ramp*.5) + 
                  (All_Years$Anglers*.5))) + (W[2]*All_Years$Fish) + 
                  (W[3]*All_Years$WB) + (W[4]*All_Years$WF)


#Group and average
All_Years$WCS_strategy<-as.factor(All_Years$WCS_strategy)
All_Years$period<-as.factor(All_Years$period)

discharges<- c(0, 2.8, 5.6, 8.5, 11.3, 14.1, 17)

PERIODS2 <-All_Years
datalist5<-list()
for(p in 1:length(discharges)){
  p1<-subset(PERIODS2, PERIODS2$WCS_strategy==discharges[p])
  p1<- p1 %>% dplyr::group_by(year,period, Board, doy) %>% summarise(Utility=mean(Utility), 
                                                              EL=mean(EL))
  p1 <- p1 %>% dplyr::group_by(year,period, Board) %>%dplyr::arrange(doy) %>%
    dplyr::mutate(CumUt = cumsum(Utility), WCS_strategy=discharges[p]) 
  datalist5[[p]] <- p1
}
PERIODS2 <- rbindlist(datalist5)

Final<- PERIODS2 %>% 
  dplyr::group_by(WCS_strategy, year, period, Board) %>% 
  dplyr::arrange(doy) %>%  
  dplyr::slice(n())

Final$elevation<-Final$EL
fin1<-subset(Final, Final$period==1)
fin1$penalty<-PenaltyM1(fin1$elevation)
fin2<-subset(Final, Final$period==2)
fin2$penalty<-PenaltyM2(fin2$elevation)
fin3<-subset(Final, Final$period==3)
fin3$penalty<-PenaltyM3(fin3$elevation)
fin4<-subset(Final, Final$period==4)
fin4$penalty<-PenaltyM4(fin4$elevation)
fin5<-subset(Final, Final$period==5)
fin5$penalty<-PenaltyM5(fin5$elevation)
fin6<-subset(Final, Final$period==6)
fin6$penalty<-PenaltyM6(fin6$elevation)
fin7<-subset(Final, Final$period==7)
fin7$penalty<-PenaltyM7(fin7$elevation)
fin8<-subset(Final, Final$period==8)
fin8$penalty<-PenaltyM8(fin8$elevation)
fin9<-subset(Final, Final$period==9)
fin9$penalty<-PenaltyM9(fin9$elevation)
Final<-rbind(fin1,fin2,fin3,fin4,fin5,fin6,fin7,fin8,fin9)

Final$CumUt<-Final$CumUt*Final$penalty

Final<- Final%>%group_by(WCS_strategy, period, Board) %>%summarise(CumUt=mean(CumUt))

Final<- Final %>% dplyr::group_by(period, Board) %>% 
  dplyr::mutate(Utility=rescale(CumUt, to=c(0,1)), WCS_strategy=WCS_strategy)

Final<-read.csv("_outputs/final.csv")
Final<-subset(Final, Board>66.6)
Final$WCS_strategy<-as.factor(Final$WCS_strategy)
plots<-list()
elevation<-unique(Final$Board)
for(u in 1:length(elevation)){
  fnl<-subset(Final, Final$Board==elevation[u])
  plt<-ggplot(fnl, aes(x=period, y=WCS_strategy)) +
    geom_tile(aes(fill = Utility)) +
    scale_fill_distiller(palette = "Greys") +
    labs(title = elevation[u],
       y = "Strategy",
       x = "Period")+theme(legend.position = "none", axis.title.x=element_blank(), 
                           axis.title.y=element_blank(), axis.text.x=element_blank(),
                           axis.text.y=element_blank())
  plots[[u]]<-plt
}

grid.arrange(grobs = plots, ncol = 5) 

Final<-read.csv("_outputs/final.csv")
Final<-subset(Final, Board>66.6)
decision<- Final%>%group_by(period, Board)%>% filter(Utility== max(Utility)) %>% 
  select(WCS_strategy, Utility)
decision$WCS_strategy<-ifelse(decision$Utility<1, 0, decision$WCS_strategy)

decision$Board<-as.factor(decision$Board)
decision$WCS_strategy<-as.factor(decision$WCS_strategy)
decision$period<-as.factor(decision$period)


#p<-
ggplot(decision, aes(x=period, y=Board)) +
  geom_tile(aes(fill = WCS_strategy)) + #scale_fill_grey()+
  theme_classic()+
  labs(title = "Decison",
       y = "Elevation",
       x = "Period")
ggsave("outputs.jpg",plot=p)