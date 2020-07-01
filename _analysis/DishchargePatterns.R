library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
#library(RmarineHeatWaves)
library(lfstat)
#library(fasstr)

dat<- read.csv("Depth-Mapping/_dat/DailyMacon01011945_01012020.csv")
dat$ï..datetime <- as.POSIXct(dat$ï..datetime, format="%m/%d/%Y")
names(dat)[1]<-"Date"
#add in missing dates
dat$DOY<-strftime(dat$Date, format = "%j")
dat$Discharge_cfs<-na.approx(dat$Discharge_cfs)

Date<-seq(as.Date("1945/1/1"), as.Date("2019/12/31"), "days")
Date<-as.data.frame(Date)
Date$Date<-as.POSIXct(Date$Date, format="%Y/%m/%d")
Date$Date<-round_date(Date$Date, "day")
dat<-left_join(Date, dat, by = "Date")
dat$TF<-is.na(dat$Discharge_cfs)
dat$Discharge_cfs<- ifelse(dat$TF == "TRUE", 
                           DoY(dat$Discharge_cfs), dat$Discharge_cfs)
dat$Discharge_cms<-dat$Discharge_cfs/35.3147
dat$WaterYr<-water_year(dat$Date, "usgs")
dat$WaterYr<-as.character(dat$WaterYr)
dat$WaterYr<-as.numeric(dat$WaterYr)
dat$month<-month(dat$Date)

#Yearly AVG Discharge
datyear<-dat%>% group_by(WaterYr)%>%summarise(mDis=mean(Discharge_cms))
ggplot(datyear, aes(WaterYr, mDis))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y=bquote('Mean Discharge'~(m^3*s^-1)))

#Monthly Discharge
datmonth<- dat%>%group_by(WaterYr, month)%>%summarise(mDis=mean(Discharge_cms))
ggplot(datmonth, aes(month, mDis, group=WaterYr))+geom_line(alpha = 1/5)+ theme_classic()+
  labs(x="Month", y=bquote('Mean Discharge'~(m^3*s^-1)))



#Define High and low flows
quantile(dat$Discharge_cms, probs=seq(0,1, 0.05), na.rm=T)
high<- 92.3128329
low<- 1.8122765
highflow<-subset(dat, Discharge_cms>=high)
lowflow<-subset(dat, Discharge_cms<=low)
highflow <- highflow %>%
  group_by(WaterYr) %>% 
  mutate(wtr_day = (as.integer(difftime(Date,ymd(paste0(WaterYr - 1 ,'-09-30')), units = "days"))))
lowflow <- lowflow %>%
  group_by(WaterYr) %>% 
  mutate(wtr_day = (as.integer(difftime(Date,ymd(paste0(WaterYr - 1 ,'-09-30')), units = "days"))))
ggplot(highflow, aes(as.numeric(wtr_day), Discharge_cms))+
  geom_point(alpha = 1/10)+theme_classic()+
  labs(x="Day of Water Year", y=bquote('Mean Discharge'~(m^3*s^-1)))
ggplot(lowflow, aes(as.numeric(wtr_day), Discharge_cms))+
  geom_point(alpha = 1/10)+theme_classic()+
  labs(x="Day of Water Year", y=bquote('Mean Discharge'~(m^3*s^-1)))

#plot cummulative percentile across years
dat2<- dat %>% group_by(WaterYr) %>% summarise(Disch90=quantile(Discharge_cms, 
                                                probs=0.90, na.rm=T))
ggplot(dat2, aes(WaterYr, Disch90))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y="90th Percentile of Discharges")
dat2$CuDis90<- cummean(dat2$Disch90)
ggplot(dat2, aes(WaterYr, CuDis90))+theme_classic()+geom_line()+
  labs(x="Water Year (October-September)", y="Cummulative 90th Percentile of Discharges")

dat3<- dat %>% group_by(WaterYr) %>% summarise(Disch10=quantile(Discharge_cms, 
                                                                probs=0.10, 
                                                                na.rm=T))
ggplot(dat3, aes(WaterYr, Disch10))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y="10th Percentile of Discharges")
dat3$CuDis90<- cummean(dat3$Disch10)
ggplot(dat3, aes(WaterYr, CuDis90))+theme_classic()+geom_line()+
  labs(x="Water Year (October-September)", y="Cummulative 10th Percentile of Discharges")


#Number of High flows
data<-dat %>%
  group_by(ID = data.table::rleid(Discharge_cms > high)) %>%
  mutate(Consec_Days = if_else(Discharge_cms > high, row_number(), 0L))
data2<-data%>%group_by(WaterYr)%>%summarise(Events=sum(Consec_Days==1))
data2$Events<-ifelse(data2$Events=="NA", 0, data2$Events)
M1<-glm(Events ~ WaterYr,
    data = data2,
    family = poisson)
summary(M1)
data2$Fit<-M1$fitted.values
ggplot(data2)+theme_classic()+geom_point(aes(WaterYr, Events))+geom_line(aes(WaterYr, Fit))+
  labs(x="Water Year (October-September)", y="Number of High Flow Events")


#Duration of high flow events
duration<-data%>%group_by(WaterYr,ID)%>%summarise(Dur=max(Consec_Days))
duration0<-subset(duration, Dur == 0)
duration<-filter(duration, Dur > 0)
duration0<-duration0 %>%
  group_by(WaterYr) %>%# in each group, arrange in ascending order by distance
  filter(row_number() == 1)
duration<-rbind(duration,duration0)
duration$WaterYr2<-as.character(duration$WaterYr)
ggplot(duration, aes(WaterYr2, y=Dur))+geom_boxplot()+
  theme_classic()+
  labs(x="Water Year (October-September)", y="Duration of High Flow Events")+
  scale_x_discrete(breaks=c(1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))

Mduration<-duration%>%group_by(WaterYr)%>%summarise(mDur=mean(Dur), SD=sd(Dur))
M2<-glm(mDur ~ WaterYr,
        data = Mduration,
        family = poisson)
summary(M2)
Mduration$Fit<-M2$fitted.values
ggplot(Mduration, aes(WaterYr, mDur))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y="Duration of High Flow Events")+
  geom_line(aes(WaterYr, Fit))+geom_errorbar(aes(ymin=mDur-SD, ymax=mDur+SD), width=.1)

#Number of low flows
data3<-dat %>%
  group_by(ID = data.table::rleid(Discharge_cms < low)) %>%
  mutate(Consec_Days = if_else(Discharge_cms < low, row_number(), 0L))
data4<-data3%>%group_by(WaterYr)%>%summarise(Events=sum(Consec_Days==1))
M3<-glm(Events ~ WaterYr,
        data = data4,
        family = poisson)
summary(M3)
data4$Fit<-M3$fitted.values
ggplot(data4, aes(WaterYr, Events))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y="Number of Low Flow Events")+
  geom_line(aes(WaterYr,Fit))


#duration of low flow events
duration2<-data3%>%group_by(WaterYr,ID)%>%summarise(Dur=max(Consec_Days))
duration20<-subset(duration2, Dur == 0)
duration2<-filter(duration2, Dur > 0)
duration20<-duration20 %>%
  group_by(WaterYr) %>%# in each group, arrange in ascending order by distance
  filter(row_number() == 1)
duration2<-rbind(duration2,duration20)
duration2$WaterYr2<-as.character(duration2$WaterYr)
ggplot(duration2, aes(WaterYr2, Dur))+geom_boxplot()+
  theme_classic()+
  labs(x="Water Year (October-September)", y="Duration of Low Flow Events")+
  scale_x_discrete(breaks=c(1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))
duration2$WaterYr2<-as.numeric(duration2$WaterYr)
Mduration2<-duration2%>%group_by(WaterYr)%>%summarise(mDur=mean(Dur), SD=sd(Dur))
M4<-glm(mDur ~ WaterYr,
        data = Mduration2,
        family = poisson)
summary(M2)
Mduration2$Fit<-M4$fitted.values
ggplot(Mduration2, aes(WaterYr, mDur))+theme_classic()+geom_point()+
  labs(x="Water Year (October-September)", y="Duration of Low Flow Events")+
  geom_line(aes(WaterYr,Fit))+geom_line(aes(WaterYr, Fit))+geom_errorbar(aes(ymin=mDur-SD, ymax=mDur+SD), width=.1)

