##Mimic Sampling by Jennings
  #Choose sampling days independently between months
  #7 samples per month
  #Stratified by weekday and weekend
  #2 time blocks 7-2, 2-9 (equal probabilities 1st month then adaptive)
  #simulated coin flips for start direction of interviews, and starting site

Week<- c(3,4,5,6,7,10,11,12,13,14,17,18,19,20,21,24,25,26,27,28)
WeekEnd<- c(1,2,8,9,15,16,22,23,29,30)
timeslots = c('7', '2')
Weekdays<-sample(Week, 3, FALSE)
Weekends<-sample(WeekEnd, 4, FALSE)
Days<- c(Weekdays,Weekends)
sites<- c("S1","S2","S3")
Start_DIR<- c("Clockwise", "Counter Clockwise")

Junesampleschedule = data.frame(TIME = sample(timeslots, 7, replace = T), 
                               DAY = sample(Days, 7, replace = F), 
                               START_SITE= sample(sites, 7, replace = T), 
                               START_DIR= sample(Start_DIR, 7, replace = T))
write.table(Junesampleschedule, "_dat/Junesampleschedule.csv", sep = ",", row.names = F) 







#Fake Creel Day
##Bank Anglers
Anglers<- c(1,2,3,4,5,6,7,8,9,10)
Fishing_start<-rnorm(10,10.76,3.18)
Fish_Time<-rnorm(10,2.6,3.23)
Fish_Time<-abs(Fish_Time)
Fishing_End<-Fishing_Start+Fish_Time
Day<-data.frame(Anglers = Anglers, Start=Fishing_start, End=Fishing_End)
plot(range(c(Day$Start, Day$End)),c(0,nrow(Day)),type="n",las=1,
     yaxt='n',ylab="Anglers"
,xlab="Fishing Time", main = "Angler Effort")
for(i in 1:nrow(Day))
{
  segments(Day$Start[i],y0=i, x1=Day$End[i],y1=i)
}
axis(2,at=c(1:nrow(Day)),labels=Day$Anglers,las=1)
##Boat Anglers
Anglers<- c(1,2,3,4,5,6,7,8,9,10)
Fishing_start<-rnorm(10,8.63,2.22)
Fish_Time<-rnorm(10,5.66,3.28)
Fish_Time<-abs(Fish_Time)
Fishing_End<-Fishing_Start+Fish_Time
Day<-data.frame(Anglers = Anglers, Start=Fishing_start, End=Fishing_End)
plot(range(c(Day$Start, Day$End)),c(0,nrow(Day)),type="n",las=1,
     yaxt='n',ylab="Anglers"
     ,xlab="Fishing Time", main = "Angler Effort")
for(i in 1:nrow(Day))
{
  segments(Day$Start[i],y0=i, x1=Day$End[i],y1=i)
}
axis(2,at=c(1:nrow(Day)),labels=Day$Anglers,las=1)


