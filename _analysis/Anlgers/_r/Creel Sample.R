##Mimic Sampling by Jennings
  #Choose sampling days independently between months
  #7 samples per month (+ any holidays slight change from original)
  #Stratified by weekday and weekend
  #3 time blocks 7-11, 11-3, 3-7 (equal probabilities 1st month then adaptive(this works now, but would not be adaptive))
  #coin flips for start direction of interviews, and starting with counts or interviews

Week<- c(1,2,3,5,6,7,8,9,10,13,14,15,16,17,20,21,22,23,24,27,28,29,30,31)
WeekEnd<- c(4,5,11,12,18,19,25,26)
timeslots = c('7', '11', '3')
Weekdays<-sample(Week, 3, FALSE)
Weekends<-sample(WeekEnd, 4, FALSE)
Days<- c(Weekdays,Weekends)


Maysampleschedule = data.frame(TIME = sample(timeslots, 7, replace = T), 
                               DAY = sample(Days, 7, replace = F))
Weekdays<-sample(Week, 3, FALSE)
WTimes<-sample(1:3, 3, TRUE)

Weekends<-sample(WeekEnd, 4, FALSE)
WETimes<-sample(1:3, 4, TRUE)

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


Hooks<- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3)

Hooks
# for trotlines
sample(Hooks, 30, FALSE)
