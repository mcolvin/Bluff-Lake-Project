dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")
dat$Z<- dat$depth2 #9 boards in the WCS
dat$tempC<-dat$Temp_C
#at<-subset(dat, dat$depth==1.4)
DO_fun<-function(t,x,parms)
{
  DO<-x
  tempC<-parms["tempC"]
  Z<-parms["Z"]
  WR<-0.07203515/60 #lake average water respiration in g/m^3/min
  # DO Saturation at a given temperature        
  DOsat<-4.09+(10.5*exp(-0.0371*tempC)) #nothing can change here
  SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
  # diffusive exchange
  DE<-(DO*DOsat)*(2.2*10^-5)*(0.03^-1)*60*10^-2 #nothing to change here but the 60/10 situation
  # below is eq 2. but i am not sure the do term should be ther
  #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
  dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
  return(list(dDO))
}


dat$DawnDO_Mod<-NA 
for (i in 1:NROW(dat))
{
  DO_dusk<-dat$DO_dusk[i]
  parms=c(tempC = dat$tempC[i], Z = dat$Z[i])
  solution<- deSolve::ode(
    y=DO_dusk, 
    times=c(0:(10*60)), 
    func=DO_fun,
    parms= parms,
    method="euler")
  dat$DawnDO_Mod[i]<-solution[601,2]
}

dat$DawnDO_Mod<-dat$DO_dusk-dat$DawnDO_Mod

plot(dat$DO_dawn~dat$DawnDO_Mod, main="Dawn Dissolved Oxygen", xlab="Model DO", ylab="True DO", ylim=c(0,9), xlim=c(0,9))

abline(0,1)
