#DO Model Function----
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

####Check Model against real data----
dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")
dat$Z<- dat$depth2 #9 boards in the WCS
dat$tempC<-dat$Temp_C
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

plot(dat$DO_dawn~dat$DawnDO_Mod, main="Dawn Dissolved Oxygen (all depths)", xlab="Model DO", ylab="True DO", ylim=c(0,9), xlim=c(0,9))
abline(0,1)

dat1<-subset(dat, dat$depth==1)
plot(dat1$DO_dawn~dat1$DawnDO_Mod, main="Dawn Dissolved Oxygen (1m)", xlab="Model DO", ylab="True DO", ylim=c(0,9), xlim=c(0,9))
abline(0,1)
dat6<-subset(dat, dat$depth==.6)
plot(dat6$DO_dawn~dat6$DawnDO_Mod, main="Dawn Dissolved Oxygen (0.6m)", xlab="Model DO", ylab="True DO", ylim=c(0,9), xlim=c(0,9))
abline(0,1)
dat2<-subset(dat, dat$depth==0.2)
plot(dat2$DO_dawn~dat2$DawnDO_Mod, main="Dawn Dissolved Oxygen (0.2m)", xlab="Model DO", ylab="True DO", ylim=c(0,9), xlim=c(0,9))
abline(0,1)


####Run model for bathymetric map----
#dat <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat <- read.csv("DO-Sampling/Export_Output.csv") #using a small file with fewer points to check model
boards<-c(9) #baby range for now 0-17
elevation<-66.45402+(0.2083*boards)
tempC<-c(20:22) #baby range for now 0-30
DO_dusk<-c(9:10) #baby range for now 5-10
combos<-expand.grid(tempC=tempC, DO_dusk=DO_dusk, elevation=elevation)
combos$SA<-NA

for(i in 1:nrow(combos)){
  dat<- subset(dat, dat$Elevation < (combos$elevation[i]))
  dat$Z <- c((combos$elevation[i]-dat$Elevation))
  DO_dusk<-combos$DO_dusk[i]
  tempC<-combos$tempC[i]
  dat$DawnDO_Mod<-NA 
  for(j in 1:nrow(dat)){    #nested DO model, this one only seems to be running for 10 rows
   parms=c(tempC = tempC, Z = dat$Z[j])
    solution<- deSolve::ode(
      y=DO_dusk, 
      times=c(0:(10*60)), 
      func=DO_fun,
      parms= parms,
      method="euler")
    dat$DawnDO_Mod[j]<-solution[601,2] #pull last value "dawn"
  }
  dat$DawnDO_Mod<-combos$DO_dusk[i]-dat$DawnDO_Mod #calculate surface area
  Z2<-length(which(dat$DawnDO_Mod > (4.5)))
  combos$SA[i]<-Z2*4/10000
}
