dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")

#DO Model Function----
DO_fun<-function(t,x,parms)
{
  DO<-x
  tempC<-parms["tempC"]
  Z<-parms["Z"]
  k<-parms["k"]
  WR<-0.07203515/60 #lake average water respiration in g/m^3/min
  # DO Saturation at a given temperature        
  DOsat<-4.09+(10.5*exp(-0.0371*tempC)) #nothing can change here
  SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
  # diffusive exchange
  DE<-(DO*DOsat)*(2.2*10^-5)*(k^-1)*60*10^-2 #nothing to change here but the 60/10 situation
  # below is eq 2. but i am not sure the do term should be ther
  #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
  dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
  return(list(dDO))
}


#Optimizing K----
Do_fit<-function(p,dat){
  K<-p
  dat$Z<- dat$depth2-dat$depth #9 boards in the WCS
  dat$tempC<-dat$Temp_C
  dat$DawnDO_Mod<-NA 
  for (i in 1:NROW(dat)){
    DO_dusk<-dat$DO_dusk[i]
    parms=c(tempC = dat$tempC[i], Z = dat$Z[i], k=(K*(dat$Z[i]/dat$depth2[i])))
    solution<- deSolve::ode(
      y=DO_dusk, 
      times=c(0:(10*60)), 
      func=DO_fun,
      parms= parms,
      method="euler")
    dat$DawnDO_Mod[i]<-solution[601,2]
  }
  dat$DawnDO_Mod<-dat$DO_dusk-dat$DawnDO_Mod
  # compare predicted to observed
  resid<- dat$DO_dawn-dat$DawnDO_Mod
  ss_res<- sum(resid^2) # sum of square residuals, minimize error
  return(ss_res)
}

# test to see if function is behaving
Do_fit (p=0.08,dat)

Fig<-optimize(Do_fit, dat, interval=c(0,0.15), tol=0.01)







#Using new k value 
####Check Model against real data----
dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")
dat$Z<- dat$depth2-dat$depth #9 boards in the WCS
dat$tempC<-dat$Temp_C
dat$k<-0.0606*(dat$Z/dat$depth2) #depth from pt 2 bttmm/total depth
dat$DawnDO_Mod<-NA 
for (i in 1:NROW(dat))
{
  DO_dusk<-dat$DO_dusk[i]
  parms=c(tempC = dat$tempC[i], Z = dat$Z[i], k=dat$k[i])
  solution<- deSolve::ode(
    y=DO_dusk, 
    times=c(0:(10*60)), 
    func=DO_fun,
    parms= parms,
    method="euler")
  dat$DawnDO_Mod[i]<-solution[601,2]
}

dat$DawnDO_Mod<-dat$DO_dusk-dat$DawnDO_Mod

plot(dat$DO_dawn~dat$DawnDO_Mod, main="Dawn Dissolved Oxygen at depth x", xlab="Model DO", ylab="True DO", ylim=c(1,9), xlim=c(1,9))
abline(0,1)
dat1<-subset(dat, dat$depth>=1)
points(dat1$DO_dawn~dat1$DawnDO_Mod, col="blue")
dat6<-subset(dat, dat$depth<1&dat$depth>0.5)
points(dat6$DO_dawn~dat6$DawnDO_Mod, col="green")
dat2<-subset(dat, dat$depth<0.5)
points(dat2$DO_dawn~dat2$DawnDO_Mod, col="red")
legend("topleft",legend=c("x>1","1>x>0.5","0.5>x"),
       col=c("blue","green","red"),pch=1,bg="white")


plot(dat$DO_dawn~dat$DawnDO_Mod, main="Dawn Dissolved Oxygen at Temperature x", xlab="Model DO", ylab="True DO", ylim=c(1,9), xlim=c(1,9), type="n")
abline(0,1)
dat1<-subset(dat, dat$Temp_C>=30)
points(dat1$DO_dawn~dat1$DawnDO_Mod, col="blue", pch=1)
dat6<-subset(dat, dat$Temp_C<30&dat$Temp_C>25)
points(dat6$DO_dawn~dat6$DawnDO_Mod, col="green", pch=2)
dat2<-subset(dat, dat$Temp_C<25&dat$Temp_C>20)
points(dat2$DO_dawn~dat2$DawnDO_Mod, col="red", pch=3)
dat9<-subset(dat, dat$Temp_C<20)
points(dat9$DO_dawn~dat9$DawnDO_Mod, col="brown", pch=4)
legend("topleft",legend=c("x>30","30>x>25","25>x>20","x<20"),
       col=c("blue","green","red","brown"),pch=c(1:4),bg="white")

