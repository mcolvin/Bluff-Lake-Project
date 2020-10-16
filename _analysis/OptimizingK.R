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
