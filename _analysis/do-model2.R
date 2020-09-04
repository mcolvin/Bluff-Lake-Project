#Creating a function for DO from dawn to dusk
DO_dusk<-10
parameters <- c(
    tempC = 25, #Same as SOU above just SOU/min 
    Z = 2) # depth
DO_fun<-function(t,x,parms)
    {
    DO<-x
    tempC<-parms["tempC"]
    Z<-parms["Z"]
    WR<-0.07203515/60 #lake average water respiration in mg/L/min
    # DO Saturation at a given temperature        
    DOsat<-4.09+(10.5*exp(-0.0371*tempC)) 
    SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
    # diffusive exchange
    DE<-(DO*DOsat)*(2.2*10^-5)*(0.06^-1)*
        60*10^-2 
    # below is eq 2. but i am not sure the do term should be ther
    #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
    # I think this is the right ode
    dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
    return(list(dDO))
    }
solution<- deSolve::ode(
    y=DO_dusk, 
    times=c(0:(10*60)), 
    func=DO_fun, 
    parms=parameters, 
    method="euler")
plot(solution,ylab="DO mg/L",xlab="Time in minutes",las=1,main="")
tail(solution, n=1)



##run function for map
#Bring in depths
dat2 <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat2$depth<- 66.52752-dat2$POINT_Z #9 boards in the WCS
dat2$depth<-ifelse(dat2$depth<=0, 0, dat2$depth) #remove dry areas
#assign starting temperatures and DO
BluffDO<-read.csv("DO-Sampling/_dat/Real9BDOmap.csv")
BluffTemp<-read.csv("DO-Sampling/_dat/Real9BTempmap.csv")
BluffTemp$DO<-BluffDO$DO
write.csv(Bluff, "DO-Sampling/_dat/RealStartTemp_DO.csv")

#assign DO and Temp to nearest depth point
dat2$long<-dat2$ï..POINT_X
dat2$lat<-dat2$POINT_Y
newdat<- merge(dat2, BluffTemp, by=c())
