


wse_dyn<-function(t,x,parms)
    {    
    V_inn_1<-1
    V_inn_2<-1
    V_inn_3<-1
    em_spill<-1
    wcs<-1
    
    dV<- inn_1 + inn_2 + inn_3 - em_spill - wcs
    return(list(dV))
    }




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
    DE<-(DO*DOsat)*(2.2*10^-5)*(0.03^-1)*
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
    method="rk4")
plot(solution,ylab="DO mg/L",las=1,main="")




