library(deSolve)

# model will be hourly


wse_dyn<-function(t,x,parms)
    {
    # lake volume in m^3
    V<-x[1]
    # convert lake volume to water surfac elevation
    wse<- Vol_2_EL(V)
    # surface area
    sa<- Vol_2_SA(V)
    
    
    
    
    lake_area<- 0# f(V)
    acc_due_to_gravivity<- (2*32)^0.5
    # water coming into the lake from the Noxubee
    V_inn_1<- 1    
    # water releasing over the WCS
    board<-0
    wcs_head<-0 
    V_inn_2<-1
    V_inn_3<-1
    em_spill<-1
    wcs<-1
    
    dV<- 1#inn_1 + inn_2 + inn_3 - em_spill - wcs
    return(list(V=dV,wse=wse))
    }
ini_values<-c(258323640)
parameters<-NULL
solution<- deSolve::ode(
    y=ini_values, 
    times=c(0:(10*60)), 
    func=wse_dyn, 
    parms=parameters, 
    method="rk4")
colnames(solution) <- c("time", "V", "wse")
solution<-as.data.table(solution)
plot(solution,ylab="DO mg/L",las=1,main="")




