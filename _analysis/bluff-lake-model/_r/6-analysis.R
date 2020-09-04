library(deSolve)

# model will be hourly


wse_dyn<-function(t,x,parms)
    {
    # lake volume in m^3
    V<-x[1]
    # convert lake volume to water surfac elevation
    wse<- Vol_2_EL(V)
    # lake surface area
    sa<- Vol_2_SA(V)
    # acceleration due to gravity
    acc_due_to_gravivity<- (2*32)^0.5
 
    # water releasing over the WCS
    board<-0  # Board_Time(ttt) need to link hour to doy
    
    # water control structure head
    # set head to zero when wse<=board
    wcs_head<- min(0,wse-wcs_width)
    # amount of water flowing out of each bay of the wcs
    wcs_out<- wcs_head^(3/2)*wcs_width*0.66*0.6*acc_due_to_gravivity



    
    # water coming into the lake from the Noxubee
    V_inn_1<- 1   
    # this adds 1 m^3/hr 
    dV<- V_inn_1-wcs_out
    return(list(V=dV,wse=wse, sa=sa))
    }
# initialize lake volume in m^3
ini_values<-c(258323640)
parameters<-NULL # no parameters yet
solution<- deSolve::ode(
    y=ini_values, 
    times=c(0:(10*60)), 
    func=wse_dyn, 
    parms=parameters, 
    method="rk4")
colnames(solution) <- c("time", "V", "wse","sa")
solution<-as.data.table(solution)
plot(V~time,solution,ylab="Lake volume",las=1,main="")
plot(sa~time,solution,ylab="Lake surface area",las=1,main="")
plot(wse~time,solution,ylab="Lake water surface elevation",las=1,main="")




