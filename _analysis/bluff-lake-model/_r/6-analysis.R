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
    # water coming into the lake from the Noxubee
    V_inn_1<- 1    
    # water releasing over the WCS
    board<-0# Board_Time(ttt) need to link hour to doy

    # this adds 1 m^3/hr 
    dV<- 1
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
plot(V~t,solution,ylab="Lake volume",las=1,main="")
plot(sa~t,solution,ylab="Lake surface area",las=1,main="")
plot(wse~t,solution,ylab="Lake water surface elevation",las=1,main="")




