

#----------------------------------------------------------------------
# 
#  lake hydrodynanamic model
#
#----------------------------------------------------------------------
wse_dyn<-function(t,x,parms)
    {
    
    #----------------------------------------------------------------------
    # 
    #  parameters and conversions
    #
    #----------------------------------------------------------------------
    # lake volume in m^3
    V<-x[1]
    # convert lake volume to water surface elevation
    wse<- Vol_2_EL(V)
    # lake surface area
    sa<- Vol_2_SA(V)
    # acceleration due to gravity m/sec^2
    acc_due_to_gravity<- 9.81
    # wse at intake
    ele_intake<-wse_intake(t)
    # wse lake
    ele_lake<-wse_lake(t)
    
    
    #----------------------------------------------------------------------
    # 
    #  water releasing over the WCS
    #
    #----------------------------------------------------------------------
    # board elevation
    WCS1_wse<-68.39712
    # water control structure head
    # set head to zero when wse<=board
    wcs_head<- min(0,wse-ele_lake)    
    # amount of water flowing out of each bay of the wcs
    wcs_out<- weir(g=9.81,w=wcs_width, h=wcs_head)



    #----------------------------------------------------------------------
    # 
    #  water coming in (intake)
    #
    #----------------------------------------------------------------------
    # board elevation
    intake_board_wse<- 68.800 # meters
    intake_width<- 1.6764 # meters
    intake_head<- min(0,ele_intake-intake_board_wse)
    # water passing over intake
    intake_in<- intake_head^(3/2)*intake_width*0.66*0.6*acc_due_to_gravity
    
    
    #----------------------------------------------------------------------
    # 
    #  change in volume
    #
    #----------------------------------------------------------------------
    # this adds 1 m^3/hr 
    dV<- 0
    return(list(V=dV,wse=wse, sa=sa,intake_in=intake_in))
    }
# initialize lake volume in m^3 given elevation at t0
ini_values<-EL_2_Vol(wse_lake(0))
parameters<-NULL # no parameters yet
solution<- deSolve::ode(
    y=ini_values, 
    times=model_data$cont_time, 
    func=wse_dyn, 
    parms=parameters, 
    method="rk4")
colnames(solution) <- c("time", "V", "wse","sa")
solution<-as.data.table(solution)
plot(V~time,solution,ylab="Lake volume",las=1,main="")
plot(sa~time,solution,ylab="Lake surface area",las=1,main="")
plot(wse~time,solution,ylab="Lake water surface elevation",las=1,main="")




