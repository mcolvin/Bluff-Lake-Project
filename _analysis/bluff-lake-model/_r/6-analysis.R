

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
    print(x[1])
    # convert lake volume to water surface elevation
    wse<- Vol_2_EL(V)
    # lake surface area
    sa<- Vol_2_SA(V)
    # acceleration due to gravity m/sec^2
    acc_due_to_gravity<- 9.81
    # wse at intake
    ele_intake<-wse_intake(t)
    # wse lake
    ele_lake<-  wse# wse_lake(t)
    
    
    #----------------------------------------------------------------------
    # 
    #  water releasing over the WCS
    #
    #----------------------------------------------------------------------
    # board elevation
    WCS1_wse<-c(68.39712,68.39712,68.39712,68.39712,68.39712,
        68.39712,68.39712,68.39712) # eight bays
    wcs_width<-rep(1.6764,8)
    # water control structure head
    # set head to zero when wse<=board
    wcs_head<- sapply(WCS1_wse,function(x)
        {
        max(0,ele_lake-WCS1_wse)    
        })
    # amount of water flowing out of each bay of the wcs
    wcs_out<- weir(g=9.81,w=wcs_width[1], h=wcs_head[1])+
        weir(g=9.81,w=wcs_width[2], h=wcs_head[2])+
        weir(g=9.81,w=wcs_width[3], h=wcs_head[3])+
        weir(g=9.81,w=wcs_width[4], h=wcs_head[4])+
        weir(g=9.81,w=wcs_width[5], h=wcs_head[5])+
        weir(g=9.81,w=wcs_width[6], h=wcs_head[6])+
        weir(g=9.81,w=wcs_width[7], h=wcs_head[7])+
        weir(g=9.81,w=wcs_width[8], h=wcs_head[8])
    wcs_out<-wcs_out*60*30

    #----------------------------------------------------------------------
    # 
    #  water coming in (intake)
    #
    #----------------------------------------------------------------------
    # board elevation
    intake_board_wse<- c(68.800,68.800) # meters bay 1 and 2
    intake_width<- c(1.6764,1.6764) # meters bay 1 and 2
    intake_head<- c(max(0,ele_intake-intake_board_wse[1]),
        max(0,ele_intake-intake_board_wse[2]))
    # water inputs to intake (cms)
    intake_in<-weir(g=9.81,h=intake_head[1],w=intake_width[1])+ # bay 1
        weir(g=9.81,h=intake_head[2],w=intake_width[2]) # bay 2
    # convert to cubic meters per 30 minutes
    intake_in<-intake_in*60*30
    
    #----------------------------------------------------------------------
    # 
    #  change in volume
    #
    #----------------------------------------------------------------------
    V<-V+(intake_in-wcs_out)# iteration needs the actual volume, not dV
    return(list(V=V,wse=wse,
        ele_lake=ele_lake,
        sa=sa,intake_in=intake_in,
        wcs_out=wcs_out))
    }
# initialize lake volume in m^3 given elevation at t0
ini_values<-EL_2_Vol(wse_lake(1))
parameters<-NULL # no parameters yet
solution<- ode(
    y=ini_values, 
    times=model_data$cont_time[1:10000], 
    func=wse_dyn, 
    parms=parameters, 
    method="iteration")
colnames(solution)[2] <- "V"
solution<-as.data.table(solution)
plot(V~time,solution,ylab="Lake volume",las=1,main="")
plot(wse~time,solution,
    ylab="Water Surface elevation, meters",
    las=1,main="",type='l',ylim=c(68.5,69.5))
points(wse_lake~cont_time,model_data,type='l',
    col="blue")
plot(ele_lake~time,solution,ylab="Water Surface elevation, meters",las=1,main="")
plot(intake_in~time,solution,ylab="Input from intake (m^3/minute)",las=1,main="")
plot(wcs_out~time,solution,ylab="Output from WCS (m^3/minute)",las=1,main="")




solution<-as.data.table(solution)
plot(V~time,solution,ylab="Lake volume",las=1,main="")
plot(sa~time,solution,ylab="Lake surface area",las=1,main="")
plot(wse~time,solution,ylab="Lake water surface elevation",las=1,main="")






