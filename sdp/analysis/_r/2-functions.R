#----------------------------------------------------------------------
# 
#  Weir equation function 
#  https://www.engineeringtoolbox.com/weirs-flow-rate-d_592.html
#----------------------------------------------------------------------
weir<-function(w=NULL,h=NULL)
    {
    g=9.81 # acceleration due to gravity m/sec^2
    Q<-(2/3)*0.66*(2*g)^(0.5)*w*h^(3/2)
    return(Q)
    }
    
#----------------------------------------------------------------------
# 
#  BROAD CRESTED WEIR
#
#----------------------------------------------------------------------
broad_weir<-function(w=NULL,h=NULL)
    {
    C=2.7                   # Tracy 1957
    w_ft<- w*3.281          # convert meters to feet
    h_ft <- h*3.281         # convert meters to feet
    Q<-(C*w_ft*h_ft^(3/2))  # discharge in cfs
    Q<- Q*0.0283            # convert to cms
    return(Q)
    }


#----------------------------------------------------------------------
# 
# Function for turning water on/off 
#
#----------------------------------------------------------------------
In_out_el<-function(location, WSE, discharge)
    {
    # Function for turning water on/off 
    ## inputs
    ### location of inflow/outflow to lake
    ### WSE in meters above sea level
    ### discharge in cubic meters per second 
    ## output
    ### x = indictor of water going into the lake (1), no connection (0), out of lake (-1)
    
    # MAIN INFLOW TO BLUFF LAKE
    # outlet of griffin slough
    if(location==Bridge_1 & WSE<67.39) {x<-0}# no water
    if(location==Bridge_1 & WSE>67.39) {x<-1}# water moving in
    
    # discharge at Macon in cms
    if(location==Bridge_2 & WSE>68.11 & discharge<13.25) {x<--1}# water moving out
    if(location==Bridge_2 & WSE<68.11 & discharge<13.25) {x<-0}# no water 
    if(location==Bridge_2 & discharge>13.25) {x<-1}
    
    # water moving in at B2 is a function of discharge not WSE
    # discharge at Macon in cms
    if(location==Notch_1 & WSE<68.38 | discharge<6.56) {x<-0}# no water 
    if(location==Notch_1 & WSE>68.38 | discharge>6.56) {x<-1}# water moving in

    # SECONDARY INFLOW; WATER FROM NOXUBEE
    if(location==WCS2 & WSE<68.23) {x<-0}# no water
    if(location==WCS2 & WSE>68.23) {x<-1}# water moving in

    if(location==Notch_2 & WSE>68.75 & discharge<24.15) {x<--1}# water moving out
    if(location==Notch_2 & WSE<68.75 & discharge<24.15) {x<-0}# no water
    if(location==Notch_2 & WSE<69.20 &discharge>24.15) {x<-1}# water moving in

    return(x)
    }




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
    #sa<- Vol_2_SA(V)
    #DOY
    #doy<-DOYfun(t) #t
    # wse at intake
    ele_intake<-q_intake(t)# predict(gam_4, newdata=data.frame(Q_bl=Q_blfun(t), doy))
    # wse lake
    ele_lake<-wse   # wse_lake(t)
    
    Board<- parms[["board_elevation"]]
    wcs_width<- parms[["wcs_width"]]
    D_Q<- parms[["D_Q"]]*releases(t) # 0 when no release, D when release occurs

    #----------------------------------------------------------------------
    # 
    #  water releasing over the WCS
    #
    #----------------------------------------------------------------------
    # board elevation
    WCS1_wse<-rep(as.numeric(Board),7) # seven bays
    # wcs_width<-rep(1.6764,8)
    # water control structure head
    # set head to zero when wse<=board
    wcs_head<- sapply(WCS1_wse,function(x)
        {
        max(0,ele_lake-WCS1_wse)    
        })
        
    # amount of water flowing out of each bay of the wcs
    wcs_out<- weir(w=wcs_width[1], h=wcs_head[1])+
        weir(w=wcs_width[2], h=wcs_head[2])+
        0*weir(w=wcs_width[3], h=wcs_head[3])+
        0*weir(w=wcs_width[4], h=wcs_head[4])+
        0*weir(w=wcs_width[5], h=wcs_head[5])+
        0*weir(w=wcs_width[6], h=wcs_head[6])+
        weir(w=wcs_width[7], h=wcs_head[7])
    wcs_out<-(wcs_out*60*60)*0.8        # per hour
    
    #----------------------------------------------------------------------
    # emergency spillway
    #----------------------------------------------------------------------
    # emergency overflow measurements (meters)
    ## width
    EOFwidth<-  23
    ## elevation
    EOFheight<-68.698
    ## calculate head, return 0 if head<0
    EOF_head<-max(0,ele_lake-EOFheight) 
    ## calculate discharge using broad wier equation
    EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
    ## convert to cubic meters per hour
    EOF_out<-EOF_out*60*60     
    
    #PfD<-PfDfun(t)
    
    
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
    intake_in<-weir(h=intake_head[1],w=intake_width[1])+ # bay 1
      weir(h=intake_head[2],w=intake_width[2]) # bay 2
    # convert to cubic meters per 60 minutes
    intake_in<-intake_in*60*60
    
    #----------------------------------------------------------------------
    # 
    #  change in volume
    #
    #----------------------------------------------------------------------
    V<-V+(intake_in-(wcs_out+EOF_out+D_Q))
   

    #----------------------------------------------------------------------
    # 
    #  RETURN VALUES
    #
    #----------------------------------------------------------------------
    return(list(V=V,                # LAKE VOLUME
        wse=wse,                    # WATER SURFACE ELEVATION
        ele_lake=ele_lake,          # LAKE ELEVATION (SAME AS WATER SURFACE ELEVATION)
        #sa=sa, # can do after
        intake_in=intake_in,        # AMOUNT OF WATER COMING IN
        EOF_out=EOF_out,            # AMOUNT OF WATER GOING OUT THE EMERGENCY SPILLWAY
        wcs_out=wcs_out))       # AMOUNT OF WATER GOING OUT THE WATER CONTROL STRUCTURE
  }
  
      
#----------------------------------------------------------------------
# 
#  lake hydrodynanamic model multiple release decisions
#
#----------------------------------------------------------------------
wse_dyn_D<-function(t,x,parms)
    {    
    #----------------------------------------------------------------------
    # 
    #  parameters and conversions
    #
    #----------------------------------------------------------------------
    # lake volume in m^3
    V<-x
    # convert lake volume to water surface elevation
    wse<- Vol_2_EL(V)
    # inflow
    ele_intake<-q_intake(t)
    # wse lake
    ele_lake<-wse  
    
    Board<- parms[["board_elevation"]]
    wcs_width<- parms[["wcs_width"]]
	
    # timing of release
    release_water<-releases(t)  
    # volume of water released
    amount_released<- parms[["D_Q"]]*release_water # 0 when no release, D when release occurs
    # account for the water in system (can't release more than is in there
    # no water is released if the the release drains the lake
    amount_released<- ifelse(V>amount_released, amount_released, 0)*release_water
    
    #----------------------------------------------------------------------
    # 
    #  water releasing over the WCS
    #
    #----------------------------------------------------------------------
    # board elevation
    WCS1_wse<-rep(as.numeric(Board),7) # seven bays
    # wcs_width<-rep(1.6764,8)
    # water control structure head
    # set head to zero when wse<=board

    # amount of water flowing out of each bay of the wcs
    wcs_out<- sapply(seq_along(ele_lake),function(x)
        {        
        wcs_head<-ele_lake[x]-WCS1_wse
        wcs_head<-ifelse(wcs_head>0,wcs_head,0)
        wcs_out<-weir(w=wcs_width[1], h=wcs_head[1])+
            weir(w=wcs_width[2], h=wcs_head[2])+
            0*weir(w=wcs_width[3], h=wcs_head[3])+
            0*weir(w=wcs_width[4], h=wcs_head[4])+
            0*weir(w=wcs_width[5], h=wcs_head[5])+
            0*weir(w=wcs_width[6], h=wcs_head[6])+
            weir(w=wcs_width[7], h=wcs_head[7])
        return((wcs_out*60*60)*0.8) # per hour
        })
    
  
    
    #----------------------------------------------------------------------
    # emergency spillway
    #----------------------------------------------------------------------
    # emergency overflow measurements (meters)
    ## width
    EOFwidth<-  23
    ## elevation
    EOFheight<-68.698
    ## calculate head, return 0 if head<0
    EOF_out<- sapply(seq_along(ele_lake),function(x)
        {        
        EOF_head<-ele_lake[x]-EOFheight
        EOF_head<-ifelse(EOF_head>0,EOF_head,0)
        ## calculate discharge using broad wier equation
        EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
        ## convert to cubic meters per hour
        return(EOF_out*60*60)
        })
   
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
    intake_in<-weir(h=intake_head[1],w=intake_width[1])+ # bay 1
      weir(h=intake_head[2],w=intake_width[2]) # bay 2
    # convert to cubic meters per 60 minutes
    intake_in<-rep(intake_in*60*60,length(ele_lake))
    #intake_in<-intake_in*60*60
    
    #----------------------------------------------------------------------
    # 
    #  change in volume
    #
    #----------------------------------------------------------------------
    V<-V+(intake_in-(wcs_out+EOF_out+amount_released))
   

    #----------------------------------------------------------------------
    # 
    #  RETURN VALUES
    #
    #----------------------------------------------------------------------
    return(list(V=V,                # LAKE VOLUME
        ele_lake=ele_lake,          # LAKE ELEVATION (SAME AS WATER SURFACE ELEVATION)
        intake_in=intake_in,        # AMOUNT OF WATER COMING IN
        EOF_out=EOF_out,            # AMOUNT OF WATER GOING OUT THE EMERGENCY SPILLWAY
        wcs_out=wcs_out,            # AMOUNT OF WATER GOING OUT THE WATER CONTROL STRUCTURE
        release_water=release_water))           
  }
  
     
      