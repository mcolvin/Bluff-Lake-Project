#' lake hydrodynanamic model 
#'
#' This function simulates water surface elevation dynamics for Bluff Lake given inflows and outflows.
#' @param t what time step
#' @param x lake volume in cubic meters
#' @param parms a vector of parameters used to simulate lake dynamics
#' @return a list of lake volume, water surface elvation, lake elevation (same as water surface elevation the amount of water entering the lake, amount of water leaving the lake through the emergency spillway, and the amount of water going out the water control structure. 
#' @keywords weir equation 
#' @export
#' @examples
#' weir()

wse_sim<-function(t,x,parms)
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

