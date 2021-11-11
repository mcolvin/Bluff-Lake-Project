#' lake hydrodynanamic model with multiple decisions
#'
#' This function simulates water surface elevation dynamics for Bluff Lake given inflows and outflows.
#' @param t what time step
#' @param x lake volume in cubic meters
#' @param parms a vector of parameters used to simulate lake dynamics
#' @return a list of lake volume, water surface elevation, lake elevation (same as water surface elevation the amount of water entering the lake, amount of water leaving the lake through the emergency spillway, and the amount of water going out the water control structure. 
#' @keywords weir equation 
#' @export
#' @examples
#' weir()
wse_sim_decisions<-function(t,x,parms)
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
  
   