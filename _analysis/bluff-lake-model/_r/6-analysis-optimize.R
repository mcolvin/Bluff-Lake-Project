source("_r/1-global.R")
source("_r/2-functions.R")
source("_r/3-load-and-clean.R")


#----------------------------------------------------------------------
# 
#  lake hydrodynanamic model
#
#----------------------------------------------------------------------
Do_fit<-function(parms,dat){
  a=parms[1]
  b=parms[2]
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
  #DOY
  
  doy<-DOYfun(t) #t
  
  # acceleration due to gravity m/sec^2
  acc_due_to_gravity<- 9.81
  # wse at intake
  ele_intake<-predict(gam_4, newdata=data.frame(Q_bl=Q_blfun(t), doy))
  # wse lake
  ele_lake<-  wse# wse_lake(t)
  
  #----------------------------------------------------------------------
  # 
  #  water releasing over the WCS
  #
  #----------------------------------------------------------------------
  # board elevation
  WCS1_wse<-c(68.40,68.40,68.40,68.40,68.40,
              68.40,68.40) # seven bays
  # wcs_width<-rep(1.6764,8)
  # water control structure head
  # set head to zero when wse<=board
  wcs_head<- sapply(WCS1_wse,function(x)
  {
    max(0,ele_lake-WCS1_wse)    
  })
  # amount of water flowing out of each bay of the wcs
  wcs_out<- weir(g=9.81,w=wcs_width[1], h=wcs_head[1])+
    weir(g=9.81,w=wcs_width[2], h=wcs_head[2])+
    0*weir(g=9.81,w=wcs_width[3], h=wcs_head[3])+
    0*weir(g=9.81,w=wcs_width[4], h=wcs_head[4])+
    0*weir(g=9.81,w=wcs_width[5], h=wcs_head[5])+
    0*weir(g=9.81,w=wcs_width[6], h=wcs_head[6])+
    weir(g=9.81,w=wcs_width[7], h=wcs_head[7])
  wcs_out<-(wcs_out*60*60)*0.8        #per hour
  # emergency spillway
  #emergency overflow measurements (meters)
  EOFwidth<-23
  EOFheight<-68.698
  EOF_head<-max(0,ele_lake-EOFheight) 
  EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
  EOF_out<-EOF_out*60*60     #per hour
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
  # convert to cubic metrs per 60 minutes
  #p<-ifelse(ele_intake>68.4&doy<150, p, 1)
  #P<-ifelse(ele_intake>68.4&doy>325, p, 1)
  p<-a+(intake_in*b)
  intake_in<-(intake_in*60*60)*p

  #----------------------------------------------------------------------
  # 
  #  change in volume
  #
  #----------------------------------------------------------------------
  V<-V+(intake_in-(wcs_out+EOF_out)) # iteration needs the actual volume, not dV
  return(list(V=V,wse=wse,
              ele_lake=ele_lake,
              sa=sa,
              intake_in=intake_in,
              EOF_out=EOF_out,
              wcs_out=wcs_out))
}


Q_blfun<- approxfun(model_data$cont_time, model_data$Q_bl)
DOYfun<- approxfun(model_data$cont_time, model_data$doy)

# initialize lake volume in m^3 given elevation at t0
ini_values<-EL_2_Vol(wse_lake(1))
parameters<-NULL # no parameters yet
solution<- ode(
    y=ini_values, 
    times=model_data$cont_time, 
    func=wse_dyn, 
    parms=parameters, 
    method="iteration")
colnames(solution)[2] <- "V"
solution<-as.data.table(solution)
solution<-cbind(solution,model_data)

names(solution)[1]<-"time1"
solution$date<-as.Date(solution$dt)


preds <- solution$wse
actual <- solution$wse_lake
rss <- sum((preds - actual) ^ 2, na.rm=T)  ## residual sum of squares
return(rss)
}


Fig<-optim(par=c(1,0.5), fn=Do_fit, method="L-BFGS-B", 
           lower=c(0, 0), upper=c(2, 1), dat=model_data)
#exponent to 3?



a=1.408
b=0.302


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
  #DOY
  
  doy<-DOYfun(t) #t
  
  # acceleration due to gravity m/sec^2
  acc_due_to_gravity<- 9.81
  # wse at intake
  ele_intake<-predict(gam_4, newdata=data.frame(Q_bl=Q_blfun(t), doy))
  # wse lake
  ele_lake<-  wse# wse_lake(t)
  
  #----------------------------------------------------------------------
  # 
  #  water releasing over the WCS
  #
  #----------------------------------------------------------------------
  # board elevation
  WCS1_wse<-c(68.40,68.40,68.40,68.40,68.40,
              68.40,68.40) # seven bays
  # wcs_width<-rep(1.6764,8)
  # water control structure head
  # set head to zero when wse<=board
  wcs_head<- sapply(WCS1_wse,function(x)
  {
    max(0,ele_lake-WCS1_wse)    
  })
  # amount of water flowing out of each bay of the wcs
  wcs_out<- weir(g=9.81,w=wcs_width[1], h=wcs_head[1])+
    weir(g=9.81,w=wcs_width[2], h=wcs_head[2])+
    0*weir(g=9.81,w=wcs_width[3], h=wcs_head[3])+
    0*weir(g=9.81,w=wcs_width[4], h=wcs_head[4])+
    0*weir(g=9.81,w=wcs_width[5], h=wcs_head[5])+
    0*weir(g=9.81,w=wcs_width[6], h=wcs_head[6])+
    weir(g=9.81,w=wcs_width[7], h=wcs_head[7])
  wcs_out<-(wcs_out*60*60)*0.8        #per hour
  # emergency spillway
  #emergency overflow measurements (meters)
  EOFwidth<-23
  EOFheight<-68.698
  EOF_head<-max(0,ele_lake-EOFheight) 
  EOF_out<-broad_weir(w=EOFwidth, h=EOF_head)
  EOF_out<-EOF_out*60*60     #per hour
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
  # convert to cubic metrs per 60 minutes
  p<-a+(intake_in*b)
  intake_in<-(intake_in*60*60)*p
  
  #----------------------------------------------------------------------
  # 
  #  change in volume
  #
  #----------------------------------------------------------------------
  V<-V+(intake_in-(wcs_out+EOF_out)) # iteration needs the actual volume, not dV
  return(list(V=V,wse=wse,
              ele_lake=ele_lake,
              sa=sa,
              intake_in=intake_in,
              EOF_out=EOF_out,
              wcs_out=wcs_out))
}


Q_blfun<- approxfun(model_data$cont_time, model_data$Q_bl)
DOYfun<- approxfun(model_data$cont_time, model_data$doy)

# initialize lake volume in m^3 given elevation at t0
ini_values<-EL_2_Vol(wse_lake(1))
parameters<-NULL # no parameters yet
solution<- ode(
  y=ini_values, 
  times=model_data$cont_time, 
  func=wse_dyn, 
  parms=parameters, 
  method="iteration")
colnames(solution)[2] <- "V"
solution<-as.data.table(solution)
solution<-cbind(solution,model_data)

names(solution)[1]<-"time1"
solution$date<-as.Date(solution$dt)

ggplot()+geom_line(data=solution, aes(x=dt, y=wse_lake))+theme_classic()+
  geom_line(aes(x=solution$dt, y=solution$wse), color="blue")+xlab("Day of Year")+
  ylab("Water Surface Elevation (m)")

preds <- solution$wse
actual <- solution$wse_lake
rss <- sum((preds - actual) ^ 2, na.rm=T)  ## residual sum of squares
tss <- sum((actual - mean(actual, na.rm=T)) ^ 2, na.rm=T)  ## total sum of squares
rsq <- 1 - rss/tss

