library(data.table)

#DO Model Function----
DO_fun<-function(t,x,parms)
{
  DO<-x
  tempC<-parms["tempC"]
  Z<-parms["Z"]
  k<-parms["k"]
  WR<-0.07203515/60 #lake average water respiration in g/m^3/min
  # DO Saturation at a given temperature        
  DOsat<-4.09+(10.5*exp(-0.0371*tempC)) #nothing can change here
  SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
  # diffusive exchange
  DE<-(DO*DOsat)*(2.2*10^-5)*(k^-1)*60*10^-2 #nothing to change here but the 60/10 situation
  # below is eq 2. but i am not sure the do term should be ther
  #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
  dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
  return(list(dDO))
}

# ####Check Model against real data----
# dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")
# dat$Z<- dat$depth2-dat$depth #9 boards in the WCS
# dat$tempC<-dat$Temp_C
# dat$k<-0.06*(dat$Z/dat$depth2) #depth from pt 2 bttmm/total depth
# dat$DawnDO_Mod<-NA 
# for (i in 1:NROW(dat))
# {
#   DO_dusk<-dat$DO_dusk[i]
#   parms=c(tempC = dat$tempC[i], Z = dat$Z[i], k=dat$k[i])
#   solution<- deSolve::ode(
#     y=DO_dusk, 
#     times=c(0:(10*60)), 
#     func=DO_fun,
#     parms= parms,
#     method="euler")
#   dat$DawnDO_Mod[i]<-solution[601,2]
# }
# 
# dat$DawnDO_Mod<-dat$DO_dusk-dat$DawnDO_Mod
# 
# plot(dat$DO_dawn~dat$DawnDO_Mod, main="Dawn Dissolved Oxygen (all depths)", xlab="Model DO", ylab="True DO", ylim=c(1,9), xlim=c(1,9))
# abline(0,1)
# dat1<-subset(dat, dat$depth==1)
# points(dat1$DO_dawn~dat1$DawnDO_Mod, col="blue")
# dat6<-subset(dat, dat$depth==.6)
# points(dat6$DO_dawn~dat6$DawnDO_Mod, col="green")
# dat2<-subset(dat, dat$depth==0.2)
# points(dat2$DO_dawn~dat2$DawnDO_Mod, col="red")
# legend("topleft",legend=c("1.4","1.0","0.6","0.2"),
#        col=c("black","blue","green","red"),pch=1,bg="white")
# 
####Try to predict dusk dissolved oxygen using temperature
#add in distance to shore (west to east)
# dat <- read.csv("DO-Sampling/_dat/Data/DawnDuskDO.csv")
# dat$ï..dt<-as.Date(dat$ï..dt, "%m/%d/%Y")
# dat$doy<-as.numeric(format(dat$ï..dt,"%j"))
# M1<-lm(dat$DO_dusk~dat$depth+dat$Temp_C+dat$y+dat$x)
# summary(M1)
# plot(M1)
# dat$pred<-predict(M1,dat)
# plot(y=dat$pred,x=dat$DO_dusk, xlab="True Dusk DO", ylab="Predicted Dusk DO", main="Predicting Dusk DO given depth, temp, and location")
# abline(0,1)
# 
# ####Set up Model on Small Data----
# #dat <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv") #actual bathymetry
# dat <- read.csv("DO-Sampling/Export_Output.csv") #using a small file with fewer points to check model
# boards<-c(17) #baby range for now 0-17
# elevation<-66.45402+(0.2083*boards)
# tempC<-c(20:22) #baby range for now 0-30
# DO_dusk<-c(9:10) #baby range for now 5-10
# DO_Crit<-c(5,4.5,4,3.5,3)
# combos<-expand.grid(tempC=tempC, DO_dusk=DO_dusk, elevation=elevation, DO_Crit=DO_Crit)
# combos$Vol<-NA
# 
# #make new points for volume slices
# for(i in 1:nrow(combos)){
#   dat<- subset(dat, dat$Elevation < (combos$elevation[i]))
#   cube<-c(0:5)
#   datalist <- list()
#   for(k in 1:length(cube)){
#     dat2<- subset(dat, dat$Elevation < (combos$elevation[i]-cube[k]))
#     dat2$Z <- c((combos$elevation[i]-dat$Elevation[k])) #depth (for calculating k)
#     dat2$Z2<-dat2$Z-cube[k] #depth from point to bottom (for DO equ)
#     dat2<-subset(dat2, dat2$Z2>0)
#     dat2$k<-0.0606*(dat2$Z2/dat2$Z) #depth from pt 2 bttmm/total depth
#     dat2$Z3<-ifelse(dat2$Z2>1,1,dat2$Z2) #depth of volume cube
#     datalist[[k]]<-dat2
#   }
#   big_data <- do.call(rbind, datalist)
#   DO_dusk<-combos$DO_dusk[i]
#   tempC<-combos$tempC[i]
#   big_data$DawnDO_Mod<-NA
#   for(j in 1:nrow(big_data)){
#    parms=c(tempC = tempC, Z = big_data$Z2[j], k=big_data$k[j])
#     solution<- deSolve::ode(
#       y=DO_dusk,
#       times=c(0:(10*60)),
#       func=DO_fun,
#       parms= parms,
#       method="euler")
#     big_data$DawnDO_Mod[j]<-solution[601,2] #pull last value "dawn"
#   }
#   big_data$DawnDO_Mod<-combos$DO_dusk[i]-big_data$DawnDO_Mod #calculate surface area
#   NumPts<-length(which(big_data$DawnDO_Mod > (combos$DO_Crit[i])))
#   combos$Vol[i]<-sum(4*NumPts3$Z3) 
# }
# tmp<-dcast(combos, tempC+DO_dusk+elevation~DO_Crit)
# tmp<-as.data.frame(tmp)

####Run Model on Bathymetric Map----
dat <- read.csv("Depth-Mapping/_dat/Bathymetry/CompleteMap.csv") #actual bathymetry
#create combos of elevation, starting water temp, and dusk DO to run over
boards<-c(1:17) #baby range for now 0-17
elevation<-66.45402+(0.2083*boards)
tempC<-c(5:30)
DO_dusk<-c(5:10)
combos<-expand.grid(tempC=tempC, DO_dusk=DO_dusk, elevation=elevation)
combos$Vol6.5<-NA
combos$Vol6<-NA
combos$Vol5.5<-NA
combos$Vol5<-NA
combos$Vol4.5<-NA
combos$Vol4<-NA
combos$Vol3.5<-NA 
combos$Vol3<-NA

for(i in 1:nrow(combos)){
  dat<- subset(dat, dat$POINT_Z < (combos$elevation[i]))
  cube<-c(0:5)
  datalist <- list()
  #make new points for volume slices
  for(k in 1:length(cube)){
    dat2<- subset(dat, dat$POINT_Z < (combos$elevation[i]-cube[k]))
    dat2$Z <- c((combos$elevation[i]-dat2$POINT_Z)) #depth (for calculating k)
    dat2$Z2 <- dat2$Z-cube[k] #depth from point to bottom (for DO equ)
    dat2 <- subset(dat2, dat2$Z2>0)
    dat2$k <- 0.01*(dat2$Z2/dat2$Z) #depth from pt 2 bttmm/total depth
    dat2$Z3 <- ifelse(dat2$Z2>1,1,dat2$Z2) #depth of volume cube
    datalist[[k]] <- dat2
  }
  big_data <- do.call(rbind, datalist)
  big_data<- subset(big_data, big_data$Z3>=0.15)
  DO_dusk<-combos$DO_dusk[i]
  tempC<-combos$tempC[i]
  big_data$DawnDO_Mod<-NA 
  #run DO model
  for(j in 1:nrow(big_data)){    
    parms=c(tempC = tempC, Z = big_data$Z2[j], k=big_data$k[j])
    solution<- deSolve::ode(
      y=DO_dusk, 
      times=c(0:(10*60)), 
      func=DO_fun,
      parms= parms,
      method="euler")
    big_data$DawnDO_Mod[j]<-solution[601,2] #pull last value "dawn"
  }
  big_data$DawnDO_Mod<-ifelse(is.nan(big_data$DawnDO_Mod),NA,big_data$DawnDO_Mod)
  big_data<-na.omit(big_data)
  big_data$DawnDO_Mod<-combos$DO_dusk[i]+big_data$DawnDO_Mod 
  #specify DO criteria
  NumPts6.5<-subset(big_data, big_data$DawnDO_Mod > 6.5)
  NumPts6<-subset(big_data, big_data$DawnDO_Mod > 6)
  NumPts5.5<-subset(big_data, big_data$DawnDO_Mod > 5.5)
  NumPts5<-subset(big_data, big_data$DawnDO_Mod > 5)
  NumPts4.5<-subset(big_data, big_data$DawnDO_Mod > 4.5)
  NumPts4<-subset(big_data, big_data$DawnDO_Mod > 4)
  NumPts3.5<-subset(big_data, big_data$DawnDO_Mod > 3.5)
  NumPts3<-subset(big_data, big_data$DawnDO_Mod > 3)
  #calculate volume
  combos$Vol6.5[i]<-sum(4*NumPts6.5$Z3) 
  combos$Vol6[i]<-sum(4*NumPts6$Z3) 
  combos$Vol5.5[i]<-sum(4*NumPts5.5$Z3)  
  combos$Vol5[i]<-sum(4*NumPts5$Z3)
  combos$Vol4.5[i]<-sum(4*NumPts4.5$Z3)  
  combos$Vol4[i]<-sum(4*NumPts4$Z3) 
  combos$Vol3.5[i]<-sum(4*NumPts3.5$Z3)  
  combos$Vol3[i]<-sum(4*NumPts3$Z3) 
  write.csv(combos,"_do-outputs/combos.csv")
  print(i/nrow(combos))
  print(i)
  print(nrow(combos))
}

