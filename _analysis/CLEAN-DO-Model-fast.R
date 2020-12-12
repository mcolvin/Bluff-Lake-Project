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


####Run Model on Bathymetric Map----
data <- fread("Depth-Mapping/_dat/Bathymetry/CompleteMap.csv") #actual bathymetry
data[,POINT_Z:=round(POINT_Z,2)]

#create combos of elevation, starting water temp, and dusk DO to run over
boards<-c(1:17) 
elevation<-round(66.45402+(0.2083*boards),2)
tempC<-c(5:30) #5-30
DO_dusk<-c(5:10) #5-10
combos<-expand.grid(tempC=tempC, 
    DO_dusk=DO_dusk, 
    elevation=elevation)
combos$Vol6.5<-NA
combos$Vol6<-  NA
combos$Vol5.5<-NA
combos$Vol5<-  NA
combos$Vol4.5<-NA
combos$Vol4<-  NA
combos$Vol3.5<-NA 
combos$Vol3<-  NA
ptm<-proc.time()
for(i in 1:nrow(combos))
    {
    dat<- subset(data, data$POINT_Z < (combos$elevation[i]))
    cube<-c(0:5)
    datalist <- list()
    #make new points for volume slices
    datalist<-lapply(1:length(cube),function(k)
        {
        dat2<- subset(dat, dat$POINT_Z < (combos$elevation[i]-cube[k]))
        dat2$Z <- c((combos$elevation[i]-dat2$POINT_Z)) #depth (for calculating k)
        dat2$Z2 <- dat2$Z-cube[k] #depth from point to bottom (for DO equ)
        dat3 <- subset(dat2, dat2$Z2>0)
        dat3$k <- 0.06 #depth from pt 2 bttmm/total depth *(dat3$Z2/dat3$Z)
        dat3$Z3 <- ifelse(dat3$Z2>1,1,dat3$Z2) #depth of volume cube
        return(dat3)
        })
    big_data <- rbindlist(datalist)
    big_data2<- subset(big_data, big_data$Z3>=0.1)
    DO_dusk<-combos$DO_dusk[i]
    tempC<-combos$tempC[i]
    #big_data$DawnDO_Mod<-NA 
    # round Z2 and k to 4 digits
    big_data2[,Z2:=round(Z2,1)] #maybe change to 2
    big_data2[,k:=round(k,4)]
    # tally up unique combos of Z2 and k
    tmp<-big_data2[,.(N=.N),by=.(Z2,k)]
    tmp[,DawnDO_Mod:=NA]

    #run DO model
    for(j in 1:nrow(tmp))
        {    
        parms=c(tempC = tempC, Z = tmp$Z2[j], k=tmp$k[j])
        solution<- deSolve::ode(
            y=DO_dusk, 
            times=c(0:(10*60)), 
            func=DO_fun,
            parms= parms,
            method="euler")
        tmp$DawnDO_Mod[j]<-solution[601,2] #pull last value "dawn"
        }
  
    # merge tmp with big_data
    big_data3<-merge(big_data2,tmp,by=c("Z2","k"),
        all.x=TRUE)
    big_data3$DawnDO_Mod<-ifelse(is.nan(big_data3$DawnDO_Mod),NA,big_data3$DawnDO_Mod)
    big_data4<-na.omit(big_data3)
    big_data4$DawnDO_Mod2<-combos$DO_dusk[i]-big_data4$DawnDO_Mod 
    #specify DO criteria
    NumPts6.5<-subset(big_data4, DawnDO_Mod2 > 6.5)
    NumPts6<-  subset(big_data4, DawnDO_Mod2 > 6)
    NumPts5.5<-subset(big_data4, DawnDO_Mod2 > 5.5)
    NumPts5<-  subset(big_data4, DawnDO_Mod2 > 5)
    NumPts4.5<-subset(big_data4, DawnDO_Mod2 > 4.5)
    NumPts4<-  subset(big_data4, DawnDO_Mod2 > 4)
    NumPts3.5<-subset(big_data4, DawnDO_Mod2 > 3.5)
    NumPts3<-  subset(big_data4, DawnDO_Mod2 > 3)
    #calculate volume
    combos$Vol6.5[i]<-sum(4*NumPts6.5$Z3) 
    combos$Vol6[i]<-  sum(4*NumPts6$Z3) 
    combos$Vol5.5[i]<-sum(4*NumPts5.5$Z3)  
    combos$Vol5[i]<-  sum(4*NumPts5$Z3)
    combos$Vol4.5[i]<-sum(4*NumPts4.5$Z3)  
    combos$Vol4[i]<-  sum(4*NumPts4$Z3) 
    combos$Vol3.5[i]<-sum(4*NumPts3.5$Z3)  
    combos$Vol3[i]<-  sum(4*NumPts3$Z3) 
    fwrite(combos,"_do-outputs/combos-fast.csv")
    print(i/nrow(combos))
    }
proc.time()-ptm

