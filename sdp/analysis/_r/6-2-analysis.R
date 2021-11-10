#----------------------------------------------------------------------
# 
#  analysis with random starting elevations 
#
#----------------------------------------------------------------------
fp<-"_outputs/drawdowns-with-random-starting-elevations"
combos<- expand.grid(drawdown_cycle_id=drawdown_cycle$drawdown_cycle_id,
    decision=c(1:length(D_Q)))
combos$fn<- paste0("simulations-with-drawdowns-drawdown_cycle_id-",
    combos$drawdown_cycle_id,
    "decision-",
    combos$decision,
    ".csv")
combos$fp<- paste0(fp,"/",combos$fn)
done<-dir(fp)
combos<- combos[!(combos$fn%in%done),]
write.csv(combos,"combos_left.csv")
combos<-read.csv("combos_left.csv")
for(rr in 1:nrow(combos))
    {
    #out<-data.table()   
    i<-combos$drawdown_cycle_id[rr]
    start_elevation<- drawdown_cycle$start_elevation[i]
    starting_wse<-seq(66.6,69,by=0.2) # bins to start
    # select 50 values between each bin
    starting_wse<-unlist(lapply(seq_len(length(starting_wse)-1),function(x)
        {
        runif(10,starting_wse[x],starting_wse[x+1])
        }))
    starring_wse<-c(start_elevation,starting_wse) # make sure to include that lake is right at the board
    # make a dataset of discharges by year
    tmp<-subset(discharge_hourly,  doy %in% drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp<- dcast(tmp,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    
    # shift 3 days
    #ddd<-3+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    #tmp2<-subset(discharge_hourly,  doy %in% ddd)
   # tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    #tmp<-cbind(tmp,tmp2[,-c(1:2)])
    # shift 6 days
   # ddd<-6+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
   # tmp2<-subset(discharge_hourly,  doy %in% ddd)
   # tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
   # tmp<-cbind(tmp,tmp2[,-c(1:2)])
    # shift 9 days
   # ddd<-9+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
  #  tmp2<-subset(discharge_hourly,  doy %in% ddd)
   # tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
  #  tmp<-cbind(tmp,tmp2[,-c(1:2)])       
    # keep years with complete records
    # discharges to loop over
    indx<-which(apply(tmp,2,min)>-1)
    tmp <- tmp[,..indx]   
    for(year in 3:ncol(tmp))
        {
        #year=3
        q_intake<-predict(gam_4, 
            newdata=data.frame(Q_bl=unlist(tmp[,..year]), doy=tmp$doy))
        q_intake<- approxfun(1:nrow(tmp),q_intake)
        # LOOP OVER DECISIONS
        for(d in 1:length(D_Q))
            {
            #d=1
            # convert starting water surface elevation to a volume (units=??)
            #initial_volume<-rep(EL_2_Vol(starting_wse[j]),length(D_Q))
            initial_volume<-EL_2_Vol(starting_wse)
            
            parameters<-list(
                board_elevation=unlist(drawdown_cycle$board_elevation[i]),
                wcs_width=rep(1.6764,7),
                D_Q=D_Q[d]) 
            parms<-parameters
            V<-initial_volume
            solution<- ode(
                y=initial_volume, 
                times=1:nrow(tmp), 
                func=wse_dyn_D, 
                parms=parameters, 
                method="iteration")
            colnames(solution)[2:8] <- paste0("V-d-",c(1:length(D_Q)))
            solution<-as.data.table(solution)
            solution[,doy:= tmp$doy]
            solution[,hour:= tmp$hour]
            solution[,rr:= year-2]                
            solution[,year:= names(tmp)[year]]
            solution[,drawdown_cycle_id:= combos$drawdown_cycle_id[rr]] # links back to the row of drawdown_cycle
            solution[,board_elevation:=drawdown_cycle$board_elevation[combos$drawdown_cycle_id[rr]]] # links back to the row of drawdown_cycle
            solution[,release_discharge:=D_Q[d]]                 
            fwrite(solution,combos$fp[rr],append=TRUE)
            #out<-rbind(out,solution)
            } #D
        print(year/ncol(tmp))
        }#year
    #fwrite(out,combos$fp[rr])
    }#rr

plot(releases(1:50))
library(lattice)

matplot(solution[,c(2:601)],type='l')

xyplot(ele_lake~time|starting_elevation,out,
    group=rr,type='l',subset=release_discharge==61200& starting_elevation==69)

library(lattice)
out<-fread(paste0(fp,"/simulations-with-drawdowns-1.csv"))
setorder(out,year,rr,starting_elevation,release_discharge,time)
xyplot(ele_lake~time|starting_elevation,out,
    group=rr,type='l',subset=release_discharge==61200& starting_elevation==69)

#----------------------------------------------------------------------
# 
#  process simulation outputs
#
#----------------------------------------------------------------------
if(!exists("sims"))
    {
    # drawdowns
    fn<-dir(fp)
    out<-lapply(fn,function(x)
        {
        sims<- fread(paste(fp,x,sep="/"))
        # reshape wide to long
        indx_ele<- names(sims)[grep("ele_lake",names(sims))]
        indx_intake<-  names(sims)[grep("intake_in",names(sims))]
        indx_eof<-  names(sims)[grep("EOF_out",names(sims))]
        indx_wcs<-  names(sims)[grep("wcs_out",names(sims))]
        base<-c("release_water","doy","hour","rr","year","drawdown_cycle_id","board_elevation","release_discharge")
        tmp<-lapply(1:length(indx_ele),function(x)
            {
            out<-sims[,.SD,.SDcols=c(base, indx_ele[x],indx_intake[x],indx_eof[x],indx_wcs[x])]
            names(out)<- c(base,"wse","intake_in","EOF_out","wcs_out") # update column names
            out[,rr_id:=x] 
            out[,time:=.I]
            return(out)         
            })
        tmp<-rbindlist(tmp)
        tmp[,id:=.GRP,by=.(rr,rr_id)]
        
        tmp[,wb:=WBM(wse)]
        tmp[,wf:=WFM(wse)]
        tmp[,fish:=FishM(wse)]
        tmp[,bank:=AnglersM(wse)]
        tmp[,boat:=RampM(wse)]
        # scale metrics from 0 to 1 within drawdown period
        tmp[,wb_sc:=rescale(wb, c(0,1))]
        tmp[,wf_sc:=rescale(wf, c(0,1))]
        tmp[,fish_sc:=rescale(fish, c(0,1))]
        tmp[,bank_sc:=rescale(bank, c(0,1))]
        tmp[,boat_sc:=rescale(boat, c(0,1))]
        # constraints
        ## Mississippi Growing Season for ded
        setDT(tmp)[doy<60 | doy>304,wf_sc:=0]
        ## boating season set to 0 outside
        setDT(tmp)[doy<60 | doy>304,boat_sc:=0]
        
        # ---- weighted utility
        ## growing & boating season
        wghts_gs<- c(wb=0.27,wf=0.3,fish=0.23,angling=0.2,bank=0.5,boat=0.5)
        ## outside growing and boating season
        wghts_ngs<- c(wb=0.3857143,wf=0,fish=0.3285714,angling=0.2857143,bank=1,boat=0)
        tmp[,wb_w:=ifelse(doy<60 | doy>304,wghts_ngs["wb"],wghts_gs["wb"])]
        tmp[,wf_w:=ifelse(doy<60 | doy>304,wghts_ngs["wf"],wghts_gs["wf"])]
        tmp[,fish_w:=ifelse(doy<60 | doy>304,wghts_ngs["fish"],wghts_gs["fish"])]
        tmp[,angling_w:=ifelse(doy<60 | doy>304,wghts_ngs["angling"],wghts_gs["angling"])]
        tmp[,bank_w:=ifelse(doy<60 | doy>304,wghts_ngs["bank"],wghts_gs["bank"])]
        tmp[,boat_w:=ifelse(doy<60 | doy>304,wghts_ngs["boat"],wghts_gs["boat"])]
        # calculate the utility
        tmp[,U:=wb_w*wb_sc+wf_w*wf_sc+fish_w*fish_sc+angling_w*(bank_w*bank_sc+boat_w*boat_sc)]
        # ---- no value for empty lake
        setDT(tmp)[wse<66.568, U:=0] 
        # weight utility within day 1/max(tmp$time) so when summed over drowdown period is 1
        tmp[,U:=U*1/max(tmp$time)]
        # cumulative utility within drawdown period
        tmp[, cumU := cumsum(U), by ="id"]
        # get starting water surface elevation
        tmp[, starting_elevation:= .SD[1],.SDcols="wse", by="id"]
        # get last value of drawdown period
        ppp<-tmp[, .SD[c(.N)], by="id"]
        return(ppp)
        })
    out<-rbindlist(out)
    sims<-fwrite(out,paste0(fp,"drawdown-outcomes.csv"))
    }



# Utilities


setwd("~/GitHub/Bluff-Lake-Project/_analysis/bluff-lake-model")
All_Years<-read.csv("_dat/All_Years_Discharge_Drawdown_Sims.csv")
All_Years$elevation<-All_Years$EL
All_Years$WB<-WBM(All_Years$elevation)
All_Years<-All_Years%>%group_by(year, period, PfD)%>%
  mutate(Avg15days=rollmax(elevation, k=336, fill=EL))
All_Years$WF<-WFM(All_Years$Avg15days)
All_Years$Fish<-FishM(All_Years$elevation)
All_Years$Anglers<-AnglersM(All_Years$elevation)
All_Years$Ramp<-RampM(All_Years$elevation)


#Seasonal Weights
All_Years$DOY<-All_Years$doy
All_Years<-merge(All_Years, dates, by="DOY")

All_Years$WB<-All_Years$WF*All_Years$WF_S
All_Years$WF<-All_Years$WB*All_Years$WB_S
All_Years$Ramp<-All_Years$Ramp*All_Years$BoatS
All_Years$Anglers<-All_Years$Anglers*All_Years$BankS

#Weight Utilities
W<- c(.20,.23,.27,.3)
All_Years$Utility<-(W[1]*((All_Years$Ramp*.5) + (All_Years$Anglers*.5))) + 
  (W[2]*All_Years$Fish) + (W[3]*All_Years$WB) + (W[4]*All_Years$WF)

#no utility for empty lake
#All_Years$Utility<-ifelse(All_Years$elevation<=66.568, 0, All_Years$Utility)




sims<-fread("_outputs/drawdown-sims.csv")

# day within period
sims<-split(sims,by=c("period"))
sims<-lapply(sims,function(x)
    {
    x[,day_in_period:=.GRP,by=doy]
    })
sims<-rbindlist(sims)

