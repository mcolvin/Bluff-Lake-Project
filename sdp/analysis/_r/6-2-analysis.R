#----------------------------------------------------------------------
# 
#  analysis with random starting elevations 
#
#----------------------------------------------------------------------
fp<-"_outputs/drawdowns-with-random-starting-elevations"
combos<- expand.grid(
    #---draw down cycle
    drawdown_cycle_id=drawdown_cycle$drawdown_cycle_id,
    #---decisions
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
    # rr=1
    i<-combos$drawdown_cycle_id[rr]
    starting_wse<-runif(500,66.6,69)
    #---make a dataset of discharges by year
    tmp<-subset(discharge_hourly,  
        doy %in% drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp<- dcast(tmp,doy+hour~year,
        value.var="Q_bl",mean,fill=-1)
    #---keep years with complete records of
    #---discharges to loop over
    indx<-which(apply(tmp,2,min)>-1)
    tmp <- tmp[,..indx]   
    for(yr in 3:ncol(tmp))
        {
        # yr=3
        q_intake<-predict(gam_4, 
            newdata=data.frame(Q_bl=unlist(tmp[,..yr]), doy=tmp$doy))
        q_intake<- approxfun(1:nrow(tmp),q_intake)
        # convert starting water surface elevation to a volume (units=??)
        #initial_volume<-rep(EL_2_Vol(starting_wse[j]),length(D_Q))
        initial_volume<-EL_2_Vol(starting_wse)
        parameters<-list(
            board_elevation=unlist(drawdown_cycle$board_elevation[i]),
            wcs_width=rep(1.6764,7),
            D_Q=D_Q[combos$decision[rr]]) 
        parms<-parameters
        V<-initial_volume
        #---use ode function that allows multiple decisions
        solution<- ode(
            y=initial_volume, 
            times=1:nrow(tmp), 
            func=wse_sim_decisions, 
            parms=parameters, 
            method="iteration")
        solution<-as.data.table(solution)
        solution[,doy:= tmp$doy]
        solution[,hour:= tmp$hour]
        solution[,rr:= names(tmp)[yr]]                
        solution[,year:= names(tmp)[yr]]
        solution[,drawdown_cycle_id:= combos$drawdown_cycle_id[rr]] # links back to the row of drawdown_cycle
        solution[,board_elevation:=drawdown_cycle$board_elevation[combos$drawdown_cycle_id[rr]]] # links back to the row of drawdown_cycle
        solution[,release_discharge:=D_Q[combos$decision[rr]]]                 
        fwrite(solution,combos$fp[rr],append=TRUE)
        #out<-rbind(out,solution)
        print(yr/ncol(tmp))
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
        print(x)
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
        rm(list=c("sims"))
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
        
        #---weights inside and outside growing season
        setDT(tmp)[doy<60 | doy>304, wb_w:=wghts_ngs["wb"]]
        setDT(tmp)[!(doy<60 | doy>304), wb_w:=wghts_gs["wb"]]

        setDT(tmp)[doy<60 | doy>304, wf_w:=wghts_ngs["wf"]]
        setDT(tmp)[!(doy<60 | doy>304), wf_w:=wghts_gs["wf"]]
        
        setDT(tmp)[doy<60 | doy>304, fish_w:=wghts_ngs["fish"]]
        setDT(tmp)[!(doy<60 | doy>304), fish_w:=wghts_gs["fish"]]

        setDT(tmp)[doy<60 | doy>304, angling_w:=wghts_ngs["angling"]]
        setDT(tmp)[!(doy<60 | doy>304), angling_w:=wghts_gs["angling"]]

        setDT(tmp)[doy<60 | doy>304, bank_w:=wghts_ngs["bank"]]
        setDT(tmp)[!(doy<60 | doy>304), bank_w:=wghts_gs["bank"]]

        setDT(tmp)[doy<60 | doy>304, boat_w:=wghts_ngs["boat"]]
        setDT(tmp)[!(doy<60 | doy>304), boat_w:=wghts_gs["boat"]]

        #---calculate the utility
        tmp[,U:=0]
        # ---- no value for empty lake
        setDT(tmp)[wse>66.568,U:=wb_w*wb_sc]
        setDT(tmp)[wse>66.568,U:=wb_w*wb_sc+wf_w*wf_sc+fish_w*fish_sc+angling_w*(bank_w*bank_sc+boat_w*boat_sc)]

        # weight utility within day 1/max(tmp$time) so when summed over drowdown period is 1
        tmp[,U:=U*1/max(tmp$time)]
        #---cumulative utility within drawdown period
        tmp[, cumU := cumsum(U), by ="id"]
        # get starting water surface elevation
        tmp[, starting_elevation:= .SD[1],.SDcols="wse", by="id"]
        # get last value of drawdown period
        ppp<-tmp[, .SD[c(.N)], by="id"]
        #---save processed data
        fwrite(ppp,paste0(fp,"drawdown-outcomes.csv"),append=TRUE)
        rm(list=c("tmp"))
        return(NULL)
        })
    #out<-rbindlist(out)
    #sims<-fwrite(out,paste0(fp,"drawdown-outcomes.csv"))
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

