#----------------------------------------------------------------------
# 
#  analysis with fixed starting elevations
#
#----------------------------------------------------------------------
fp<-"_outputs/drawdowns-with-fixed-starting-elevations"
done<-dir(fp)
done<-sapply(done,function(x){
    tmp<-unlist(strsplit(x,"-"))[4]
    tmp<-as.numeric(unlist(strsplit(tmp,".",fixed=TRUE))[1])
    return(tmp)
    })
toDo<- 1:nrow(drawdown_cycle)
toDo<- toDo[!(toDo%in%done)]
for(i in toDo)
    {
    out<-data.table()   
    duration<-  drawdown_cycle$end_doy[i]-drawdown_cycle$start_doy[i]
    start_elevation<- drawdown_cycle$start_elevation[i]
    starting_wse<-sort(c(start_elevation,seq(66.6,69,by=0.2)))
    # make a dataset of discharges by year
    tmp<-subset(discharge_hourly,  doy %in% drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp<- dcast(tmp,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    # shift 3 days
    ddd<-3+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp2<-subset(discharge_hourly,  doy %in% ddd)
    tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    tmp<-cbind(tmp,tmp2[,-c(1:2)])
    # shift 6 days
    ddd<-6+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp2<-subset(discharge_hourly,  doy %in% ddd)
    tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    tmp<-cbind(tmp,tmp2[,-c(1:2)])
    # shift 9 days
    ddd<-9+c(drawdown_cycle$start_doy[i]:drawdown_cycle$end_doy[i])
    tmp2<-subset(discharge_hourly,  doy %in% ddd)
    tmp2<- dcast(tmp2,doy+hour~year,value.var="Q_bl",mean,fill=-1)
    tmp<-cbind(tmp,tmp2[,-c(1:2)])       
    # keep years with complete records
    # discharges to loop over
    indx<-which(apply(tmp,2,min)>-1)
    tmp <- tmp[,..indx]   
        
    for(d in D_Q) # decisions
        {
        #d<-1        
        for(year in 3:ncol(tmp))
            {
            #year=3
            q_intake<-predict(gam_4, 
                newdata=data.frame(Q_bl=unlist(tmp[,..year]), doy=tmp$doy))
            q_intake<- approxfun(1:nrow(tmp),q_intake)
            # LOOP OVER POSSIBLE WSE
            for(j in 1:length(starting_wse))
                {
                #j=1
                # convert starting water surface elevation to a volume (units=??)
                initial_volume<-EL_2_Vol(starting_wse[j])
                parameters<-list(
                    board_elevation=unlist(drawdown_cycle$board_elevation[i]),
                    wcs_width=rep(1.6764,7),
                    D_Q=d) # decision: water released
                parms<-parameters
                V<-initial_volume
                solution<- ode(
                    y=initial_volume, 
                    times=1:nrow(tmp), 
                    func=wse_dyn, 
                    parms=parameters, 
                    method="iteration")
                colnames(solution)[2] <- "V"
                solution<-as.data.table(solution)
                solution[,starting_elevation:=starting_wse[j]]
                solution[,doy:= tmp$doy]
                solution[,hour:= tmp$hour]
                solution[,rr:= year-2]                
                solution[,year:= names(tmp)[year]]
                solution[,drawdown_cycle_id:= i] # links back to the row of drawdown_cycle
                solution[,board_elevation:=drawdown_cycle$board_elevation[i]] # links back to the row of drawdown_cycle
                solution[,release_discharge:=d]                 
                out<-rbind(out,solution)
                } #j
            }#year
        print(d)
        }#d
    print(i)  
    fwrite(out,paste0(fp,"/simulations-with-drawdowns-",i,".csv"))
    }#i


library(lattice)
out<-fread(,paste0(fp,"/simulations-with-drawdowns-1.csv"))
out<-fread(,paste0(fp,"/simulations-with-drawdowns-2.csv"))
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
    fn<-dir(fp)# x<-fn[12]
    out<-lapply(fn,function(x)
        {
        tmp<- fread(paste(fp,x,sep="/"))
        tmp[,id:=.GRP,by=.(starting_elevation,rr,year,drawdown_cycle_id,release_discharge)]
        
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
        tmp[, cumU := cumsum(U), by =id]
        # get last value of drawdown period
        ppp<-tmp[, .SD[c(.N)], by=id]
        return(ppp)
        })
    out<-rbindlist(out)
    sims<-fwrite(out,"_outputs/drawdown-outcomes-fixes.csv")
    }




# Utilities

if(2==3)
    {
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

}