#----------------------------------------------------------------------
# 
#  TRANSITION MATRICES FOR SIMULATION OUTCOMES
#  FIXED STARTING ELEVATIONS
# 
#----------------------------------------------------------------------
# ---- READ IN OUTCOMES
sims<-fread("_outputs/drawdown-outcomes-fixes.csv")
sims<-subset(sims, !is.na(wse)) # not sure why these are NAs?
wse_brks<-c(0,seq(66.6,69,,by=0.2),100)

labs<- paste0(wse_brks[-length(wse)],"-",wse_brks[-1])
#---prevent new states
labs[1]<-labs[2]
labs[length(labs)]<-labs[length(labs)-1]

sims[,end_wse_bin:=factor(cut(wse,wse_brks,labels=labs),levels=unique(labs))]
sims[,start_wse_bin:=factor(cut(starting_elevation,wse_brks,labels=labs),levels=unique(labs))]



sims<-merge(sims,
    drawdown_cycle[,.SD,.SDcols=c("drawdown_cycle_id","start_state", "next_state")],
    by="drawdown_cycle_id",all.x=TRUE)
#---transition probabilities
P<-dcast(sims, release_discharge+start_state+start_wse_bin~next_state+end_wse_bin,
    value.var="cumU",length,fill=0,
    drop=c(FALSE,FALSE))
#---transitions for each decision, drawdown period, and starting water elevation
tmat<- P[,-c(1:3)]
tmat<- tmat/rowSums(tmat)
tmat<-cbind(P[,c(1:3)],tmat)
states
dim(tmat)

#---for making transition matrices
#---number of states (elevation state x periods)
states<- length(unique(labs))*max(drawdown_cycle$drawdown_cycle_id)
indx<- matrix(c(1:states),
    nrow=states,ncol=states)
#---shift index to capture period to period transition
indx<-indx[,c(c(2:9),1)]


#---transition probability matrix for 
P<- matrix(0,nrow=states,ncol=states)

#---split up outcomes
cpt<-split(cpt,by=c("release_discharge","period"))
cpt<-lapply(cpt,function(x)
    {
    meta<-as.data.frame(x[,c(1:3)])
    outcomes<- x[,-c(1:3)]
    outcomes<- outcomes/rowSums(outcomes)
    pp<-P
    col_indx<-as.vector(indx[,unique(meta$period)])
    pp[,col_indx]<-as.matrix(outcomes)
    return(as.data.table(cbind(meta,pp)))
    })
cpt<-rbindlist(cpt) # combine up together again. 

#---make a list of transition matrices by release 
cpt<-split(cpt,by=c("WCS_strategy"))
#---meta data for each matrix
meta<- lapply(cpt,function(x)
    {
    return(x[,c(1:3)])
    })
#---transition matrices for each release strategy
P<- lapply(cpt,function(x)
    {
    return(as.matrix(x[,-c(1:3)]))
    })

 
 
 
# rewards matrix
R<-dcast(sims, release_discharge+start_state+start_wse_bin~next_state+end_wse_bin,value.var="cumU",mean,fill=0)
    
    
    
#----------------------------------------------------------------------
# 
#  random starting elevations
#
#----------------------------------------------------------------------    