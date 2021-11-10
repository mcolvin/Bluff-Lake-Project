
library(data.table)
library(MDPtoolbox)
outcomes<-fread("SDP_final.csv")
names(outcomes)[26]<-"U" # rename utility to something easier
# starting elevations
starting_ele<-unique(outcomes$Board1)
period<-unique(outcomes$period)
D<-unique(outcomes$WCS_strategy)
outcomes[,EL_new:=EL]
setDT(outcomes)[EL<66.6,EL_new:=66.6] # keep new states from occurring
setDT(outcomes)[EL>70,EL_new:=70] # keep new states from occurring
# a function to map ending eles to starting ele
outcomes[,ending_ele:=cut(outcomes$EL_new,c(starting_ele,1000),
    lab=starting_ele, include.lowest = TRUE)]
# tally up outcomes to calculate the probability of each
cpt<-dcast(outcomes, WCS_strategy+period+Board1~ending_ele,value.var="ending_ele",length,
    drop=c(FALSE,FALSE))

# for making transition matrices
indx<- matrix(c(1:(length(starting_ele)*length(period))),
    nrow=length(starting_ele),ncol=length(period))
# shift index to capture period to period transition
indx<-indx[,c(c(2:9),1)]

# number of states (elevation state x periods)
states<-length(starting_ele)*length(period)
# transition probability matrix for 
P<- matrix(0,nrow=length(starting_ele),ncol=states)

# split up outcomes
cpt<-split(cpt,by=c("WCS_strategy","period"))
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

# make a list of transition matrices by release 
cpt<-split(cpt,by=c("WCS_strategy"))
# meta data for each matrix
meta<- lapply(cpt,function(x)
    {
    return(x[,c(1:3)])
    })
# transition matrices for each release strategy
P<- lapply(cpt,function(x)
    {
    return(as.matrix(x[,-c(1:3)]))
    })


# reward matrix [period:elevation, release strategy]
RR<-dcast(outcomes, WCS_strategy+period+Board1~ending_ele,value.var="U",median,
    drop=c(FALSE,FALSE))
RR[is.na(RR)]<-0



# transition probability matrix for 
#R<- matrix(0,nrow=length(starting_ele),ncol=states)
# split up outcomes
R<-split(RR,by=c("WCS_strategy","period"))
R<-lapply(R,function(x)
    {
    meta<-as.data.frame(x[,c(1:3)])
    utilities<- x[,-c(1:3)]
    pp<-matrix(0,nrow=length(starting_ele),ncol=states)
    col_indx<-as.vector(indx[,unique(meta$period)])
    pp[,col_indx]<-as.matrix(utilities)
    return(as.data.table(cbind(meta,pp)))
    })

R<-rbindlist(R) # combine up together again. 

# make a list of transition matrices by release 
R<-split(R,by=c("WCS_strategy"))
# meta data for each matrix
meta<- lapply(R,function(x)
    {
    return(x[,c(1:3)])
    })
# transition matrices for each release strategy
R<- lapply(R,function(x)
    {
    return(as.matrix(x[,-c(1:3)]))
    })
    
    indx
RR[WCS_strategy==17 & period==5 &  Board1==70,]  
tmp<-R[["17"]]  
tmp[ 96 ,]   
tmp[126,]   
      
# Check the validity of the MDP
mdp_check(P, R)
# Solve the problem
results <- mdp_value_iteration(P, R, 0.96, 0.001,max_iter=10000);
policy <- results$policy # state dependent optimal policy
# format policies for policy plot
policies<-t(matrix(policy,nrow=length(starting_ele),ncol=length(period)))

# make policy plot
library(fields)
image.plot(y=starting_ele,x=period,z=policies,las=1,
    ylab="Lake elevation (m)", xlab="Drawdown period",
    axis.args = list(at = 1:length(D), labels=D))
