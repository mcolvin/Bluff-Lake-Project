
library(data.table)
library(MDPtoolbox)
outcomes<-fread("SDP_final.csv")
names(outcomes)[26]<-"U"
# starting elevations
starting_ele<-unique(outcomes$Board1)
period<-unique(outcomes$period)
D<-unique(outcomes$WCS_strategy)
outcomes[,EL_new:=EL]
setDT(outcomes)[EL<66.6,EL_new:=66.6] # keep new states from occuring
setDT(outcomes)[EL>70,EL_new:=70] # keep new states from occuring
# a function to map ending eles to starting ele
outcomes[,ending_ele:=cut(outcomes$EL_new,c(starting_ele,1000),
    lab=starting_ele, include.lowest = TRUE)]

cpt<-dcast(outcomes, WCS_strategy+period+Board1~ending_ele,value.var="ending_ele",length,
    drop=c(FALSE,FALSE))


indx<- matrix(c(1:(length(starting_ele)*length(period))),
    nrow=length(starting_ele),ncol=length(period))
# shift index to capture period to period transition
indx<-indx[,c(c(2:9),1)]

# number of states (elevation state x periods)
states<-length(starting_ele)*length(period)
# transition probablity matrix for 
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
cpt<-rbindlist(cpt)  

cpt<-split(cpt,by=c("WCS_strategy"))

meta<- lapply(cpt,function(x)
    {
    return(x[,c(1:3)])
    })
P<- lapply(cpt,function(x)
    {
    return(as.matrix(x[,-c(1:3)]))
    })

R <- dcast(outcomes, period+Board1~WCS_strategy,value.var="U",mean)
R<-as.matrix(R[,-c(1:2)])


# Checks the validity of the MDP
mdp_check(P, R)
# Solve the reserve design problem
results <- mdp_value_iteration(P, R, 0.96, 0.001);
policy <- results$policy


# Generate the species richness matrix
M <- round(matrix(nrow = 7, ncol = 20, data = runif(7*20,0,1)))

# Generate the transition and reward matrix
PR <- mdp_example_reserve(M, 0.2)
P <- PR$P
R <- PR$R
	
# Checks the validity of the MDP
mdp_check(P, R)

# Solve the reserve design problem
results <- mdp_value_iteration(P, R, 0.96, 0.001);
policy <- results$policy

policies<-t(matrix(policy,nrow=length(starting_ele),ncol=length(period)))

image(y=starting_ele,x=period,z=policies,las=1,
    ylab="Lake elevation (m)", xlab="Drawdown period")


# Explore solution with initial state all sites available
explore_solution_reserve(numeric(7), policy, M, P, R)


















WCS_strategy
period


MinEL	EL

# drawdown
fp<-"C:/Users/mcolvin/OneDrive - Mississippi State University/share/years"
fn<-dir(fp)
fn<-fn[grep("Final",fn)]# get the finals datasets
# read in 1990 to get it setup
x<-fread(paste(fp,fn[1],sep="/"))
# split to use lapply
x<-split(x,by=c("WCS_strategy","period"))
# use lapply on split data to get ending ele
x<-lapply(x,function(i)
    {
    setorder(i, dateTime) # order to grab last ele
    # need to get utility here for all values
    # ???? function to do this?
    out<-i[nrow(i),] # ending ele, starting is Boardl
    return(out)
    })
# make list into data.table
x<-rbindlist(x)





# no drawdown
fp<-"C:/Users/mcolvin/OneDrive - Mississippi State University/share/years/yearND"
fn<-dir(fp)
# read in 1990 to get it setup
x<-fread(paste(fp,fn[1],sep="/"))
# split to use lapply
x<-split(x,by=c("WCS_strategy","period"))
# use lapply on split data to get ending ele
x<-lapply(x,function(i)
    {
    setorder(i, dateTime) # order to grab last ele
    # need to get utility here for all values
    # ???? function to do this?
    out<-i[nrow(i),] # ending ele, starting is Boardl
    return(out)
    })
# make list into data.table
x<-rbindlist(x)

