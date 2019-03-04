



area<- 800 # acres
depth<- 4 # mean depth in feet
volume<- depth*area # volume in acre feet
volume<- volume* 43559.9 # convert acre feet to cubic feet

# RELATIONSHIP OF VOLUME AND AREA
## ASSUMED FUNCTIONAL RELATIONSHIP
## MAXIMUM AREA IF VOLUME APPROACHES 0 
## AND MINIMUM AREA WHEN LAKE IS FULL
x<-seq(0,volume,length.out=250)
k<-0.0000001
lake_area<- area-(area/(1+exp(-k*(x-volume*0.5))))
plot(x,lake_area,
    xlab="Volume (cubic feet)",
    ylab="Exposed area (acres)",
    type='p',las=1)
abline(h=area,lty=2)
## RETURN THE EXPOSED LAKE BED AREA FOR 
## A GIVEN LAKE VOLUME
exposed_area<-approxfun(x,lake_area)

# WOODSTORK HABITAT
## ASSUMED FUNCTIONAL RELATIONSHIP
ws_habitat<- 12*lake_area^0.35
plot(lake_area,ws_habitat,
    xlab="Exposed lake (acres)",
    ylab="Woodstork habitat (acres)",
    type='l',las=1)    

# RETURN WOODSTORK HABITAT GIVEN LAKE AREA
ws<-approxfun(lake_area,ws_habitat)   
    
    
# DUCKS: MOIST SOIL HABITATS
## ASSUMED FUNCTIONAL RELATIONSHIP
ms_habitat<- lake_area
plot(lake_area,ms_habitat,
    xlab="Exposed lake (acres)",
    ylab="Moist soil habitat (acres)",
    type='l',las=1) 
    
ms<-approxfun(lake_area,ms_habitat)   
    
    
# PADDLEFISH RELEASES release
## 400 cubic feet/second for at least 8 hours
pfRelease<-400 # cubic feet per second
pfDuration<- 8 # hours


release<- pfRelease*60*60*pfDuration


ndays<-28
nreps<- 5000

## LAKE VOLUME
v<-matrix(0,ndays,nreps)
v[1,]<-volume

## KEEP TRACK OF WATER RELEASES
releaseDone<-matrix(0,ndays,nreps)

## EXPOSED LAKE AREA
e_area<- matrix(0,ndays,nreps)
e_area[1,]<- exposed_area(v[1,])

## WOODSTORK HABITAT AREA
ws_area<-matrix(0,ndays,nreps)
ws_area[1,]<- ws(e_area[1,])

## MOIST SOIL AREA
ms_area<-matrix(0,ndays,nreps)
ms_area[1,]<- ms(e_area[1,])

for(d in 2:28)
    {
    releaseDone[d,]<- ifelse(d%in%c(4,11,18,25) &
        v[d-1,]>release,1,0)
    v[d,]<- v[d-1,]-release*releaseDone[d,] +
        exp(rnorm(nreps,log(release*0.1),0.5))*rbinom(nreps,1,0.1)
    v[d,]<- ifelse(v[d,]> volume, volume,v[d,])
    e_area[d,]<- exposed_area(v[d,])   
    ws_area[d,]<- ws(e_area[d,]) 
    ms_area[d,]<- ms(e_area[d,]) 
    }

trans_black<- rgb(0,0,0,alpha=40,maxColorValue=255)
matplot(v,type='l',col=trans_black)
matplot(c(1:28),ms_area,type='l',col=trans_black) 
matplot(c(1:28),ws_area,type='l',col=trans_black,
    ylab="Wood stork foraging habitat",
    xlab="Day")


# OUTCOMES
## WOOD STORK FORAGING HABITAT
ws_vals<-colSums(ws_area)
hist(ws_vals)
## MOIST SOIL
ms_vals<-colSums(ms_area)   
hist(ms_vals)  

    
    
    
    
plot(v~c(1:28),type='s')
plot(ms_area~c(1:28),type='s')
plot(ws_area~c(1:28),type='s')




# PADDLEFISH OUTCOMES
## NUMBER OF PADDLEFISH RELEASES DONE
nReleases<-colSums(releaseDone)


# WOOD STORK HABITAT
## AMOUNT OF HABITAT IN ACRE DAYS








