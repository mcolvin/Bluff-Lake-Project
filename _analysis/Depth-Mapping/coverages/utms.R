

library(gstat)
library(sp)

f<-dir("2019-02-16")
dat<-read.csv(paste("2019-02-16/",f[2],sep=""))
for(i in 2:length(f))
    {
    app<-read.csv(paste("2019-02-16/",f[i],sep=""))
    dat<-rbind(dat,app)
    }

f<-dir("2019-03-02")    
for(i in 2:length(f))
    {
    app<-read.csv(paste("2019-03-02/",f[i],sep=""))
    dat<-rbind(dat,app)
    }
coordinates(dat) = ~Longitude+Latitude
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84") 

vals_utm<- spTransform(dat, CRS("+proj=utm +zone=16 ellps=WGS84"))

write.csv(vals_utm, "depth_vals_utm2.csv")    
