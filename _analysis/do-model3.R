#Creating a function for DO from dawn to dusk
DO_dusk<-10
parameters <- c(
  tempC = 25, #Same as SOU above just SOU/min 
  Z = 2) # depth
DO_fun<-function(t,x,parms)
{
  DO<-x
  tempC<-parms["tempC"]
  Z<-parms["Z"]
  WR<-0.07203515/60 #lake average water respiration in mg/L/min
  # DO Saturation at a given temperature        
  DOsat<-4.09+(10.5*exp(-0.0371*tempC)) 
  SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
  # diffusive exchange
  DE<-(DO*DOsat)*(2.2*10^-5)*(0.06^-1)*
    60*10^-2 
  # below is eq 2. but i am not sure the do term should be ther
  #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
  # I think this is the right ode
  dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
  return(list(dDO))
}
solution<- deSolve::ode(
  y=DO_dusk, 
  times=c(0:(10*60)), 
  func=DO_fun, 
  parms=parameters, 
  method="euler")
plot(solution,ylab="DO mg/L",xlab="Time in minutes",las=1,main="")
tail(solution, n=1)
solution[601,2]







#Creating a function for DO from dawn to dusk
DO_fun<-function(t,x,parms)
    {
    DO<-x
    tempC<-parms["tempC"]
    Z<-parms["Z"]
    WR<-0.07203515/60 #lake average water respiration in mg/L/min
    # DO Saturation at a given temperature        
    DOsat<-4.09+(10.5*exp(-0.0371*tempC)) 
    SR<- ((((0.287*tempC-2.5)*44.661)*0.7)*0.001)/60
    # diffusive exchange
    DE<-(DO*DOsat)*(2.2*10^-5)*(0.06^-1)*
        60*10^-2 
    # below is eq 2. but i am not sure the do term should be ther
    #dDO<- DO - SR*Z^-1 + WR +DE*Z^-1
    # I think this is the right ode
    dDO<- -1*(SR*Z^-1+WR+DE*Z^-1)
    return(list(dDO))
}


##run function for map
#Bring in depths
dat <- read.csv("DO-Sampling/Export_Output.csv")
dat$Z<- 68.39712-dat$Elevation #9 boards in the WCS
dat$Z<-ifelse(dat$Z<=0, 0, dat$Z) #remove dry areas
dat$tempC<-dat$Temp
dat$DO_dusk<-dat$DO

dat$DawnDO<-NA 
for (i in 1:length(dat))
{
  DO_dusk<-dat$DO_dusk[i]
  parms=c(tempC = dat$tempC[i], Z = dat$Z[i])
  solution<- deSolve::ode(
    y=DO_dusk, 
    times=c(0:(10*60)), 
    func=DO_fun,
    parms= parms,
    method="euler")
  dat$DawnDO[i]<-solution[601,2]
}




dat$Longitude<-dat$POINT_X
dat$Latitude<-dat$POINT_Y
coordinates(dat) = ~Longitude+Latitude
proj4string(dat) <- CRS("+proj=utm +zone=16 ellps=WGS84")

# Grid
dat2 <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat2$Longitude<-dat2$ï..POINT_X
dat2$Latitude<-dat2$POINT_Y
coordinates(dat2) = ~Longitude+Latitude
proj4string(dat2) <- CRS("+proj=utm +zone=16 ellps=WGS84")
## GET THE RANGE FOR X AND Y
xrange <- range(dat2@coords[,1])
xrange <- c(xrange[1]-200, xrange[2]+0)
yrange <- range(dat2@coords[,2])
yrange <- c(yrange[1]-0, yrange[2]+200)
## MAKE A GRID TO INTERPOLATE 15X15 METER GRID
grd <- expand.grid(x=seq(from=xrange[1],
                         to=xrange[2], by=15),
                   y=seq(from=yrange[1], to=yrange[2], by=15))
## CONVERT THE GRID TO A SPATIAL PIXEL CLASS
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <-CRS("+proj=utm +zone=16 ellps=WGS84")


## INTERPOLATE THE SURFACE BY INVERSE DISTANCE WEIGHTING
xx<-idw(formula= DawnDO ~ 1,
        locations=dat, newdata=grd)

## SAVE THE OUTPUT OF THE INTERPOLATION AS A DATA.FRAME
xxoutput=as.data.frame(xx)[,-4]# DROP VARIANCE COLUMN
names(xxoutput)[1:3]<-c("long","lat","DO")

## ASSIGN COORDINATES TO THE INTERPOLOATIONS AND SPATIAL GRID
coordinates(xxoutput) = ~long+lat
gridded(xxoutput)<-TRUE

## import the vector boundary
Polygons <- readOGR(dsn=path.expand("DO-Sampling/_dat/BluffLakeOutline/Polygons.shp"),layer="Polygons")
## PROJECT TO UTM
Polygons<-spTransform(Polygons, CRS("+proj=utm +zone=16 ellps=WGS84"))

#plot imported shapefile
plot(Polygons,
     main = "Shapefile imported into R - crop extent",
     axes = TRUE,
     border = "blue")
BluffCrop<-crop(xxoutput, Polygons)

BluffCrop = as.data.frame(BluffCrop)
write.csv(BluffCrop, "DO-Sampling/_dat/ModeledDawn9B.csv")
## PLOT THE INTERPOLATED GRID
ggplot(aes(x = long, y = lat), data = BluffCrop) + geom_tile(aes(fill = DO))+
  theme_classic()+  scale_fill_gradient(low = "black", high = "grey99", limits = c(0,11))+
  xlab("Longitude")+ylab("Latitude")+theme(legend.title=element_blank())

ggplot(aes(x = POINT_X, y = POINT_Y), data = dat) + geom_tile(aes(fill = dat$DO))+
  theme_classic()+  scale_fill_gradient(low = "black", high = "grey99", limits = c(0,11))+
  xlab("Longitude")+ylab("Latitude")+theme(legend.title=element_blank())
