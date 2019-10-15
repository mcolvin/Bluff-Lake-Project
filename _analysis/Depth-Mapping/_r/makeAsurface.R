library(gstat)
library(sp)
setwd("C:/Users/Victoria Starnes/Documents/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping")
## READ IN COORINDATES AND DEPTHS
vals <- read.csv("_dat/Bathymetry/CombinedCorrected.csv")

## LOOK AT ELEVATION TRACK
plot(vals$ElevationBottom,ylim=c(-4,0));abline(h=0)


## ASSIGN LONGITUED AND LATITUDE AS X AND Y 
## COORINDATES
coordinates(vals) = ~Longitude+Latitude
proj4string(vals) <- CRS("+proj=longlat +datum=WGS84") 

## PROJECT TO UTM
vals_utm<- spTransform(vals, CRS("+proj=utm +zone=16 ellps=WGS84"))

write.csv(vals_utm, "depth_vals_utm.csv")

## PLOT THE COORDINATES
plot(vals_utm,axes=TRUE)

## GET THE RANGE FOR X AND Y 
xrange <- range(vals_utm@coords[,1])
xrange <- c(xrange[1]-70, xrange[2]+60)
yrange <- range(vals_utm@coords[,2])
yrange <- c(yrange[1]-60, yrange[2]+500)

## MAKE A GRID TO INTERPOLATE 15X15 METER GRID
grd <- expand.grid(x=seq(from=xrange[1], 
    to=xrange[2], by=15), 
    y=seq(from=yrange[1], to=yrange[2], by=15))

## CONVERT THE GRID TO A SPATIAL PIXEL CLASS
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <-CRS("+proj=utm +zone=16 ellps=WGS84")

## PLOT THE GRID AND THE DEPTH COORDINATES
plot(grd, cex=1.5)
points(vals_utm, pch=1, col='red', cex=1)

## INTERPOLATE THE SURFACE BY INVERSE DISTANCE WEIGHTING
xx<-idw(formula=ElevationBottom ~ 1, 
    locations=vals_utm, newdata=grd)
    
## SAVE THE OUTPUT OF THE INTERPOLATION AS A DATA.FRAME
xxoutput=as.data.frame(xx)[,-4]# DROP VARIANCE COLUMN
names(xxoutput)[1:3]<-c("long","lat","depth")

## ASSIGN COORDINATES TO THE INTERPOLOATIONS AND SPATIAL GRID
coordinates(xxoutput) = ~long+lat
gridded(xxoutput)<-TRUE
write.csv(xxoutput, "_dat/Bathymetry/InterpolatedDepths.csv")
## PLOT THE INTERPOLATED GRID
plot(xxoutput["depth"],zlim=c(-3,0))
points(vals_utm, col='white', type='l') # PUT DOWN TRACK



