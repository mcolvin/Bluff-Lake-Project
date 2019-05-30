


library(gstat)
library(sp)

## READ IN COORINDATES AND DEPTHS
vals <- read.csv("SonarTRX/SonarTRX-R00001-20190211-091600/NavTrackProfile.csv")

## ASSIGN LONGITUED AND LATITUDE AS X AND Y 
## COORINDATES
coordinates(vals) = ~Longitude+Latitude

## PLOT THE COORDINATES
plot(vals)

## GET THE RANGE FOR X AND Y 
xrange <- range(vals@coords[,1])
yrange <- range(vals@coords[,2])

## MAKE A GRID TO INTERPOLATE 
grd <- expand.grid(x=seq(from=xrange[1], 
    to=xrange[2], by=0.000002), 
    y=seq(from=yrange[1], to=yrange[2], by=0.000002))
## CONVERT THE GRID TO A SPATIAL PIXEL CLASS
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

## PLOT THE GRID AND THE DEPTH COORDINATES
plot(grd, cex=1.5)
points(vals, pch=1, col='red', cex=1)

## INTERPOLATE THE SURFACE BY INVERSE DISTANCE WEIGHTING
xx<-idw(formula=ElevationBottom ~ 1, 
    locations=vals, newdata=grd)
    
## SAVE THE OUTPUT OF THE INTERPOLATION AS A DATA.FRAME
xxoutput=as.data.frame(xx)[,-4]# DROP VARIANCE COLUMN
names(xxoutput)[1:3]<-c("long","lat","depth")

## ASSIGN COORDINATES TO THE INTERPOLOATIONS AND SPATIAL GRID
coordinates(xxoutput) = ~long+lat
gridded(xxoutput)<-TRUE

## PLOT THE INTERPOLATED GRID
plot(xxoutput["depth"],zlim=c(-3,0))
points(vals, col='white', type='l') # PUT DOWN TRACK




  arc.write(path, data, ..., overwrite = FALSE)