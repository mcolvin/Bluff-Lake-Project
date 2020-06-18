#Water Respiration----
dat<-read.csv("DO-Sampling/_dat/WC_Respiration_June15_16_2020.csv")
Dark<-subset(dat, ï..SampleType=="Dark")
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
Dark%>%group_by(Samplelocation)%>%summarize(mean(EndDO))
Dark$StartTime<-as.POSIXct(Dark$StartTime, format="%m/%d/%Y %H:%M")
Dark$EndTime<-as.POSIXct(Dark$EndTime, format="%m/%d/%Y %H:%M")
Dark$EndTime-Dark$StartTime
S1<-(9.66-7.56)/22.5
S2<-(7.90-6.37)/22.75
WCresp<-(S1+S2)/2
tempC<-31
SOU<-(((0.287*tempC-2.5)*44.661)*.7)*0.001
dat2 <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat2$depth<- 67.77732-dat2$POINT_Z
dat2$depth<-ifelse(dat2$depth<=0, 0, dat2$depth)
DOdusk<-10
Lengthnight<-10
dat2$Longitude<-dat2$ï..POINT_X
dat2$Latitude<-dat2$POINT_Y

dat2$DOdawn<-DOdusk-(Lengthnight*((SOU/dat2$depth)+WCresp))
dat2$DOdawn<-ifelse(dat2$DOdawn<=0, 0, dat2$DOdawn)

library(gstat)
library(sp)
## ASSIGN LONGITUED AND LATITUDE AS X AND Y 
## COORINDATES
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
xx<-idw(formula=DOdawn ~ 1, 
        locations=dat2, newdata=grd)

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

write.csv(BluffCrop, "DO-Sampling/_dat/6BDOmap.csv")
## PLOT THE INTERPOLATED GRID
BluffCrop = as.data.frame(BluffCrop)

ggplot(aes(x = long, y = lat), data = BluffCrop) + geom_tile(aes(fill = DO))+
  scale_color_grey()+theme_classic()



plot(BluffCrop["DO"])








  