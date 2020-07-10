library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

#Water Respiration----
dat<-read.csv("DO-Sampling/_dat/Data/WC_Respiration_June15_16_2020.csv")
Dark<-subset(dat, ï..SampleType=="Dark")
Dark$StartTime<-as.POSIXct(Dark$StartTime, format="%m/%d/%Y %H:%M")
Dark$EndTime<-as.POSIXct(Dark$EndTime, format="%m/%d/%Y %H:%M")
Dark$DiffTime<-Dark$EndTime-Dark$StartTime
Dark$DiffTime<-as.numeric(Dark$DiffTime)
Dark$WCresp<-(Dark$InitialDO-Dark$EndDO)/Dark$DiffTime
EndDO<-Dark%>%dplyr::group_by(as.factor(Samplelocation), DepthofCollection)%>%dplyr::summarize(EndDO=mean(EndDO), StartDO=mean(InitialDO),DiffTime=mean(DiffTime))
EndDO$DiffTime<-as.numeric(EndDO$DiffTime)
EndDO$WCresp<-(EndDO$StartDO-EndDO$EndDO)/EndDO$DiffTime


WCresp<-0.07203515 #lake average
tempC<-25
#Sediment Respiration----
SOU<-(((0.287*tempC-2.5)*44.661)*.7)*0.001
dat2 <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat2$depth<- 66.52752-dat2$POINT_Z
dat2$depth<-ifelse(dat2$depth<=0, 0, dat2$depth)
DOdusk<-10
Lengthnight<-10
#DO Model----
#dat2$DOdawn<-DOdusk-(Lengthnight*((SOU/dat2$depth)+WCresp))
#dat2$DOdawn<-ifelse(dat2$DOdawn<=0, 0, dat2$DOdawn)

#DO Model 2----
dat2 <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
WCresp<-0.07203515/60 #lake average
tempC<-25
SOU<-((((0.287*tempC-2.5)*44.661)*.7)*0.001)/60
DOdusk<-10
Lengthnight<-10*60
DOsat<-4.09+(10.5*exp(-0.0371*25))
DE<-(DOdusk*DOsat)*(2.2*10^-5)*(0.03^-1)*60*10^-2
# model of DO dynamics. Equation 1 in Miranda et al 2001
dat2$Dawn<-DOdusk-(((SOU*dat2$depth^-1)+WCresp+(DE*dat2$depth^-1))*Lengthnight)
dat2$DOdawn<-ifelse(dat2$DOdawn<=0, 0, dat2$DOdawn)


library(gstat)
library(sp)
## ASSIGN LONGITUED AND LATITUDE AS X AND Y
## COORINDATES
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

BluffCrop = as.data.frame(BluffCrop)
write.csv(BluffCrop, "DO-Sampling/_dat/ModeledDO/MirandaModel/0BDOmap.csv")


## PLOT THE INTERPOLATED GRID
BluffCrop<-read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/6BDOmap.csv")
ggplot(aes(x = long, y = lat), data = BluffCrop) + geom_tile(aes(fill = DO))+
  theme_classic()+  scale_fill_gradient(low = "black", high = "grey99", limits = c(0,9))+
  xlab("Longitude")+ylab("Latitude")+theme(legend.title=element_blank())


#plot(BluffCrop["DO"])


# #Actual DO----
# dat3<-read.csv("DO-Sampling/_dat/Data/June13_14_2020profile.csv")
# S1<-dat3%>%group_by(Site, ï..WP)%>%summarise(mean(DO.2..mg.L.))
# S2<-dat3%>%group_by(Site, ï..WP)%>%summarise(mean(na.omit(DO.2..mg.L..1)))
# S1-S2
# dat4<-read.csv("DO-Sampling/_dat/Data/DO_Map_June13_14_2020.csv")
# dat4$Longitude<-dat4$x
# dat4$Latitude<-dat4$y
# coordinates(dat4) = ~Longitude+Latitude
# proj4string(dat4) <- CRS("+proj=longlat +datum=WGS84") 
# ## PROJECT TO UTM
# dat4<-spTransform(dat4, CRS("+proj=utm +zone=16 ellps=WGS84"))
# ## INTERPOLATE THE SURFACE BY INVERSE DISTANCE WEIGHTING
# xx<-idw(formula= DO.2..mg.L..1 ~ 1, 
#         locations=dat4, newdata=grd)
# 
# ## SAVE THE OUTPUT OF THE INTERPOLATION AS A DATA.FRAME
# xxoutput=as.data.frame(xx)[,-4]# DROP VARIANCE COLUMN
# names(xxoutput)[1:3]<-c("long","lat","DO")
# 
# ## ASSIGN COORDINATES TO THE INTERPOLOATIONS AND SPATIAL GRID
# coordinates(xxoutput) = ~long+lat
# gridded(xxoutput)<-TRUE
# 
# ## import the vector boundary
# Polygons <- readOGR(dsn=path.expand("DO-Sampling/_dat/BluffLakeOutline/Polygons.shp"),layer="Polygons")
# ## PROJECT TO UTM
# Polygons<-spTransform(Polygons, CRS("+proj=utm +zone=16 ellps=WGS84"))
# 
# #plot imported shapefile
# plot(Polygons,
#      main = "Shapefile imported into R - crop extent",
#      axes = TRUE,
#      border = "blue")
# BluffCrop<-crop(xxoutput, Polygons)
# 
# BluffCrop = as.data.frame(BluffCrop)
#write.csv(BluffCrop, "DO-Sampling/_dat/9BDOmap.csv")
## PLOT THE INTERPOLATED GRID
BluffCrop2<-read.csv("DO-Sampling/_dat/9BDOmap.csv")
ggplot(aes(x = long, y = lat), data = BluffCrop) + geom_tile(aes(fill = DO))+
  theme_classic()+  scale_fill_gradient(low = "black", high = "grey99", limits = c(0,9))+
  xlab("Longitude")+ylab("Latitude")+theme(legend.title=element_blank())


plot(BluffCrop["DO"])


#Juvenile Fish Habitat----
Z <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/0BDOmap.csv")
Z <- sum(Z$DO > 3)*225
B <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/1BDOmap.csv")
B <- sum(B$DO > 3)*225
BB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/2BDOmap.csv")
BB <- sum(BB$DO > 3)*225
BBB<- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/3BDOmap.csv")
BBB<- sum(BBB$DO > 3)*225
BBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/4BDOmap.csv")
BBBB <- sum(BBBB$DO > 3)*225
BBBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/5BDOmap.csv")
BBBBB <- sum(BBBBB$DO > 3)*225
BBBBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/6BDOmap.csv")
BBBBBB <- sum(BBBBBB$DO > 3)*255
BBBBBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/7BDOmap.csv")
BBBBBBB <- sum(BBBBBBB$DO > 3)*255
BBBBBBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/8BDOmap.csv")
BBBBBBBB <- sum(BBBBBBBB$DO > 3)*255
BBBBBBBBB <- read.csv("DO-Sampling/_dat/ModeledDO/MirandaModel/9BDOmap.csv")
BBBBBBBBB <- sum(BBBBBBBBB$DO > 3)*255
elevation <- c(66.52752,66.73582,66.94412,67.15242,67.36072,67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
Fish <- c(Z,B,BB,BBB,BBBB,BBBBB, BBBBBB, BBBBBBB, BBBBBBBB, BBBBBBBBB)/10000
data<-data.frame(Fish, elevation)
ggplot(data, aes(elevation, Fish)) + geom_line() + 
  labs(y = "Hectares with >3mg/L dissolved oxygen", x = "Water Surface Elevation (m)")+   
  theme_classic()
data$DOConc<-c('3DO')
write.csv(data,"DO-Sampling/_dat/Tolerance/3DO.csv")

data<-read.csv("DO-Sampling/_dat/Tolerance/3DO.csv")
ggplot(data, aes(elevation, Fish)) + geom_line() + 
  labs(y = "Hectares with >3mg/L dissolved oxygen", x = "Water Surface Elevation (m)")+   
  theme_classic()+
  ylim(0, 400)
