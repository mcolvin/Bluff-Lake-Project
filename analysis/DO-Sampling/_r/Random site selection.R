
## Usually no need to set a 
#setwd("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection")
setwd("..") # SET WORKING DIRECTORY TO THE FOLDER ABOVE THIS ONE 
#kml.text <- readLines("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection/Bluff Lake Draft 1.kml")

## PULL KML DATA IN
kml.text <- readLines("_dat/Bluff Lake Draft 1.kml")
kml.text
coords <- data.frame(kml.text[51:116])
coords <- data.frame(do.call('rbind', strsplit(as.character(coords$kml.text.51.116.), ',', fixed = TRUE)))
str(coords)
coords$X1 <- as.numeric(as.character(coords$X1))
coords$X2 <- as.numeric(as.character(coords$X2))
coords$X3 <- as.numeric(as.character(coords$X3))
colnames(coords) <- c('x','y','z')
str(coords)

## SAVE COORDINATES TO CSV
write.table(coords, "_dat/poly_coordinates.csv", sep = ",", row.names = F)

library(sp)
xy <- cbind(coords$x, coords$y)
plot(xy, type = 'l')
xy.poly <- Polygon(xy)## MAKE A POLYGON
## NOW MAKE A POLYGONS OBJECT
## WE ONLY HAVE 1 POLYGON 
bl<- Polygons(list(xy.poly), ID=1)


## HERE IS WHERE THE PROJECTION HAPPENS
bl <- SpatialPolygons(list(bl),
	proj4string=CRS("+proj=longlat +datum=NAD83"))

plot(bl) ## THAT LOOKS MUCH BETTER

## NOW TRANSFORM TO UTM
## I PERSONALLY LIKE UTM B/C THE GRID IS 1 METER
bl_utm<-spTransform(bl, 
    CRS("+proj=utm +zone=16 +datum=NAD83"))

xy.points.reg <- spsample(bl_utm, n = 23, type = "regular") # n is sample size
plot(bl_utm,axes=TRUE)
points(xy.points.reg)


set.seed(123)
xy.points.nonal.20 <- spsample(xy.poly, n = 23, type = "nonaligned") # n is sample size
plot(xy.points.nonal.20, add = TRUE, pch = 3)

write.table(xy.points, "point_coordinates.csv", sep = ",", row.names = F) 
