
## Usually no need to set a 
#setwd("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection")
setwd("C:/Users/Victoria Starnes/Documents/GitHub/Bluff-Lake-Project/_analysis/DO-Sampling") # SET WORKING DIRECTORY TO THE FOLDER ABOVE THIS ONE
#kml.text <- readLines("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection/Bluff Lake Draft 1.kml")

## PULL KML DATA IN
kml.text <- readLines("_dat/Bluff Lake Sample Area.kml")

#change seccond number based on coordinates in kml file (kml.text[51:?])
coords <- data.frame(kml.text[51:115])
coords <- data.frame(do.call('rbind', strsplit(as.character(coords$kml.text.51.115.), ',', fixed = TRUE)))
str(coords)
coords$X1 <- as.numeric(as.character(coords$X1))
coords$X2 <- as.numeric(as.character(coords$X2))
coords$X3 <- as.numeric(as.character(coords$X3))
colnames(coords) <- c('x','y','z')
str(coords)

## SAVE COORDINATES TO CSV
write.table(coords, "_dat/poly_coordinates_Bluff_SampleArea.csv", sep = ",", row.names = F)

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

#making a grid
#grid <- makegrid(bl, cellsize = 0.00001) # cellsize in map units!

# grid is a data.frame. To change it to a spatial data set we have to
#grid <- SpatialPoints(grid, proj4string = CRS(proj4string(bl)))
#grid <- grid[bl]
#plot(bl)
#plot(grid, pch = 1, add = T)
#write.table(grid, "_dat/poly_grid Sampling Area.csv", sep = ",", row.names = F)



#Bluff Lake sample site locations
plot(bl_utm,axes=TRUE, main="Water Quaility Sampling Sites")
set.seed(141)
xy.points.nonal.20 <- spsample(bl_utm, n = 20, type = "nonaligned") # n is sample size
plot(xy.points.nonal.20, add = TRUE, pch = 3)
write.table(xy.points.nonal.20, "_dat/Bluff_point_coordinates.csv", sep = ",", row.names = F)






#Dorman Lake sample site locations

kml.text <- readLines("_dat/Dorman Lake.kml")

#change seccond number based on coordinates in kml file (kml.text[51:?])
dcoords <- data.frame(kml.text[51:195])                
dcoords <- data.frame(do.call('rbind', strsplit(as.character(dcoords$kml.text.51.195.), ',', fixed = TRUE)))
str(dcoords)
dcoords$X1 <- as.numeric(as.character(dcoords$X1))
dcoords$X2 <- as.numeric(as.character(dcoords$X2))
dcoords$X3 <- as.numeric(as.character(dcoords$X3))
colnames(dcoords) <- c('x','y','z')
str(dcoords)

## SAVE COORDINATES TO CSV
write.table(dcoords, "_dat/poly_coordinates_Dorman_SampleArea.csv", sep = ",", row.names = F)

library(sp)
dxy <- cbind(dcoords$x,dcoords$y)
plot(dxy, type = 'l')
dxy.poly <- Polygon(dxy)## MAKE A POLYGON
## NOW MAKE A POLYGONS OBJECT
## WE ONLY HAVE 1 POLYGON 
dor<-Polygons(list(dxy.poly), ID=1)

## HERE IS WHERE THE PROJECTION HAPPENS
dor<-SpatialPolygons(list(dor),
                      proj4string=CRS("+proj=longlat +datum=NAD83"))
plot(dor)     # THAT LOOKS MUCH BETTER
## NOW TRANSFORM TO UTM
## I PERSONALLY LIKE UTM B/C THE GRID IS 1 METER
dor_utm<-spTransform(dor, 
                    CRS("+proj=utm +zone=16 +datum=NAD83"))
plot(dor_utm,axes=TRUE, main="Water Quaility Sampling Sites")
set.seed(23)
dor.xy.points.nonal.20 <- spsample(dor_utm, n = 20, type = "nonaligned") # n is sample size
plot(dor.xy.points.nonal.20, add = TRUE, pch = 3)
write.table(dor.xy.points.nonal.20, "_dat/Dorman_point_coordinates.csv", sep = ",", row.names = F) 


###making plots
par(mfrow=c(1,2))
plot(bl_utm,axes=TRUE, main="Bluff Lake", pch=7)
plot(xy.points.nonal.20, add = TRUE, pch = 3)
plot(dor_utm,axes=TRUE, main="Dorman Lake")
plot(dor.xy.points.nonal.20, add = TRUE, pch = 3)
