


I have no problem reading in the file using rgdal 1.2-3 and raster 2.5-8:

require(raster)
require(rgdal)   
#test <- rgdal('noxubee10.e00')
test <- raster('noxubee10.e00')
test(x) <- "+proj=utm +zone=48 +datum=WGS84"
plot(test)
?writeRaster

     # write to an integer binary file
rf <- writeRaster(test, filename="allint.ascii’", datatype='ascii’', overwrite=TRUE)

## CLIP THE RASTER
r <-  raster(m[ , 'soil'])
r <- crop(r, extent(c(180000, 181560, 331000, 333760)) 
