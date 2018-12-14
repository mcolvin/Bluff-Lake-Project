setwd("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection")
kml.text <- readLines("C:/Users/Victoria Starnes/Documents/Bluff Lake DO site selection/Bluff Lake Draft 1.kml")
kml.text
coords <- data.frame(kml.text[51:116])
coords <- data.frame(do.call('rbind', strsplit(as.character(coords$kml.text.51.116.), ',', fixed = TRUE)))
str(coords)
coords$X1 <- as.numeric(as.character(coords$X1))
coords$X2 <- as.numeric(as.character(coords$X2))
coords$X3 <- as.numeric(as.character(coords$X3))
colnames(coords) <- c('x','y','z')
str(coords)
      
write.table(coords, "poly_coordinates.csv", sep = ",", row.names = F)

library(sp)
xy <- cbind(coords$x, coords$y)
plot(xy, type = 'l')
xy.poly <- Polygon(xy)
xy.points.reg <- spsample(xy.poly, n = 23, type = "regular") # n is sample size


CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
crs(xy.poly)



set.seed(123)
xy.points.nonal.20 <- spsample(xy.poly, n = 23, type = "nonaligned") # n is sample size
plot(xy.points.nonal.20, add = TRUE, pch = 3)

write.table(xy.points, "point_coordinates.csv", sep = ",", row.names = F) 
