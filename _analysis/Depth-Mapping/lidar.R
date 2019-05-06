install.packages("rLiDAR")

library(rLiDAR)

# Reading LAS filer
rLAS<-readLAS("coverages/LIDAR-FILES/16SCB315795.las",short=TRUE)
# Summary of the LAS 
filesummary(rLAS)

# 01 Set a single color
col<-"forestgreen"
# plot 2D
plot(rLAS[,1],rLAS[,2], col=col,xlab="UTM.Easting", ylab="UTM.Northing", main="Single color")
# plot 3D
library(rgl)
points3d(rLAS[,1:3], col=col, axes=FALSE,xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))                 
# 
axesgrid3d(side=c('x+','y-','z'), col="gray") # 
gridtitle3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height(m)", col="red") # 
titleplanes3d(0, 0, -1, 0.001, col="gray", alpha=0.7)   # terrain