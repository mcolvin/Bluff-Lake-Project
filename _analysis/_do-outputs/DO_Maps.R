library(gstat)
library(sp)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(data.table)
library(gridExtra)

setwd("C:/Users/Victoria Starnes/Documents/GitHub/Bluff-Lake-Project/_analysis")
data <- fread("Depth-Mapping/_dat/Bathymetry/CompleteMap.csv") #actual bathymetry
coordinates(data) = ~POINT_X+POINT_Y
proj4string(data) <- CRS("+proj=utm +zone=16 ellps=WGS84")
data<-spTransform(data, CRS("+proj=longlat +datum=WGS84"))
data<-data.frame(data)

vals<-big_data4
vals$combo<-paste(vals$POINT_X, vals$POINT_Y, sep = "_")
coordinates(vals) = ~POINT_X+POINT_Y
proj4string(vals) <- CRS("+proj=utm +zone=16 ellps=WGS84")
vals<-spTransform(vals, CRS("+proj=longlat +datum=WGS84"))
vals<-data.frame(vals)


vals0_1<-subset(vals, vals$Z2<1)
vals1_2<-subset(vals, vals$Z2<2&vals$Z2>=1)
vals2_<-subset(vals, vals$Z2>=2)
 



vals0_1<-vals0_1%>%dplyr::group_by(combo)%>%dplyr::summarise(X=mean(POINT_X), Y=mean(POINT_Y), DawnDO_Mod2=mean(DawnDO_Mod2))

vals1_2<-vals1_2%>%dplyr::group_by(combo)%>%dplyr::summarise(X=mean(POINT_X), Y=mean(POINT_Y), DawnDO_Mod2=mean(DawnDO_Mod2))

vals2_<-vals2_%>%dplyr::group_by(combo)%>%dplyr::summarise(X=mean(POINT_X), Y=mean(POINT_Y), DawnDO_Mod2=mean(DawnDO_Mod2))

kml.text <- readLines("_do-outputs/_Polygons_.kml")

#change seccond number based on coordinates in kml file (kml.text[51:?])
coords <- data.frame(kml.text[51:288])
coords <- data.frame(do.call('rbind', strsplit(as.character(coords$kml.text.51.288.), ',', fixed = TRUE)))
str(coords)
coords$X1 <- as.numeric(as.character(coords$X1))
coords$X2 <- as.numeric(as.character(coords$X2))
coords$X3 <- as.numeric(as.character(coords$X3))
colnames(coords) <- c('x','y','z')

A<-ggplot() +geom_point(data=vals0_1, aes(x=X, y=Y, color=DawnDO_Mod2))+theme_classic()+
  labs(color = "Dissolved Oxygen")+
  scale_color_gradient(limits = c(0, 11.5))+geom_path(aes(x=coords$x, y=coords$y), size=1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        text = element_text(size=8),legend.position = "none")+
  annotate(geom="text", x=-88.807, y=33.297, size=3,label="A")
B<-ggplot() +geom_point(data=vals1_2, aes(x=X, y=Y, color=DawnDO_Mod2))+theme_classic()+
  labs(color = "Dissolved Oxygen")+
  scale_color_gradient(limits = c(0, 11.5))+geom_path(aes(x=coords$x, y=coords$y), size=1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        text = element_text(size=8),legend.position = "none")+
  annotate(geom="text", x=-88.807, y=33.297, size=3,label="B")
C<-ggplot() +geom_point(data=vals2_, aes(x=X, y=Y, color=DawnDO_Mod2))+theme_classic()+
  labs(color = "Dissolved Oxygen")+
  scale_color_gradient(limits = c(0, 11.5))+geom_path(aes(x=coords$x, y=coords$y), size=1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        text = element_text(size=8))+
  annotate(geom="text", x=-88.807, y=33.297,size=3,label="C")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend<-get_legend(C)

C<-ggplot() +geom_point(data=vals2_, aes(x=X, y=Y, color=DawnDO_Mod2))+theme_classic()+
  labs(color = "Dissolved Oxygen")+
  scale_color_gradient(limits = c(0, 11.5))+geom_path(aes(x=coords$x, y=coords$y), size=1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        text = element_text(size=8), legend.position = "none")+
  annotate(geom="text", x=-88.807, y=33.297,size=3,label="C")


grid.arrange(A, B, C, legend, ncol=2,
             bottom="X", left="Y")


data$POINT_Z<-(data$POINT_Z-68.33)

ggplot() +geom_point(data=data, aes(x=POINT_X, y=POINT_Y, color=POINT_Z))+theme_classic()+
  scale_color_gradient(limits = c(-4,0))

