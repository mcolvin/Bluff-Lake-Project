#----------------------------------------------------------------------
# 
#  Weir equation function 
#  https://www.engineeringtoolbox.com/weirs-flow-rate-d_592.html
#----------------------------------------------------------------------
weir<-function(g=NULL,w=NULL,h=NULL)
    {
    Q<-(2/3)*0.66*(2*g)^(0.5)*w*h^(3/2)
    return(Q)
    }
#----------------------------------------------------------------------
# 
#  BROAD CRESTED WEIR
#
#----------------------------------------------------------------------
broad_weir<-function(g=NULL,w=NULL,h=NULL)
    {
    C=2.7 #Tracy 1957
    w_ft<- w*3.281 # convert meters to feet
    h_ft <- h*3.281# convert meters to feet
    Q<-(C*w_ft*h_ft^(3/2)) # discharge in cfs
    Q<- Q*0.0283 # convert to cms
    return(Q)
    }

# Function for turning water on/off ----
# WSE in meters above sea level
# discharge in cubic meters per second
In_out_el<-function(location, WSE, discharge)

    {
    # MAIN INFLOW TO BLUFF LAKE
    # outlet of griffin slough
    if(location==Bridge_1 & WSE<67.39) {x<-0}# no water
    if(location==Bridge_1 & WSE>67.39) {x<-1}# water moving in
    
    # discharge at Macon in cms
    if(location==Bridge_2 & WSE>68.11 & discharge<13.25) {x<--1}# water moving out
    if(location==Bridge_2 & WSE<68.11 & discharge<13.25) {x<-0}# no water 
    if(location==Bridge_2 & discharge>13.25) {x<-1}
    
    # water moving in at B2 is a function of discharge not WSE
    # discharge at Macon in cms
    if(location==Notch_1 & WSE<68.38 | discharge<6.56) {x<-0}# no water 
    if(location==Notch_1 & WSE>68.38 | discharge>6.56) {x<-1}# water moving in

    # SECONDARY INFLOW; WATER FROM NOXUBEE
    if(location==WCS2 & WSE<68.23) {x<-0}# no water
    if(location==WCS2 & WSE>68.23) {x<-1}# water moving in

    if(location==Notch_2 & WSE>68.75 & discharge<24.15) {x<--1}# water moving out
    if(location==Notch_2 & WSE<68.75 & discharge<24.15) {x<-0}# no water
    if(location==Notch_2 & WSE<69.20 &discharge>24.15) {x<-1}# water moving in

    return(x)
    }


# Function for converting elevation to volume or volume to elevation ----
#elevation in meters above sea level
#volume in cubic meters
dat <- read.csv("_dat/CompleteMap.csv")
volume<-NA
boards<-c(-10:-1,0:18)
elevation<-66.40+(0.20*boards) #added additional elevation up to ~70m
for(i in 1:length(elevation)){
  Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
  Z <- c((elevation[i]-Z))
  Z <- Z*4
  volume[i]<-sum(Z)
}
s<-as.data.frame(elevation)
data<-cbind(s, volume)
ggplot(data, aes(elevation, (volume/1000000))) + geom_line()+labs(y = bquote('Water Volume'~('million'~m^3)), x = "Water Surface Elevation (m)")+ theme_classic()
EL_2_Vol<- approxfun(elevation, volume, rule=2)
Vol_2_EL<- approxfun(volume, elevation, rule=2)

# Function for converting volume to elevation ----
#dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
#dat <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")

# Function for converting elevation or volume to surface area ----
# suface area in meters squared
# volume in cubic meters
# elevation in meters above sea level
surface <- NA
for(i in 1:length(elevation)){
  Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
  surface[i]<-length(Z)*4
}
Vol_2_SA<-approxfun(volume,surface,  rule=2)
EL_2_SA<-approxfun(elevation,surface,  rule=2)


# Function for Board Elevation over Time
# Board_Time<-function(DOY, Rotation)
# {
#   if(DOY>=1 & DOY<=14) {x<-68.19392}
#   if(DOY>=15 & DOY<=181) {x<-68.39712}
#   if(DOY>=182 & DOY<=195) {x<-68.19392}
#   if(DOY>=196 & DOY<=212) {x<-67.98562}
#   if(DOY>=213 & DOY<=226) {x<-67.77732}
#   if(DOY>=227 & DOY<=243) {x<-67.56902}
#   if(DOY>=244 & DOY<=334 & Rotation==1) {x<-67.33402}
#   if(DOY>=244 & DOY<=334 & Rotation==2) {x<-67.56902}
#   if(DOY>=335 & DOY<=348) {x<-67.77732}
#   if(DOY>=349 & DOY<=366) {x<-67.98562}
#   return(x)
# }

Board_Time<-function(period)
{
  if(period==1) {x<-68.20} 
  if(period==2) {x<-68.40} 
  if(period==3) {x<-68.20} 
  if(period==4) {x<-68.00}
  if(period==5) {x<-67.80}
  if(period==6) {x<-67.60}
  if(period==7) {x<-67.40}
  if(period==8) {x<-67.80}
  if(period==9) {x<-68.00}
  return(x)
}

#Board Penalty
Penalty<-as.data.frame(elevation)
Penalty$volume<-volume

DOY<-c(1:365)
datalist <- list()
for(i in 1:length(DOY)){
  Penalty$doy<-DOY[i]
  datalist[[i]]<-Penalty
}
Penalty <- do.call(rbind, datalist)

Penalty$period<-ifelse(test = Penalty$doy>=1 & Penalty$doy<=14, yes= "1", no=ifelse(Penalty$doy>=15 & Penalty$doy<=181, yes="2", no=ifelse(Penalty$doy>=182 & Penalty$doy<=195, yes="3", no=ifelse(Penalty$doy>=196 & Penalty$doy<=212, yes="4", no=ifelse(Penalty$doy>=213 & Penalty$doy<=226, yes="5", no=ifelse(Penalty$doy>=227 & Penalty$doy<=243, yes="6", no=ifelse(Penalty$doy>=244 & Penalty$doy<=334, yes="7", no=ifelse(Penalty$doy>=335 & Penalty$doy<=348, yes="8", no="9"))))))))

for(q in 1:nrow(Penalty)){
  num<-as.numeric(Penalty$period[q])+1
  num<-ifelse(num==10, 9, num)
  Penalty$Period_board[q]<-Board_Time(num)
}

Penalty$period<-as.factor(Penalty$period)

Penalty<- Penalty %>% 
  dplyr::group_by(elevation,period) %>% 
  dplyr::arrange(doy) %>%  
  dplyr::slice(n())

Penalty2<-Penalty
Penalty2$penalty<-Penalty$elevation/Penalty$Period_board
Penalty2$penalty<-rescale(Penalty2$penalty, to=c(0,1))

PP1<-subset(Penalty2, Penalty2$period==1)
PP2<-subset(Penalty2, Penalty2$period==2)
PP3<-subset(Penalty2, Penalty2$period==3)
PP4<-subset(Penalty2, Penalty2$period==4)
PP5<-subset(Penalty2, Penalty2$period==5)
PP6<-subset(Penalty2, Penalty2$period==6)
PP7<-subset(Penalty2, Penalty2$period==7)
PP8<-subset(Penalty2, Penalty2$period==8)
PP9<-subset(Penalty2, Penalty2$period==9)


PenaltyM1<-approxfun(PP1$elevation, PP1$penalty, rule=2,yleft=0,yright=1)
PenaltyM2<-approxfun(PP2$elevation, PP2$penalty, rule=2,yleft=0,yright=1)
PenaltyM3<-approxfun(PP3$elevation, PP3$penalty, rule=2,yleft=0,yright=1)
PenaltyM4<-approxfun(PP4$elevation, PP4$penalty, rule=2,yleft=0,yright=1)
PenaltyM5<-approxfun(PP5$elevation, PP5$penalty, rule=2,yleft=0,yright=1)
PenaltyM6<-approxfun(PP6$elevation, PP6$penalty, rule=2,yleft=0,yright=1)
PenaltyM7<-approxfun(PP7$elevation, PP7$penalty, rule=2,yleft=0,yright=1)
PenaltyM8<-approxfun(PP8$elevation, PP8$penalty, rule=2,yleft=0,yright=1)
PenaltyM9<-approxfun(PP9$elevation, PP9$penalty, rule=2,yleft=0,yright=1)

# Function for Board Elevation over Time

Penalty2$penalty<-ifelse(Penalty2$elevation>Penalty2$Period_board, 1, Penalty2$penalty)
Penalty2$penalty<-ifelse(Penalty2$elevation<66.568, 0, Penalty2$penalty)


Penalty2<-subset(Penalty2, Penalty2$period==1|Penalty2$period==2|Penalty2$period==3|Penalty2$period==4|Penalty2$period==5|Penalty2$period==6)
Penalty2$period<-as.character(Penalty2$period)
Penalty2$period[Penalty2$period=="2"] <- "2 & 9"
Penalty2$period[Penalty2$period=="3"] <- "3 & 8"
Penalty2$period[Penalty2$period=="4"] <- "4 & 7"


ggplot(Penalty2, aes(elevation, penalty, group=period,color=period)) + geom_line(size=0.75) + 
  labs(x = "Water Surface Elevation (m)", y="Penalty")+   
  theme_classic()
