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

dat <- read.csv("Bathymetry/CompleteMap.csv")
volume<-NA
boards<-c(-10:-1,0:17)
elevation<-66.568+(0.2032*boards) #added additional elevation up to ~70m
for(i in 1:length(elevation)){
  Z <- subset(dat$POINT_Z, dat$POINT_Z < (elevation[i]))
  Z <- c((elevation[i]-Z))
  Z <- Z*4
  volume[i]<-sum(Z)
}
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
  if(period==1) {x<-68.19392}
  if(period==2) {x<-68.39712}
  if(period==3) {x<-68.19392}
  if(period==4) {x<-67.98562}
  if(period==5) {x<-67.77732}
  if(period==6) {x<-67.56902}
  if(period==7) {x<-67.33402}
  if(period==8) {x<-67.77732}
  if(period==9) {x<-67.98562}
  return(x)
}

