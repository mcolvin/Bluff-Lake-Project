# Function for turning water on/off ----
In_out_el<-function(location, WSE, discharge)
    {
    if(location==Bridge_1 & WSE<67.39) {x<-0}# no water
    if(location==Bridge_1 & WSE>67.39) {x<-1}# water moving in

    if(location==Bridge_2 & WSE>68.11 & discharge<13.25) {x<--1}# water moving out
    if(location==Bridge_2 & WSE<68.11 & discharge<13.25) {x<-0}# no water 
    if(location==Bridge_2 & discharge>13.25) {x<-1}
    # water moving in at B2 is a function of discharge not WSE

    if(location==Notch_1 & WSE<68.38 | discharge<6.56) {x<-0}# no water 
    if(location==Notch_1 & WSE>68.38 | discharge>6.56) {x<-1}# water moving in

    if(location==WCS2 & WSE<68.23) {x<-0}# no water
    if(location==WCS2 & WSE>68.23) {x<-1}# water moving in

    if(location==Notch_2 & WSE>68.75 & discharge<24.15) {x<--1}# water moving out
    if(location==Notch_2 & WSE<68.75 & discharge<24.15) {x<-0}# no water
    if(location==Notch_2 & WSE<69.20 &discharge>24.15) {x<-1}# water moving in

    return(x)
    }

# Function for converting elevation to volume or volume to elevation ----
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
volume <- c(257558197, 258323648, 259113476, 259651880, 259786359,
            259909631, 260121798, 260439030, 260989054, 261496613)
EL_Vol<- approxfun(elevation, volume, rule=2)
Vol_EL<- approxfun(volume, elevation, rule=2)

# Function for converting volume to elevation ----
dat <- read.csv("~/GitHub/Bluff-Lake-Project/_analysis/Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")
dat <- read.csv("Depth-Mapping/_dat/Bathymetry/WCS_BTTMUP_2_2.csv")

# Function for converting elevation or volume to surface area ----
surface <- c(56128, 98168, 206544, 520988, 1239356, 1968932, 
            2609612, 3145228, 3448052, 3793340)

SA_Vol<-approxfun(surface, volume, rule=2)
SA_EL<-approxfun(surface, elevation, rule=2)

# Function for Board Elevation over Time
Board_Time<-function(DOY, Rotation)
{
if(DOY>=1 & DOY<=14) {x<-68.19392}
if(DOY>=15 & DOY<=181) {x<-68.39712}
if(DOY>=182 & DOY<=195) {x<-68.19392}
if(DOY>=196 & DOY<=212) {x<-67.98562}
if(DOY>=213 & DOY<=226) {x<-67.77732}
if(DOY>=227 & DOY<=243) {x<-67.56902}
if(DOY>=244 & DOY<=334 & Rotation==1) {x<-67.33402}
if(DOY>=244 & DOY<=334 & Rotation==2) {x<-67.56902}
if(DOY>=335 & DOY<=348) {x<-67.77732}
if(DOY>=349 & DOY<=366) {x<-67.98562}
return(x)
}
