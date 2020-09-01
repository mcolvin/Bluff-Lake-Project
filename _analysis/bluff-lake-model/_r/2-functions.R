#Function for turning water on/off
In_out_el<-function(location, WSE, discharge)
    {
    if(location==Bridge_1 & WSE<67.39) {x<-0)# no water
    if(location==Bridge_1 & WSE>67.39) {x<-1)# water moving in

    if(location==Bridge_2 & WSE>68.11 & discharge<13.25) {x<--1}# water moving out
    if(location==Bridge_2 & WSE<68.11 & discharge<13.25) {x<-0)# no water 
    if(location==Bridge_2 & discharge>13.25) {x<-1}
    # water moving in at B2 is a function of discharge not WSE

    if(location==Notch_1 & WSE<68.38 | discharge<6.56) {x<-0)# no water 
    if(location==Notch_1 & WSE>68.38 | discharge>6.56) {x<-1)# water moving in

    if(location==WCS2 & WSE<68.23) {x<-0)# no water
    if(location==WCS2 & WSE>68.23) {x<-1)# water moving in

    if(location==Notch_2 & WSE>68.75 & discharge<24.15) {x<--1}# water moving out
    if(location==Notch_2 & WSE<68.75 & discharge<24.15) {x<-0)# no water
    if(location==Notch_2 & WSE<69.20 &discharge>24.15) {x<-1)# water moving in

    return(x)
    }

#function for converting elevation to volume or volume to elevation
elevation <- c(66.45402, 66.67402, 66.89402, 67.11402, 67.33402,
               67.55402, 67.77402, 67.99402, 68.21402, 68.43402)
volume <- c(257558197, 258323648, 259113476, 259651880, 259786359,
            259909631, 260121798, 260439030, 260989054, 261496613)
EL_Vol<- approxfun(elevation, volume, rule=2)
Vol_EL<- approxfun(volume, elevation, rule=2)
