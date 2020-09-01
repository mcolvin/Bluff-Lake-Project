
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

