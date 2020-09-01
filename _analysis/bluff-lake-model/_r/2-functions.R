
#----------------------------------------------------------------------
# 
#  Bridges and Notches
#
#----------------------------------------------------------------------

In_out<-function(location,wse)
    {
    if(location==1 & WSE<67.39) {x<-0}# no water
    if(location==1 & WSE>67.39) {x<-1}# water moving in

    if(location==2 & WSE>68.11) {x<-1}# water moving out
    if(location==2 & WSE<68.11) {x<-0}# no water
    if(location==2 & WSE>???) {x<-1}# water moving in

    if(location==3 & WSE<68.04) {x<-0}# no water
    if(location==3 & WSE>68.04) {x<-1}# water moving in

    if(location==4 & WSE<68.23) {x<-0}# no water
    if(location==4 & WSE>68.23) {x<-1}# water moving in

    if(location==5 & WSE>???) {x<-1}# water moving out
    if(location==5 & WSE<68.82) {x<-0}# no water
    if(location==5 & WSE>68.82) {x<-1}# water moving in

    return(x)
    }

