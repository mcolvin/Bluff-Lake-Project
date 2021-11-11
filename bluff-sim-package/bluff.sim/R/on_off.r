#' On off function 
#'
#' This function allows water to enter, leave, or stay in bluff lake depending on location and lake elevation.
#' @param location of inflow/outflow to lake (Bridge_1, Bridge_2, Notch_1, WCS2, WCS2, Notch_2)
#' @param WSE water surface elevation in meters above sea level
#' @param discharge in cubic meters per second 
#' @return x = indictor of water going into the lake (1), no connection (0), out of lake (-1)
#' @keywords weir equation 
#' @export
#' @examples
#' weir()

In_out_el<-function(location, WSE, discharge)
    {
    ## output
    ### x = indictor of water going into the lake (1), no connection (0), out of lake (-1)
    
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
