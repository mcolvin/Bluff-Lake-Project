#' Weir equation function https://www.engineeringtoolbox.com/weirs-flow-rate-d_592.html
#'
#' This function simulates individual capture histories for PSPAP type monitoring program for the Missouri River
#' @param w width of the weir in meters
#' @param h head (difference in water surface elevation and weir elevation) in meaters
#' @keywords weir equation 
#' @export
#' @examples
#' weir()

sharp_crested_weir<-function(w=NULL,h=NULL)
    {
    g=9.81 # acceleration due to gravity m/sec^2
    Q<-(2/3)*0.66*(2*g)^(0.5)*w*h^(3/2)
    return(Q)
    }