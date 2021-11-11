#' Broad crested weir equation 
#'
#' This function simulates individual capture histories for PSPAP type monitoring program for the Missouri River
#' @param w width of the weir in meters
#' @param h head (difference in water surface elevation and weir elevation) in meaters
#' @keywords weir equation 
#' @export
#' @examples
#' weir()

broad_crested_weir<-function(w=NULL,h=NULL)
    {
    C=2.7                   # Tracy 1957
    w_ft<- w*3.281          # convert meters to feet
    h_ft <- h*3.281         # convert meters to feet
    Q<-(C*w_ft*h_ft^(3/2))  # discharge in cfs
    Q<- Q*0.0283            # convert to cms
    return(Q)
    }