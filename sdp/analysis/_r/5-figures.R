figures<- function(n){

if(n==1)
    {
    # gam fit for intake water surface elevation predicted from macon discharge
    plot(Intake~doy, lake_info, type="l", col="blue", ylim=c(69,70.2), xlim=c(0,365), ylab=NA, xlab=NA)
    par(new=T)
    plot(FitG4~doy, newdat, type="l", col="red", ylim=c(69,70.2), xlim=c(0,365),
         main="GAM Model Intake ~ Macon + DOY", xlab="Date", 
         ylab="Water Surface Elevation")
    legend("topright", c("Predicted", "Lake Elevation"),
           col = c("red", "blue"), lty = c(1, 1))  

    }
if(n=="utilities")
    {
    # waterbirds    
    WB2<-ggplot(utility_data, aes(elevation, WB)) + geom_line() + 
        labs(y = "Hectares <0.20m in depth", x = "Water Surface Elevation (m)")+   
        theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
        annotate(geom="text", x=64.5, y=425,size=3,label="B")
    WF2<-ggplot(utility_data, aes(elevation, WF)) + geom_line() + 
        labs(y = "Duck Energy Days (million)", x = "Water Surface Elevation (m)")+   
        theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
        annotate(geom="text", x=64.5, y=2,size=3,label="A")
    Fish<-ggplot(utility_data, aes(elevation,Fish/1000000)) + geom_line() + 
        labs(y = bquote('Water Volume'~('million'~m^3)), x = "Elevation")+   
        theme_classic()+theme(legend.position = "none")+ylim(0,10)+theme(axis.title.x=element_blank(), text = element_text(size=8))+
        annotate(geom="text", x=64.5, y=10,size=3,label="E") 
    Bank<-ggplot(utility_data, aes(elevation, Anglers)) + geom_line() + 
        labs(y = "Hectares >1m in depth", x = "Water Surface Elevation (m)")+   
        theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
        annotate(geom="text", x=64.5, y=7,size=3,label="C")
    Boat<-ggplot(utility_data, aes(elevation, Ramp)) + geom_line() + 
        labs(y = "Square meters >0.5m", x = "Water Surface Elevation (m)")+   
        theme_classic()+theme(axis.title.x=element_blank(), text = element_text(size=8))+
        annotate(geom="text", x=64.5, y=225,size=3,label="D")
     grid.arrange(WF2, WB2, Bank, Boat,Fish, ncol=3,
             bottom="Water Surface Elevation (m)")
    }

}