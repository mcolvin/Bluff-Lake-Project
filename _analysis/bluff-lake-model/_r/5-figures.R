figures<- function(n){

if(n==1)
    {
    cypress[,location:="Cypress"]
    WCS2[,location:="Intake"]
    dat<-rbind(WCS2[,.SD,.SDcols=c("location","Date.Time","elevation")],
        cypress[,.SD,.SDcols=c("location","Date.Time","elevation")])
    plot(elevation~Date.Time,dat,type='n',ylim=c(67.5,70.0),
        ylab="Elevation (m)",xlab="Date",las=1)
    points(elevation~Date.Time,dat,type='l',col="black",lwd=2,
        subset=location=="Intake")
    points(elevation~Date.Time,dat,type='l',col="grey",lwd=2,
        subset=location=="Cypress")
    legend("topleft",c("Intake","Cypress"),lwd=c(2,2),
        col=c("black","grey"),bty='n')
    }   

}