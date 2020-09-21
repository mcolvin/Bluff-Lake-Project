figures<- function(n)
    {
    if(n==1)
        {
        plot(Cypress~cont_time,lake_info,type='l',ylab="Water surface elevation",
            ylim=c(67.5,70.5))
        points(Gauge~cont_time,lake_info,type='l',col="red")
        points(Intake~cont_time,lake_info,type='l',col="green")
        par(new=TRUE)
        plot(discharge_cms~cont_time,lake_info,type='l',yaxt="n",ylab='',
            col="lightgrey")
        axis(side=4, at=axTicks(2),labels=TRUE)
        mtext(side=4,"Discharge")
             abline(v=125000)       
        legend("topleft",legend=c("Cypress","Gauge","Intake","Macon gauge"),
            col=c("black","red","green","lightgrey"),lwd=3,bg="white")

        } 

    if(n==2)
        {
        plot(Intake~discharge_cms,lake_info,type='p',ylab="Water surface elevation")
        plot(Cypress~cont_time,lake_info,type='l',ylab="Water surface elevation",
            ylim=c(67.5,70.5))
        points(Gauge~cont_time,lake_info,type='l',col="red")
        points(Intake~cont_time,lake_info,type='l',col="green")
        par(new=TRUE)
        plot(discharge_cms~cont_time,lake_info,type='l',yaxt="n",ylab='',
            col="lightgrey")
        axis(side=4, at=axTicks(2),labels=TRUE)
        mtext(side=4,"Discharge")
             abline(v=125000)       
        legend("topleft",legend=c("Cypress","Gauge","Intake","Macon gauge"),
            col=c("black","red","green","lightgrey"),lwd=3,bg="white")
        } 
        
    if(n=="bluff-lake-daily-q-input")
        {
        # plot of discharge scaled to watershed size
        # for bluff lake
        plot(Q_bl~date,discharge_daily,type="l",
            ylab="Discharge, scaled to watershed size (m^3/s)",
            xlab="Date")
        }
    if(n=="bluff-lake-hourly-q-input")
        {
        # plot of discharge scaled to watershed size
        # for bluff lake
        plot(Q_bl~date,discharge_hourly,type="l",
            ylab="Discharge, scaled to watershed size (m^3/s)",
            xlab="Date")
            loggers
         points(Q_bl~date, discharge_daily,type="p",col="red")
        }        
        
        
    }
    
    discharge_hourly
    
    
    
    
    