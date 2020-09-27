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
        plot(Intake~cont_time,lake_info,type='l',ylab="Water surface elevation")
        par(new=TRUE)
        plot(discharge_cms~cont_time,lake_info,type='l',yaxt="n",ylab='',
            col="lightgrey",lwd=3)
        axis(side=4, at=axTicks(2),labels=TRUE)
        mtext(side=4,"Discharge")
        legend("topleft",legend=c("Intake","Macon gage"),
            col=c("black","lightgrey"),lwd=3,bg="white")

        } 
    if(n=="macon-intake")
        {
        fit<- lm(Intake~log(discharge_cms),lake_info)  
        lake_info$pred<-predict(fit,lake_info)
        par(mfrow=c(2,1))
        #plot(Cypress~discharge_cms,lake_info)  
        #plot(Cypress~log(discharge_cms),lake_info)  
        plot(Intake~log(discharge_cms),lake_info)  
        plot(pred~Intake,lake_info)
        abline(0,1)
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
    
    
    
    
    