


library(data.table)

out<-data.frame()
for(yr in c(1990:1992,1994:2011,2013:2020))
    {
    dat<-fread(paste0("C:/Users/mcolvin/OneDrive - Mississippi State University/share/years/",yr,"-hydro-sims-Final-drawdown.csv"))
    out<-rbind(out,dat[,.(end=mean(Board2)),by=.(year,period,WCS_strategy,Board1)])
    }
out[,period2:= period+1]
setDT(out)[period==9,period2:=1]



tmat<- dcast(out,WCS_strategy+period+Board1~period2



table(dat[period==1&doy==1 & hour==0,]$WCS_strategy)

library(lattice)
xyplot(ele_lake~dateTime|Board1,dat[period==5 & WCS_strategy==0,])



dat[period==1&doy==1 & hour==0,]$WCS_strategy



