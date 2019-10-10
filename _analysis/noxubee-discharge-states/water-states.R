library(dataRetrieval)

#Retrieve daily Q
siteNumber<-c("02448000")
parameterCd <- "00060" #Discharge
dailyQ <- readNWISdv(siteNumber, parameterCd) 
dailyQ <- renameNWISColumns(dailyQ)
stationInfo <- readNWISsite(siteNumber)

# Calculate moving average
library(dplyr)

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(dailyQ$Date))) != (nrow(dailyQ)+1)){
  fullDates <- seq(from=min(dailyQ$Date),
                   to = max(dailyQ$Date), by="1 day")
  fullDates <- data.frame(Date = fullDates, 
                          agency_cd = dailyQ$agency_cd[1],
                          site_no = dailyQ$site_no[1],
                          stringsAsFactors = FALSE)
  dailyQ <- full_join(dailyQ, fullDates,
                      by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

ma <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}



dailyQ$lnFlow<- log(dailyQ$Flow)

dailyQ <- dailyQ %>%
  mutate(rollMean = as.numeric(ma(lnFlow)),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))
dailyQ$year<- as.numeric(format(dailyQ$Date,"%Y")) 
plot(rollMean~Date,dailyQ,subset=year>=2018)
plot(Flow~Date,dailyQ,subset=year>=2018)
plot(Flow~Date,dailyQ)
 
## Calculate historical percentiles
summaryQ <- dailyQ %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rollMean, probs = .75, na.rm = TRUE),
            p25 = quantile(rollMean, probs = .25, na.rm = TRUE),
            p10 = quantile(rollMean, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rollMean, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rollMean, probs = 0, na.rm = TRUE)) 

current.year <- as.numeric(strftime(Sys.Date(), format = "%Y"))

summary.0 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
    origin = paste0(current.year-2,"-01-01")),
    day.of.year = day.of.year - 365)
summary.1 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))
summary.2 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

summaryQ <- bind_rows(summary.0, summary.1, summary.2) 

smooth.span <- 0.3

summaryQ$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ, span = smooth.span))



summaryQ <- select(summaryQ, Date, day.of.year,p75,p25,p10,p05,p00,
                   sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

latest.years <- dailyQ %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))
  
  
title.text <- paste0(stationInfo$station_nm,"\n",
        "Provisional Data - Subject to change\n",
        "Record Start = ", min(dailyQ$Date),
        "  Number of years = ",
        as.integer(as.numeric(difftime(time1 = max(dailyQ$Date), 
        time2 = min(dailyQ$Date),
        units = "weeks"))/52.25),
        "\nDate of plot = ",Sys.Date(),
        "  Drainage Area = ",stationInfo$drain_area_va, "mi^2")

mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

summary.year1 <- data.frame(summaryQ[2:366,])
summary.year2 <- data.frame(summaryQ[367:733,])


#----------------------------------------------------------------------
# 
#  LOWESS SMOOTHED
#
#----------------------------------------------------------------------
plot(latest.years$day.of.year, (latest.years$rollMean), 
     xlim = c(1, 733),xaxt='n',
     type="l", las=1,ylim=c(3.5,9),
     ylab = "30-day moving ave",
     xlab = "")
title(title.text, cex.main = 0.75)
polygon(c(summary.year1$day.of.year,rev(summary.year1$day.of.year)),
        (c(summary.year1$sm.75, rev(summary.year1$sm.25))), 
        col = "darkgreen", border = FALSE)
polygon(c(summary.year2$day.of.year,rev(summary.year2$day.of.year)),
        (c(summary.year2$sm.75, rev(summary.year2$sm.25))), 
        col = "darkgreen", border = FALSE)

polygon(c(summary.year1$day.of.year,rev(summary.year1$day.of.year)),
        (c(summary.year1$sm.25, rev(summary.year1$sm.10))), 
        col = "yellow", border = FALSE)
polygon(c(summary.year2$day.of.year,rev(summary.year2$day.of.year)),
       (c(summary.year2$sm.25, rev(summary.year2$sm.10))), 
        col = "yellow", border = FALSE)
        
        
polygon(c(summary.year1$day.of.year,rev(summary.year1$day.of.year)),
        (c(summary.year1$sm.10, rev(summary.year1$sm.05))), 
        col = "orange", border = FALSE)
polygon(c(summary.year2$day.of.year,rev(summary.year2$day.of.year)),
        (c(summary.year2$sm.10, rev(summary.year2$sm.05))), 
        col = "orange", border = FALSE)
polygon(c(summary.year1$day.of.year,rev(summary.year1$day.of.year)),
        (c(summary.year1$sm.05, rev(summary.year1$sm.00))), 
        col = "red", border = FALSE)
polygon(c(summary.year2$day.of.year,rev(summary.year2$day.of.year)),
        (c(summary.year2$sm.05, rev(summary.year2$sm.00))), 
        col = "red", border = FALSE)
lines(latest.years$day.of.year, (latest.years$rollMean), 
      lwd=2, col = "black")
abline(v = 366)
axis(1, at =  c(mid.month.days,365+mid.month.days), 
     labels = rep(month.letters,2), 
     tick = FALSE, line = -0.5, cex.axis = 0.75)
axis(1, at = c(start.month.days, 365+start.month.days),
     labels = NA, tck = -0.02)
axis(1, at = c(182,547), labels = c(current.year-1,current.year), 
     line = .5, tick = FALSE)
legend("topright", label.text,
       fill = c("darkgreen","yellow","orange","red"),
       bty = "n", cex = 0.75)
box()