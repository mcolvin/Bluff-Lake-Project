##plot measurements of DO and temp vs time
#Temp
dat<-read.csv("_dat/2019_6_13edt.csv")
plot(Decimal.Time~Temp,data=dat, main="Time vs. Temperature",
     xlab="Temperature (C)",
     ylab="Military Time",
     type="n")
points(Decimal.Time~Temp,data=dat,
       subset=Measurement.Number=="1",
       col="red")
points(Decimal.Time~Temp,data=dat,
       subset=Measurement.Number=="2",
       col="blue")
points(Decimal.Time~Temp,data=dat,
       subset=Measurement.Number=="3",
       col="green")
points(Decimal.Time~Temp,data=dat,
       subset=Measurement.Number=="4",
       col="purple")
abline(lm(Decimal.Time~Temp, data=dat), col="red")

##Temperature is obviously increasing with time.
lm(Decimal.Time~Temp, data=dat)

#DO
plot(Decimal.Time~DO,data=dat,  main="Time vs. Dissolved Oxygen",
     xlab="Dissolved Oxygen (mg/L)",
     ylab="Military Time",
     type="n")
points(Decimal.Time~DO,data=dat,
       subset=Measurement.Number=="1",
       col="red")
points(Decimal.Time~DO,data=dat,
       subset=Measurement.Number=="2",
       col="blue")
points(Decimal.Time~DO,data=dat,
       subset=Measurement.Number=="3",
       col="green")
points(Decimal.Time~DO,data=dat,
       subset=Measurement.Number=="4",
       col="purple")
abline(lm(Decimal.Time~DO, data=dat), col="red")

##The pattern of increasing DO is less obvious than temperature. Let's flip the axis.

plot(DO~Decimal.Time,data=dat,  main="Dissolved Oxygen vs Time",
     xlab="Military Time",
     ylab="Dissolved Oxygen (mg/L)",
     type="n")
points(DO~Decimal.Time,data=dat,
       subset=Measurement.Number=="1",
       col="red")
points(DO~Decimal.Time,data=dat,
       subset=Measurement.Number=="2",
       col="blue")
points(DO~Decimal.Time,data=dat,
       subset=Measurement.Number=="3",
       col="green")
points(DO~Decimal.Time,data=dat,
       subset=Measurement.Number=="4",
       col="purple")
abline(lm(DO~Decimal.Time, data=dat), col="red")

##I like that alot better. I feel like a nlm would fit this better. There is a more obvious shift between measurement times 1 and 2. Which is how you would expect DO to behave.

plot(DO~Temp,data=dat, main="DO vs. Temperature",
     xlab="Temperature (C)",
     ylab="Dissolved Oxygen",
     type="n")
points(DO~Temp,data=dat,
       subset=Measurement.Number=="1",
       col="red")
points(DO~Temp,data=dat,
       subset=Measurement.Number=="2",
       col="blue")
points(DO~Temp,data=dat,
       subset=Measurement.Number=="3",
       col="green")
points(DO~Temp,data=dat,
       subset=Measurement.Number=="4",
       col="purple")
abline(lm(DO~Temp, data=dat), col="red")

##I liiiiike this graph. 
lm(DO~Temp, data=dat)

##Make boxplots separating data by vegetation type
boxplot(Turbidity~Veg, data=dat, main="Turbidity", xlab="Vegetation", ylab="Secchi Depth (cm)")
boxplot(DO~Veg, data=dat, main="Dissolved Oxygen",xlab="Vegetation", ylab="DO (mg/L)")
boxplot(Temp~Veg, data=dat, main="Temperature", xlab="Vegetation", ylab="Temperature (C)")

##I feel okay about not allocating sampling based on vegetation type. Vegetation clearly affects DO and turbidity, but at this point I'm not sure how much veg will be left or where it would even be. 

##I think we would be alright to make a single morning pass over all 20 points. This smpling occasion shows relationships between time, DO, and temperature that we can use later if we really want to look a changes within the day.


#I still need to look at DO profile data.
