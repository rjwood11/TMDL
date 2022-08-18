
str(Cont.w)

HARPE062<-Cont.w[Cont.w$Station_ID=="HARPE062",]


Hwy100<-Cont.w[Cont.w$Station_ID=="USGS_03433500",]

str(Hwy100)

str(HARPE062)

HARPE062$Time <- format(HARPE062$Date,"%H:%M")

HARPE062$Date <- as.POSIXct(format(HARPE062$Date,"%Y-%m-%d"))

Hwy100 <- merge(HARPE062,Hwy100,by=c("Date"))

