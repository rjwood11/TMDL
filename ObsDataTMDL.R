
str(Cont.w)


#sites_millcreek = c("03431060", "03430550", "03431083","03431000")
sites_harpeth = c("03433500")
#sites_richland = c("03431700", "03431655")
parameter_codes = c("00060", "00300")


sites = readNWISsite(siteNumbers=c(sites_harpeth))
site_coords = sites[c(1),]
data.table::setnames(site_coords, old = c("dec_lat_va"),
                     new = c("Latitude"))
data.table::setnames(site_coords, old = c("dec_long_va"),
                     new = c("Longitude"))

Hwy100 = readNWISdata(sites=c("03433500"), service="dv",asDateTime=T)

#today_data = readNWISdata(sites=c('0343233905'), service="iv"))


Hwy100=Hwy100 %>% 
  group_by(site_no) #%>%
  #slice(which.max(as.POSIXct(dateTime,format = '%Y-%m-%d %H:%M:%S')))



HARPE062<-Cont.w[Cont.w$Station_ID=="HARPE062",]


Hwy100<-Cont.w[Cont.w$Station_ID=="USGS_03433500",]

str(Hwy100)

str(HARPE062)

HARPE062$Time <- format(HARPE062$Date,"%H:%M")

HARPE062$Date <- as.POSIXct(format(HARPE062$Date,"%Y-%m-%d"))

Hwy100 <- merge(HARPE062,Hwy100,by=c("Date"))


####################################################################################

unique(BaseModel$Station_ID)

BaseModel.188<-BaseModel1[BaseModel1$Station_ID=="LSPC188PERO",]

years<-c("2011","2012","2013","2014","2015","2016","2017","2018","2019")

BaseModel.188 = BaseModel.188[-1,] #Remove first line of a dataframe.

BaseModel.188$Year<-format(as.POSIXct(BaseModel.188$Date), format = "%Y")

BaseModel.188$Month<-format(as.POSIXct(BaseModel.188$Date), format = "%b")

BaseModel.2011<-BaseModel.188[BaseModel.188$Year=="2011",]

BaseModel.2012<-BaseModel.188[BaseModel.188$Year=="2012",]

BaseModel.2013<-BaseModel.188[BaseModel.188$Year=="2013",]

BaseModel.2014<-BaseModel.188[BaseModel.188$Year=="2014",]

BaseModel.2015<-BaseModel.188[BaseModel.188$Year=="2015",]

BaseModel.2016<-BaseModel.188[BaseModel.188$Year=="2016",]

BaseModel.2017<-BaseModel.188[BaseModel.188$Year=="2017",]

BaseModel.2018<-BaseModel.188[BaseModel.188$Year=="2018",]

BaseModel.2019<-BaseModel.188[BaseModel.188$Year=="2019",]

BaseModel.2011$TPLoad.csum <- ave(BaseModel.2011$TPLoad, BaseModel.2011$Station_ID, FUN=cumsum)

BaseModel.2012$TPLoad.csum <- ave(BaseModel.2012$TPLoad, BaseModel.2012$Station_ID, FUN=cumsum)

BaseModel.2013$TPLoad.csum <- ave(BaseModel.2013$TPLoad, BaseModel.2013$Station_ID, FUN=cumsum)

BaseModel.2014$TPLoad.csum <- ave(BaseModel.2014$TPLoad, BaseModel.2014$Station_ID, FUN=cumsum)

BaseModel.2015$TPLoad.csum <- ave(BaseModel.2015$TPLoad, BaseModel.2015$Station_ID, FUN=cumsum)

BaseModel.2016$TPLoad.csum <- ave(BaseModel.2016$TPLoad, BaseModel.2016$Station_ID, FUN=cumsum)

BaseModel.2017$TPLoad.csum <- ave(BaseModel.2017$TPLoad, BaseModel.2017$Station_ID, FUN=cumsum)

BaseModel.2018$TPLoad.csum <- ave(BaseModel.2018$TPLoad, BaseModel.2018$Station_ID, FUN=cumsum)

BaseModel.2019$TPLoad.csum <- ave(BaseModel.2019$TPLoad, BaseModel.2019$Station_ID, FUN=cumsum)

tail(BaseModel.2012)





#############################################################################################################

plot(BaseModel.188$FLOW_CMS,BaseModel.188$TP)

plot(log(BaseModel.188$TPLoad)~log(BaseModel.188$FLOW_CMS))

x<-log(BaseModel.188$TPLoad)

y<-log(BaseModel.188$FLOW_CMS)



abline(lm(x~y))

x <- log(BaseModel.188$FLOW_CMS)
y <- BaseModel.188$TP
a <- BaseModel.188$DO
b <- BaseModel.188$`SOLID-WS`
c <- log(BaseModel.188$PHYTO)


plot(c~x)

mlr<-lm(a~x+b+c)

summary(mlr)

plot(mlr)


