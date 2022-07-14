HarpethTMDL
================
RJ
2022-07-14

``` r
WASP.version<-8
```

# Introduction

**Harpeth TMDL** – Water Quality Analysis Simulation Program (WASP
version 8)

Exploration of water quality data using the calibrated WASP models
provided to TDEC and others by the EPA Region 4

``` r
#Load in required packages

library(tidyverse)
library(osmdata) 
library(ggmap)
library(dataRetrieval)
library(XML)
library(DT)
library(dbplyr)
library(widgetframe)
library(rgdal)
library(leaflet)
library(plotly)
library(formattable)
library(htmltools)
library(leaflet)
library(sf)
library(kableExtra)
library(knitr)
library(wql)
library(readxl)
library(fasstr)
library(readr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(reshape)
library(reshape2)

time = lubridate::with_tz(Sys.time(), "CST6CDT")
```

This file was last updated on 2022-07-14 14:43:33

``` r
#Import Data


Grab <- read_excel("GrabSamples.xlsx", 
    col_types = c("skip", "text", "text", 
        "skip", "skip", "date", "text", "numeric", 
        "text"))

head(Grab,15)
## # A tibble: 15 × 6
##    DataSource Station_ID             Date_Time           Pcode      Result Units
##    <chr>      <chr>                  <dttm>              <chr>       <dbl> <chr>
##  1 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 COND     306      uS/c…
##  2 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 DOSAT     90.3    %    
##  3 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 DO        12.6    mg/l 
##  4 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 PH         7.76   std …
##  5 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 NO3_N      3.03   mg/l 
##  6 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 TEMPWC     0.4    degC 
##  7 STORET     11NPSWRD_WQX_NATR_BUBR 2012-01-03 08:33:00 FLOW_CMS   0.0212 cms  
##  8 TDEC       TNW000002791           2012-01-03 11:55:00 COND     376      uS/c…
##  9 TDEC       TNW000002790           2012-01-03 10:45:00 COND     373      uS/c…
## 10 TDEC       TNW000003300           2012-01-03 09:35:00 COND     295      uS/c…
## 11 TDEC       TNW000005654           2012-01-03 12:20:00 COND     246      uS/c…
## 12 TDEC       TNW000003508           2012-01-03 11:15:00 COND     242      uS/c…
## 13 TDEC       TNW000006704           2012-01-03 13:15:00 COND     234      uS/c…
## 14 TDEC       TNW000006704           2012-01-03 13:15:00 DO        16.5    mg/l 
## 15 TDEC       TNW000003508           2012-01-03 11:15:00 DO        15.6    mg/l

BaseModel <- read_delim("Epa_Base_Model_2011_To_2020.txt", 
     delim = "\t", escape_double = FALSE, 
     col_types = cols(`Date-Time` = col_datetime(format = "%m/%d/%Y %H:%M"), 
         CBOD = col_number(), DO = col_number(), 
         FLOW_CMS = col_number(), NH3_N = col_number(), 
         NO3O2 = col_number(), PHYTO = col_number(), 
         SOD_T = col_number(), `SOLID-WS` = col_number(), 
         TN = col_number(), TP = col_number(), 
         WTEMP = col_number()), trim_ws = TRUE)

head(BaseModel,15)
## # A tibble: 15 × 13
##    `Date-Time`         Station_ID   CBOD    DO FLOW_CMS  NH3_N NO3O2 PHYTO SOD_T
##    <dttm>              <chr>       <dbl> <dbl>    <dbl>  <dbl> <dbl> <dbl> <dbl>
##  1 2011-01-01 00:00:00 ARKANSAS_1… 0      0       0.202 0      0         0  0   
##  2 2011-01-02 00:04:00 ARKANSAS_1… 0.742  7.22    0.200 0.0116 0.153     0  1.22
##  3 2011-01-03 00:01:00 ARKANSAS_1… 0.742 10.7     0.200 0.0223 0.158     0  1.26
##  4 2011-01-04 00:04:00 ARKANSAS_1… 0.742 10.7     0.200 0.0354 0.160     0  1.25
##  5 2011-01-05 00:08:00 ARKANSAS_1… 0.742 10.7     0.200 0.0465 0.161     0  1.24
##  6 2011-01-06 00:01:00 ARKANSAS_1… 0.742 10.7     0.200 0.0550 0.161     0  1.22
##  7 2011-01-07 00:04:00 ARKANSAS_1… 0.742 10.7     0.200 0.0612 0.161     0  1.21
##  8 2011-01-08 00:08:00 ARKANSAS_1… 0.742 10.7     0.200 0.0658 0.161     0  1.20
##  9 2011-01-09 00:01:00 ARKANSAS_1… 0.742 10.7     0.200 0.0690 0.161     0  1.19
## 10 2011-01-10 00:04:00 ARKANSAS_1… 0.742 10.7     0.200 0.0711 0.161     0  1.18
## 11 2011-01-11 00:07:00 ARKANSAS_1… 0.742 10.7     0.200 0.0724 0.161     0  1.18
## 12 2011-01-12 00:01:00 ARKANSAS_1… 0.742 10.7     0.200 0.0731 0.161     0  1.17
## 13 2011-01-13 00:04:00 ARKANSAS_1… 0.742 10.7     0.200 0.0733 0.161     0  1.16
## 14 2011-01-14 00:07:00 ARKANSAS_1… 0.742 10.7     0.200 0.0731 0.161     0  1.15
## 15 2011-01-15 00:00:00 ARKANSAS_1… 0.742 10.7     0.200 0.0727 0.161     0  1.15
## # … with 4 more variables: `SOLID-WS` <dbl>, TN <dbl>, TP <dbl>, WTEMP <dbl>


Cont <- read_csv("Cont.csv", col_types = cols(...1 = col_skip(), 
    Date_Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
    Result = col_number()))

head(Cont,15)
## # A tibble: 15 × 5
##    Station_ID    Date_Time           Pcode  Result Units
##    <chr>         <dttm>              <chr>   <dbl> <chr>
##  1 USGS_03432390 2012-01-26 00:00:00 TEMPWC   11.8 degC 
##  2 USGS_03432390 2012-01-26 00:00:00 COND    550   <NA> 
##  3 USGS_03432390 2012-01-26 06:00:00 TEMPWC   12.6 degC 
##  4 USGS_03432390 2012-01-26 06:00:00 COND    552   <NA> 
##  5 USGS_03432390 2012-01-26 12:00:00 TEMPWC   13.6 degC 
##  6 USGS_03432390 2012-01-26 12:00:00 COND    499   <NA> 
##  7 USGS_03432390 2012-01-26 18:00:00 TEMPWC   14   degC 
##  8 USGS_03432390 2012-01-26 18:00:00 COND    451   <NA> 
##  9 USGS_03432390 2012-01-27 00:00:00 TEMPWC   12.5 degC 
## 10 USGS_03432390 2012-01-27 00:00:00 COND    306   <NA> 
## 11 USGS_03432390 2012-01-27 06:00:00 TEMPWC   11.9 degC 
## 12 USGS_03432390 2012-01-27 06:00:00 COND    455   <NA> 
## 13 USGS_03432390 2012-01-27 12:00:00 TEMPWC   12.2 degC 
## 14 USGS_03432390 2012-01-27 12:00:00 COND    512   <NA> 
## 15 USGS_03432390 2012-01-27 18:00:00 TEMPWC   12.6 degC


unique(Grab[c(4,6)])
## # A tibble: 52 × 2
##    Pcode    Units     
##    <chr>    <chr>     
##  1 COND     uS/cm @25C
##  2 DOSAT    %         
##  3 DO       mg/l      
##  4 PH       std units 
##  5 NO3_N    mg/l      
##  6 TEMPWC   degC      
##  7 FLOW_CMS cms       
##  8 TSS      mg/l      
##  9 TURB     NTU       
## 10 NO3O2_N  mg/l      
## # … with 42 more rows
```

`Grab` data table contains grab sample results collected from 171
locations throughout the Harpeth River watershed

``` r
#Subset Data into Logical WASP Segments
# HARPETH_1 = Mouth of the Harpeth
# JONES_167 = Jones Creek
# HARPETH_2 = Main Stem w/o Jones Creek
# TURNBULL_143 = Turnbull Creek
# S_HARPETH_124 = South Harpeth
# HARPETH_34 = West Harpeth
# HARPETH_62 = Upstream of Franklin STP
# LSPC188PERO = Downstream of Franklin STP
# LSPC183PERO = Headwaters


sites=c("HARPETH_1","JONES_167","HARPETH_2", "TURNBULL_143", "S_HARPETH_124", "HARPETH_34", "LSPC188PERO", "HARPETH_62", "LSPC183PERO")

colnames(BaseModel)[1] <- "Date"

colnames(Grab)[3] <- "Date"

colnames(Cont)[2] <- "Date"

BaseModel<-BaseModel[BaseModel$Station_ID==sites,]


BaseModel$TPLoad<-BaseModel$TP*0.00220462*BaseModel$FLOW_CMS*86400

BaseModel$TPLoad.csum <- ave(BaseModel$TPLoad, BaseModel$Station_ID, FUN=cumsum)

#####################################################################################################

molten.data <- melt(Grab, id = c("Date","Station_ID","Pcode","Units","DataSource"))

Grab.w<-dcast(molten.data, Date+Station_ID~Pcode, mean)


head(Grab.w, 10)
##                   Date             Station_ID ALK BOD30 BOD5 CBOD30 CBOD5
## 1  2012-01-03 08:33:00 11NPSWRD_WQX_NATR_BUBR NaN   NaN  NaN    NaN   NaN
## 2  2012-01-03 09:35:00           TNW000003300 NaN   NaN  NaN    NaN   NaN
## 3  2012-01-03 10:45:00           TNW000002790 NaN   NaN  NaN    NaN     1
## 4  2012-01-03 11:15:00           TNW000003508 NaN   NaN  NaN    NaN   NaN
## 5  2012-01-03 11:55:00           TNW000002791 NaN   NaN  NaN    NaN   NaN
## 6  2012-01-03 12:20:00           TNW000005654 NaN   NaN  NaN    NaN   NaN
## 7  2012-01-03 13:15:00           TNW000006704 NaN   NaN  NaN    NaN   NaN
## 8  2012-01-04 08:30:00           TNW000003576 NaN   NaN  NaN    NaN     1
## 9  2012-01-04 09:43:00           TNW000006443 NaN   NaN  NaN    NaN   NaN
## 10 2012-01-04 10:25:00           TNW000003306 NaN   NaN  NaN    NaN   NaN
##    CBOD90 CHLA COND    DO DOSAT FLOW_CFS   FLOW_CMS NBOD90  NH3_N NO2 NO2_N NO3
## 1     NaN  NaN  306 12.55  90.3      NaN 0.02123764    NaN    NaN NaN   NaN NaN
## 2     NaN  NaN  295 14.36   NaN      NaN        NaN    NaN    NaN NaN   NaN NaN
## 3     NaN  NaN  373 12.81   NaN      NaN        NaN    NaN 0.0165 NaN   NaN NaN
## 4     NaN  NaN  242 15.58   NaN      NaN        NaN    NaN    NaN NaN   NaN NaN
## 5     NaN  NaN  376 13.39   NaN      NaN        NaN    NaN    NaN NaN   NaN NaN
## 6     NaN  NaN  246 15.08   NaN      NaN        NaN    NaN    NaN NaN   NaN NaN
## 7     NaN  NaN  234 16.51   NaN      NaN        NaN    NaN    NaN NaN   NaN NaN
## 8     NaN  NaN  544 13.33   NaN      NaN        NaN    NaN 0.0165 NaN   NaN NaN
## 9     NaN  NaN  317 14.70   NaN      NaN        NaN    NaN 0.0165 NaN   NaN NaN
## 10    NaN  NaN  421 17.50   NaN      NaN        NaN    NaN 0.0165 NaN   NaN NaN
##    NO3_N NO3O2_N OPO4 OPO4_P ORGN ORP PERI%N PERI%P PERI_AI PERI_ASH PERI_NP
## 1   3.03     NaN  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 2    NaN    0.43  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 3    NaN    1.10  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 4    NaN    0.15  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 5    NaN    1.10  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 6    NaN    0.41  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 7    NaN    0.76  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 8    NaN    1.30  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 9    NaN    1.70  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
## 10   NaN    0.92  NaN    NaN  NaN NaN    NaN    NaN     NaN      NaN     NaN
##    PERI_TN PERI_TP PERIAFDM PERICHLA   PH SOD_20 SOD_T SRP TBOD90 TDP TDS
## 1      NaN     NaN      NaN      NaN 7.76    NaN   NaN NaN    NaN NaN NaN
## 2      NaN     NaN      NaN      NaN 7.85    NaN   NaN NaN    NaN NaN NaN
## 3      NaN     NaN      NaN      NaN 8.04    NaN   NaN NaN    NaN NaN NaN
## 4      NaN     NaN      NaN      NaN 8.27    NaN   NaN NaN    NaN NaN NaN
## 5      NaN     NaN      NaN      NaN 8.13    NaN   NaN NaN    NaN NaN NaN
## 6      NaN     NaN      NaN      NaN 8.27    NaN   NaN NaN    NaN NaN NaN
## 7      NaN     NaN      NaN      NaN 8.45    NaN   NaN NaN    NaN NaN NaN
## 8      NaN     NaN      NaN      NaN 7.41    NaN   NaN NaN    NaN NaN NaN
## 9      NaN     NaN      NaN      NaN 8.31    NaN   NaN NaN    NaN NaN NaN
## 10     NaN     NaN      NaN      NaN 8.59    NaN   NaN NaN    NaN NaN NaN
##    TEMPWC   TKN  TN    TP  TS TSS TURB
## 1    0.40   NaN NaN   NaN NaN NaN  NaN
## 2    4.33 0.065 NaN 0.006 NaN   5 0.68
## 3    6.16 0.065 NaN 0.210 NaN   5 3.07
## 4    2.61 0.065 NaN 0.013 NaN   5 0.32
## 5    6.36 0.200 NaN 0.210 NaN   5 2.81
## 6    6.19 0.065 NaN 0.044 NaN   5 0.74
## 7    3.39 0.065 NaN 0.082 NaN   5 1.75
## 8    4.62 0.065 NaN 0.230 NaN   5 1.36
## 9    4.60 0.065 NaN 0.210 NaN   5 0.52
## 10   5.46 0.200 NaN 0.033 NaN   5 0.88
```

\#Data Summary

Subset of 9 modeled segments in WASP

``` r
ggplot(data = BaseModel, aes(x = Date, y = FLOW_CMS, group = Station_ID, colour = Station_ID)) +
    geom_line() +
    facet_wrap(~ Station_ID)
```

![](HarpethTMDL_files/figure-gfm/Basic%20Plots-1.png)<!-- -->

``` r





ggplot(data = BaseModel, aes(x = Date, y = TPLoad, group = Station_ID, colour = Station_ID, )) +
  ylab("TP Load (lbs/day)") +
    geom_line() +
    facet_wrap(~ Station_ID)
```

![](HarpethTMDL_files/figure-gfm/Basic%20Plots-2.png)<!-- -->

``` r

#Boxplot - look at data distribution



tp<-ggplot(BaseModel, aes(x = Station_ID, y = TN, fill = Station_ID)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("TP (mg/L)")

tp
```

![](HarpethTMDL_files/figure-gfm/Basic%20Plots-3.png)<!-- -->

``` r

tn<-ggplot(BaseModel, aes(x = Station_ID, y = TN, fill = Station_ID)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("TN (mg/L)")

tn
```

![](HarpethTMDL_files/figure-gfm/Basic%20Plots-4.png)<!-- -->

``` r
LSPC188_tp<-ggplot(BaseModel,aes(x=Date,y=TP))+geom_line(data=subset(BaseModel,Station_ID=="LSPC188PERO")) +theme_bw()+
  labs(y="TP (mg/L)")+ylim(min(BaseModel$TP),max(BaseModel$TP))+
  theme(
    axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Down","Frank_CottonLn","Franklin_Site_2","Franklin_Site_3")), aes(x=Date, y=TP, color=Station_ID))

LSPC188_tp
```

![](HarpethTMDL_files/figure-gfm/Data%20Comparison-1.png)<!-- -->

``` r
HARPETH_62<-ggplot(BaseModel,aes(x=Date,y=TP))+geom_line(data=subset(BaseModel,Station_ID=="HARPETH_62")) +theme_bw()+
  labs(y="TP (mg/L)")+ylim(min(BaseModel$TP),max(BaseModel$TP))+
  theme(
    axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Up","Frank_Eff","Franklin_Site_1")), aes(x=Date, y=TP, color=Station_ID))


HARPETH_62
```

![](HarpethTMDL_files/figure-gfm/Data%20Comparison-2.png)<!-- -->

``` r



ppi=300
#png("awesome stacked time series.png",width=12*ppi, height=8*ppi, res=ppi)
grid.arrange(LSPC188_tp,HARPETH_62, ncol=1)
```

![](HarpethTMDL_files/figure-gfm/Data%20Comparison-3.png)<!-- -->

``` r
#dev.off()
```
