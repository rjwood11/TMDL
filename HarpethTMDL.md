HarpethTMDL
================
RJ
2022-07-15

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
library(forecast)

time = lubridate::with_tz(Sys.time(), "CST6CDT")
```

This file was last updated on 2022-07-15 14:53:11

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



Natural <- read_delim("NATURAL_MODEL_2011_To_2020.txt", 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(`Date-Time` = col_datetime(format = "%m/%d/%Y %H:%M"), 
        CBOD90 = col_number(), DIP = col_number(), 
        DO = col_number(), DON = col_number(), 
        DOP = col_number(), FLOW_CMS = col_number(), 
        NH3_N = col_number(), NO3O2 = col_number(), 
        PHYTO = col_number(), PON = col_number(), 
        SOD_T = col_number(), TKN = col_number(), 
        TN = col_number(), TP = col_number(), 
        WTEMP = col_number()), trim_ws = TRUE)

head(Natural,15)
## # A tibble: 15 × 17
##    `Date-Time`         Station_ID   CBOD90    DIP    DO    DON    DOP FLOW_CMS
##    <dttm>              <chr>         <dbl>  <dbl> <dbl>  <dbl>  <dbl>    <dbl>
##  1 2011-01-01 00:00:00 ARKANSAS_131   0    0       0    0      0         0.210
##  2 2011-01-02 00:06:00 ARKANSAS_131   2.38 0.0968  7.18 0.0193 0.0107    0.208
##  3 2011-01-03 00:04:00 ARKANSAS_131   2.38 0.0948 10.7  0.0193 0.0107    0.208
##  4 2011-01-04 00:02:00 ARKANSAS_131   2.38 0.0949 10.6  0.0193 0.0107    0.208
##  5 2011-01-04 23:59:00 ARKANSAS_131   2.38 0.0951 10.6  0.0193 0.0107    0.208
##  6 2011-01-06 00:05:00 ARKANSAS_131   2.38 0.0953 10.6  0.0193 0.0107    0.208
##  7 2011-01-07 00:02:00 ARKANSAS_131   2.38 0.0954 10.6  0.0193 0.0107    0.208
##  8 2011-01-08 00:00:00 ARKANSAS_131   2.38 0.0956 10.6  0.0193 0.0107    0.208
##  9 2011-01-09 00:06:00 ARKANSAS_131   2.38 0.0957 10.6  0.0193 0.0107    0.208
## 10 2011-01-10 00:03:00 ARKANSAS_131   2.38 0.0958 10.6  0.0193 0.0107    0.208
## 11 2011-01-11 00:00:00 ARKANSAS_131   2.38 0.0960 10.6  0.0193 0.0107    0.208
## 12 2011-01-12 00:06:00 ARKANSAS_131   2.38 0.0961 10.6  0.0193 0.0107    0.208
## 13 2011-01-13 00:04:00 ARKANSAS_131   2.38 0.0962 10.6  0.0193 0.0107    0.208
## 14 2011-01-14 00:01:00 ARKANSAS_131   2.38 0.0963 10.6  0.0193 0.0107    0.208
## 15 2011-01-14 23:58:00 ARKANSAS_131   2.38 0.0964 10.6  0.0193 0.0107    0.208
## # … with 9 more variables: NH3_N <dbl>, NO3O2 <dbl>, PHYTO <dbl>, PON <dbl>,
## #   SOD_T <dbl>, TKN <dbl>, TN <dbl>, TP <dbl>, WTEMP <dbl>



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

colnames(Natural)[1] <- "Date"

BaseModel<-BaseModel[BaseModel$Station_ID==sites,]


BaseModel$TPLoad<-BaseModel$TP*0.00220462*BaseModel$FLOW_CMS*86400

BaseModel$TPLoad.csum <- ave(BaseModel$TPLoad, BaseModel$Station_ID, FUN=cumsum)

BaseModel$NP<-BaseModel$TN/BaseModel$TP

#####################################################################################################

molten.data <- melt(Grab, id = c("Date","Station_ID","Pcode","Units","DataSource"))

Grab.w<-dcast(molten.data, Date+Station_ID~Pcode, mean)

Grab.w$NP<-Grab.w$TN/Grab.w$TP

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
##    TEMPWC   TKN  TN    TP  TS TSS TURB  NP
## 1    0.40   NaN NaN   NaN NaN NaN  NaN NaN
## 2    4.33 0.065 NaN 0.006 NaN   5 0.68 NaN
## 3    6.16 0.065 NaN 0.210 NaN   5 3.07 NaN
## 4    2.61 0.065 NaN 0.013 NaN   5 0.32 NaN
## 5    6.36 0.200 NaN 0.210 NaN   5 2.81 NaN
## 6    6.19 0.065 NaN 0.044 NaN   5 0.74 NaN
## 7    3.39 0.065 NaN 0.082 NaN   5 1.75 NaN
## 8    4.62 0.065 NaN 0.230 NaN   5 1.36 NaN
## 9    4.60 0.065 NaN 0.210 NaN   5 0.52 NaN
## 10   5.46 0.200 NaN 0.033 NaN   5 0.88 NaN



molten.cont <- melt(Cont, id = c("Date","Station_ID","Pcode","Units"))

Cont.w<-dcast(molten.cont, Date+Station_ID~Pcode, mean)

head(Cont.w, 10)
##          Date    Station_ID COND  DO DOSAT FLOW_CFS    FLOW_CMS  PH TEMPWC TURB
## 1  2011-01-01 USGS_03432350  NaN NaN   NaN   828.00 23.44634932 NaN    NaN  NaN
## 2  2011-01-01 USGS_03432400  NaN NaN   NaN   919.00 26.02318239 NaN    NaN  NaN
## 3  2011-01-01 USGS_03433500  NaN NaN   NaN   456.00 12.91248223 NaN    NaN  NaN
## 4  2011-01-01 USGS_03433640  NaN NaN   NaN     3.83  0.10845352 NaN    NaN  NaN
## 5  2011-01-01 USGS_03434500  NaN NaN   NaN   464.00 13.13901701 NaN    NaN  NaN
## 6  2011-01-02 USGS_03432350  NaN NaN   NaN  1020.00 28.88318394 NaN    NaN  NaN
## 7  2011-01-02 USGS_03432400  NaN NaN   NaN  1330.00 37.66140651 NaN    NaN  NaN
## 8  2011-01-02 USGS_03433500  NaN NaN   NaN  2220.00 62.86340034 NaN    NaN  NaN
## 9  2011-01-02 USGS_03433640  NaN NaN   NaN     1.44  0.04077626 NaN    NaN  NaN
## 10 2011-01-02 USGS_03434500  NaN NaN   NaN  2130.00 60.31488411 NaN    NaN  NaN
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



tp<-ggplot(BaseModel, aes(x = Station_ID, y = TP, fill = Station_ID)) + 
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

np<-ggplot(BaseModel, aes(x = Station_ID, y = NP, fill = Station_ID)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("TN:TP Ratio")

np
```

![](HarpethTMDL_files/figure-gfm/Basic%20Plots-5.png)<!-- -->

``` r
LSPC188_tp<-ggplot(BaseModel,aes(x=Date,y=TP, color="Base Model"))+geom_line(data=subset(BaseModel,Station_ID=="LSPC188PERO")) +theme_bw()+
  labs(y="TP (mg/L)")+ylim(min(BaseModel$TP),max(BaseModel$TP))+
  theme(
    axis.text.y=element_text(size=11),axis.title.y=element_text(size=12, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Down","Frank_CottonLn","Franklin_Site_2","Franklin_Site_3")), aes(x=Date, y=TP, color=Station_ID))+
  geom_line(data=subset(Natural, Station_ID=="LSPC188PERO"), aes(x=Date, y=TP, color="Natural"))

#LSPC188_tp

LSPC188_tn<-ggplot(BaseModel,aes(x=Date,y=TN))+geom_line(data=subset(BaseModel,Station_ID=="LSPC188PERO")) +theme_bw()+
  labs(y="TN (mg/L)")+ylim(min(BaseModel$TN),max(BaseModel$TN))+
  theme(
    axis.text.y=element_text(size=11),axis.title.y=element_text(size=12, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Down","Frank_CottonLn","Franklin_Site_2","Franklin_Site_3")), aes(x=Date, y=TN, color=Station_ID))

#LSPC188_tn

LSPC188_flow<-ggplot(BaseModel,aes(x=Date,y=FLOW_CMS))+geom_line(data=subset(BaseModel,Station_ID=="LSPC188PERO")) +theme_bw()+
  labs(y="Flow (cms)")+ylim(min(BaseModel$FLOW_CMS),200)+
  theme(
    axis.text.y=element_text(size=11),axis.title.y=element_text(size=12, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Cont.w, Station_ID==c("USGS_03432400")), aes(x=Date, y=FLOW_CMS, color=Station_ID))

#LSPC188_flow



LSPC188_np<-ggplot(BaseModel,aes(x=Date,y=NP))+geom_line(data=subset(BaseModel,Station_ID=="LSPC188PERO")) +theme_bw()+
  labs(y="TN:TP ratio", x="Date")+ylim(min(BaseModel$NP),max(BaseModel$NP))+
  theme(
    axis.text.y=element_text(size=11),axis.title.y=element_text(size=12, vjust=1.2),
    axis.text.x=element_text(size=11),axis.title.x=element_text(size=11), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Down","Frank_CottonLn","Franklin_Site_2","Franklin_Site_3")), aes(x=Date, y=NP, color=Station_ID))

#LSPC188_np

grid.arrange(LSPC188_tp,LSPC188_tn,LSPC188_flow,LSPC188_np, ncol=1,padding=100)
```

![](HarpethTMDL_files/figure-gfm/Data%20Comparison-1.png)<!-- -->

``` r

HARPETH_62<-ggplot(BaseModel,aes(x=Date,y=TP))+geom_line(data=subset(BaseModel,Station_ID=="HARPETH_62")) +theme_bw()+
  labs(y="TP (mg/L)")+ylim(min(BaseModel$TP),max(BaseModel$TP))+
  theme(
    axis.text.y=element_text(size=16),axis.title.y=element_text(size=12, vjust=1.2),
    axis.text.x=element_blank(),axis.title.x=element_blank(), 
    panel.border = element_rect(size=.8, colour = "black"))+
  geom_point(data=subset(Grab.w, Station_ID==c("Frank_Up","Frank_Eff","Franklin_Site_1")), aes(x=Date, y=TP, color=Station_ID))


#HARPETH_62







#ppi=300
#png("awesome stacked time series.png",width=12*ppi, height=8*ppi, res=ppi)
#grid.arrange(LSPC188_tp,HARPETH_62, ncol=1)
#dev.off()
```

``` r

monthOrder <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
BaseModel$Month <- factor(format(BaseModel$Date, "%b"), levels = monthOrder)
BaseModel$Year <- factor(format(BaseModel$Date, "%Y"))
ggplot(BaseModel, aes(Month, TPLoad)) + geom_boxplot(data=subset(BaseModel,Station_ID=="LSPC188PERO")) + stat_boxplot(geom ='errorbar') + ggtitle("TP Load (lbs/day)")
```

![](HarpethTMDL_files/figure-gfm/Seasonal%20Trends-1.png)<!-- -->

``` r
ggplot(BaseModel,aes(Month,TPLoad)) + geom_bar(stat="identity") + ggtitle("TP Load (lbs/month)")
```

![](HarpethTMDL_files/figure-gfm/Seasonal%20Trends-2.png)<!-- -->

``` r
##################

#LSPC188_tpload <- subset(BaseModel, Station_ID==c("LSPC188PERO","HARPETH_62"), select=c(Station_ID, Year, Month, TPLoad))
#meanTPLoad_62 <- subset(BaseModel, Station_ID=="HARPETH_62", select=c(Station_ID, Year, Month, TPLoad))
#meanTPLoad_188 <- subset(BaseModel, Station_ID=="LSPC188PERO", select=c(Station_ID, Year, Month, TPLoad))

#library(plyr)
#tpload62<-ddply(meanTPLoad_62, c("Month","Station_ID"), summarise, x = mean(TPLoad))

#meanTPLoad_62 <- join(meanTPLoad_62, tpload62, match="all")






#ggplot(LSPC188_tpload,aes(Year,TPLoad,colour=Station_ID)) +
#geom_point(data=LSPC188_tpload,size=I(2),alpha=I(0.6)) + 
#geom_line(data=meanTPLoad_62, aes(Month,x), size=I(1.5),alpha=I(0.6)) + 
##geom_line(data=mean(meanTPLoad_62$TPLoad),size=I(1.5),alpha=I(0.4)) + 
#theme_grey(base_size=15) +
#theme(legend.title = element_blank(), legend.position=c(.85,.85), axis.title.y=element_blank(),axis.text.x=element_blank()) + 
#ggtitle("TP Load (lbs/day)") + facet_grid(. ~ Month) + 
#xlab(paste("Years: 2011 to 2019"))













#theme_set(theme_classic())

#BaseModel$Date.ts<-as.ts(BaseModel$Date)

# Subset data
#Base_LSPC188 <- window(subset(BaseModel,Station_ID=="LSPC188PERO"), start=c(2012, 1), end=c(2019, 8))  # subset a smaller timewindow

# Plot
#ggseasonplot(subset(BaseModel,Station_ID=="LSPC188PERO"), x=BaseModel$Date.ts) + labs(title="Seasonal plot: International Airline Passengers")
#ggseasonplot(subset(BaseModel,Station_ID=="LSPC188PERO")) + labs(title="Seasonal plot: Air temperatures at Nottingham Castle")
```
