HarpethTMDL
================
RJ
2022-07-13

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

time = lubridate::with_tz(Sys.time(), "CST6CDT")
```

This file was last updated on 2022-07-13 11:34:54

``` r
#Import Data

library(readxl)
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
```

`Grab` data table contains grab sample results collected from 171
locations throughout the Harpeth River watershed
