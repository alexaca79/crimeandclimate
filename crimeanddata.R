#Climate and Crime Analysis

## Research Question: 
### What is the temperature in which crime explodes 
### Looking at Toronto 

setwd("https://intra.sse.gov.on.ca/MCSCS/raib/au/Shared Documents/Crimeandclimate")

### Libraries 
library(tidyverse)
library(caret)
library(rcurl)
library(rclimateca)
library(jsonlite)
library(lubridate)
library(forecast)
library(imputeTS)
library(aTSA)


###Retrieve Climate Data from Statscan using Toronto International Airport Climate Station 
climate<-ec_climate_data(
  "TORONTO INTL A ON 51459", timeframe = "daily",
  start = "2014-01-01", end = "2017-12-31"
)

### Interpolation max temp (there is one NA)
climate$max_temp_c_nascrup <- na.interpolation(climate$max_temp_c, option = "linear")

###creating TS of Max Temp 
climatemax <- ts(climate$max_temp_c_nascrup, start=c(2014, 1,1), frequency = 365)

###Retrieve Crime Data from TPS 
crime<- fromJSON("https://opendata.arcgis.com/datasets/d38f7c2079b84c849b31f506020faaaa_0.geojson")
crimedf<- crime[["features"]][["properties"]]


###Group by Date 
crimedfbydate <- crimedf %>%
                        group_by(occurrencedate) %>%
                        subset(occurrencedate > "2014-01-01") %>%
                        count(occurrencedate)

# crimedfbydate$nint <- na.interpolation(crimedfbydate$nint, option = "linear")
crimedfbydatets <-ts(crimedfbydate$n, start=c(2014, 1,1), frequency = 365)

#plot crime by max temperature 
crimeplot<- ggplot(crimedfbydate,aes(x=occurrencedate))+
            geom_point(aes(y=n),colour="red")+
            geom_point(aes(y=climate$max_temp_c),colour="blue") 

#Decompose
decomposeclimate <-decompose(climatemax)
decomposecrime <- decompose(crimedfbydatets) 

#lag 
climatemaxlag <-  diff(climatemax, lag = 1, differences = 1)
crimedfbydatetslag <- diff(crimedfbydatets, lag = 1, differences = 1)

#correlation Test 
cor.test(climatemax,crimedfbydatets)
adf.test(crimedfbydatets)

##Plot 
crimtemplot<- ggplot(data=climate, aes(x=max_temp_c,y=crimedfbydate$n))+
              geom_point(colour="red")+
              geom_smooth(method = "lm")
crimtemplot
