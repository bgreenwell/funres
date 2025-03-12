library(Rmisc)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(tseries)
library(plyr)
library(VGAM)
library(pscl)
library(ggpointdensity)
library(MASS)
library(gridExtra)
library(mgcv)
library(tidyverse)
library(forecast)
library(tsibble)
library(tscount)



set.seed(3)
bikedata<-read.csv("./Datasets/hour.csv")
source("./R_functions/ffplot.R")
source("./R_functions/fresiduals.R")
source("./R_functions/fresplot.R")

### Our analysis focuses on the 2012 data (Selecting the 2012 data).
bikedata<-bikedata %>% 
  filter(yr==1)


bikedata$dteday <- as.Date(bikedata$dteday, format = "%Y-%m-%d")
bikedata$winter<-ifelse(bikedata$season==1,1,0)
### Create a new variable called "winter" as our analysis 
### shows the winter season is different from other seasons

bikedata$dteday<-as.Date(bikedata$dteday)
bikedata$hr<-as.numeric(bikedata$hr)
bikedata$hour<-bikedata$hr
bikedata <- bikedata %>% mutate(datetime=
                                  as.POSIXct(as.character(paste(bikedata$dteday, bikedata$hr)), 
                                                    format="%Y-%m-%d %H"))



##################################################################################
########################## INGARCH models ######################################
##################################################################################

xpart<-bikedata %>% dplyr::select(weekday,workingday,weathersit,temp,
                           windspeed,hum,winter)

modeltsnb1<-tsglm(bikedata$cnt,distr="nbinom",model=list(past_obs=1),
               xreg = xpart,link = "log")# ingarch 1,0

modeltsnb2<-tsglm(bikedata$cnt,distr="nbinom",model=list(past_obs=2),
                  xreg = xpart,link = "log")# ingarch 2,0
modeltsnb3<-tsglm(bikedata$cnt,distr="nbinom",model=list(past_obs=3),
                  xreg = xpart,link = "log")# ingarch 3,0
modeltsnb33<-tsglm(bikedata$cnt,distr="nbinom",
                   model=list(past_obs=3,past_mean=3),
                  xreg = xpart)# ingarch 3,3

fres10<-fresiduals(modeltsnb1)
fres20<-fresiduals(modeltsnb2)
fres30<-fresiduals(modeltsnb3)
fres33<-fresiduals(modeltsnb33)


########################Figure S20 Functional-residual-vs-hour plots using different INGARCH models#####################################


heatmaptsnb1_hour<-fresplot(fres10,bikedata$hr,title = "(a) INGARCH(1,0)",
                            xl=0,xp=24,heatmapcut = 11,scale = "normal",xlabs = "hour")
  
heatmaptsnb2_hour<-fresplot(fres20,bikedata$hr,title = "(b) INGARCH(2,0)",
                            xl=0,xp=24,heatmapcut = 11,scale = "normal",xlabs = "hour")

heatmaptsnb3_hour<-fresplot(fres30,bikedata$hr,title = "(c) INGARCH(3,0)",
                            xl=0,xp=24,heatmapcut = 11,scale = "normal",xlabs = "hour")

heatmaptsnb33_hour<-fresplot(fres33,bikedata$hr,title = "(c) INGARCH(3,3)",
                             xl=0,xp=24,heatmapcut = 11,scale = "normal",xlabs = "hour")


grid.arrange(heatmaptsnb1_hour,heatmaptsnb2_hour,
             heatmaptsnb3_hour,heatmaptsnb33_hour,ncol=2)









