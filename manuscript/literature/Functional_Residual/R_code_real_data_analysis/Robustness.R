library(Rmisc)
library(plyr)
library(ggplot2)
library(VGAM)
library(pscl)
library(ggpointdensity)
library(MASS)
library(dplyr)
library(gridExtra)
library(mgcv)
source("./R_functions/ffplot.R")
source("./R_functions/fresiduals.R")
source("./R_functions/fresplot.R")

#When a much smaller subset (n = 250) is randomly selected from each of the original data sets. 
#The same analysis is repeated.


set.seed(6)


whitewine<-read.csv("./Datasets/winequality-white.csv",sep = ";")
subsample<-sample(c(1:nrow(whitewine)),size = 250,replace = F)
whitewine<-whitewine[subsample,]


model1ww<- vglm(quality~volatile.acidity+
                alcohol+sulphates+fixed.acidity+
                residual.sugar+free.sulfur.dioxide+
                pH+density,
              family=acat(reverse=TRUE, parallel=TRUE),data =whitewine)

fr1_sub_ww<-fresiduals(model1ww)#function residuals

#######Function residuals-vs-covariate plot


heatmap6_norm<-fresplot(fr1_sub_ww,whitewine$free.sulfur.dioxide,
                        scale ="normal",
                        xl=0,xp=150,heatmapcut = 11,yl=-3,yp=3,
                        title = "(a) free.sulfur.dioxide",xlabs = "")
  

bikedata<-read.csv("./Datasets/hour.csv")


bikedata<-bikedata %>% 
  filter(yr==1)

bikedata$winter<-ifelse(bikedata$season==1,1,0)

subsample<-sample(c(1:nrow(bikedata)),size = 250,replace = F)

bikedata<-bikedata[subsample,]

model1bike<-glm(cnt~hr+workingday+weathersit+temp+hum+windspeed+winter,family = "poisson",data = bikedata)



fr_sub_bike<-fresiduals(model1bike)### Functional residuals




### Function residuals-vs-hr plot
heatmap2_norm<-fresplot(fr_sub_bike,bikedata$hr,
                        scale ="normal",
                        xl=0,xp=24,heatmapcut = 11,yl=-20,yp=20,
                        title = "(b) hour",xlabs = "")


### Generalized additive Poisson model


model_gam<-gam(cnt~winter+s(hr)+workingday+weathersit+
                 s(temp)+s(hum)+s(windspeed),
               family = poisson,
               data = bikedata)


summary(model_gam)



fr_sub_gam_bike<-fresiduals(model_gam)### Functional residuals of generalized additive Poisson model


########### Fn-Fn plot for the generalized additive Poisson model

ff<-ffplot(fr_sub_gam_bike,title = "(c) Fn-Fn plot")

#####Generalized additive quasi-Poisson model


model_gam_quasi<-gam(cnt~winter+s(hr)+workingday+weathersit+
                       s(temp)+s(hum)+s(windspeed),
                     family = quasipoisson,
                     data = bikedata)
summary(model_gam_quasi)

fr_sub_gam_quasi<-fresiduals(model_gam_quasi)# Functional residuals of generalized additive quasi-Poisson model


# Functional residuals vs hr plot for the generalized additive quasi-Poisson model
heatmap2_norm_gam_quasi<-fresplot(fr_sub_gam_quasi,bikedata$hr,
                                  scale ="normal",
                                  xl=0,xp=24,heatmapcut = 11,yl=-3,yp=3,
                                  title = "(d) hour",xlabs = "")

################################################
## Figure S24 Model diagnostics for the two case studies 
## when a much smaller subset (n = 250)
## is randomly selected from each of the original data sets.
################################################
grid.arrange(heatmap6_norm,heatmap2_norm,
             ff,heatmap2_norm_gam_quasi,nrow=2)
