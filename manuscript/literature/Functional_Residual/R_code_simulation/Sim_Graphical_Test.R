source("./R_functions/ffplot.R")
source("./R_functions/fresiduals.R")
source("./R_functions/fresplot.R")
source("./R_functions/ffplot.envelop.R")
library(Rmisc)
library(plyr)
library(ggplot2)
library(VGAM)
library(pscl)
library(ggpointdensity)
library(MASS)
library(gridExtra)
library(gam)

set.seed(3)

########## Simulation for Overdispersion ##############

rpois.od<-function (n, lambda,d) {
  rnbinom(n, size=(lambda/(d-1)), mu=lambda)
}


n<-1000
px1<-rnorm(1000,0,1)
t1<-seq(0,1,0.02)
plinearp<-1.2+1.3*px1# for link function. linear predictor
f<-7
plambda<-exp(plinearp)

py<-rpois.od(n,plambda,f)
pdata<-cbind.data.frame(px1,py)
pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)
pmodel2<-glm(py~px1,family = quasipoisson)

pa<-ffplot.envelop(model=pmodel1, B=2000, title = "(a) Regular Poisson model")

pb<-ffplot.envelop(model=pmodel2, B=2000, title = "(b) Quasi-Poisson model")

########### Simulation for Zero-inflated #############

set.seed(3)
n<-1000
t1<-seq(0,1,0.02)
px1<-rnorm(1000,0,0.8)


plinearp<-1+1*px1# link only for link function. linear predictor
plambda<-exp(plinearp)
p0<-exp(1+0.2*px1)/(exp(1+0.2*px1)+1)
py<-rzipois(n,lambda=plambda,pstr0 = p0)
pdata<-cbind.data.frame(px1,py)

pmodel1z<-glm(py~px1,family = poisson(link=log),data = pdata)
pmodel2z<-zeroinfl(py~px1,data = pdata)

pc<-ffplot.envelop(model=pmodel1z, B=2000, title = "(c) Regular Poisson model")

pd<-ffplot.envelop(model=pmodel2z, B=2000, title = "(d) Zero-inflated Poisson model")

###################### Figure S23 Fn-Fn plots with test envelops, depicted by dotted curves surrounding the solid
###################### curve, in the presence of overdispersion and zero-inflation.#############################################

grid.arrange(pa,pb,pc,pd,nrow=2)