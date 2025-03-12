source("./R_functions/ffplot.R")
source("./R_functions/fresiduals.R")
source("./R_functions/fresplot.R")
library(Rmisc)
library(plyr)
library(ggplot2)
library(VGAM)
library(pscl)
library(ggpointdensity)
library(MASS)
library(gridExtra)
library(gam)

###################Count Data Examples##############################

##################################################################################
################Examples 5 and S1 (a) ############################################
##################################################################################
set.seed(3)
n<-1000
px1<-rnorm(1000,0,1)
px2<-px1^2
plinearp<-1+0.2*px1+0.15*px2# link only for link function. linear predictor
plambda<-exp(plinearp)
py<-c()
for (i in 1:length(px1)) {
  py[i]<-rpois(1,plambda[i])
  
}
pdata<-cbind.data.frame(px1,px2,py)
pmodel1<-glm(py~px1,family = "poisson",data = pdata)
pmodel2<-glm(py~px1+px2,family = "poisson",data = pdata)

#################### Functional residuals #############
fr1<-fresiduals(pmodel1)
fr2<-fresiduals(pmodel2)
#################### Deviance & Pearson residuals #############
deviance1<-resid(pmodel1,type="deviance")
pearson1<-resid(pmodel1,type="pearson")
deviance2<-resid(pmodel2,type="deviance")
pearson2<-resid(pmodel2,type="pearson")
d1<-cbind.data.frame(px1,deviance1)
d2<-cbind.data.frame(px1,deviance2)
p1<-cbind.data.frame(px1,pearson1)
p2<-cbind.data.frame(px1,pearson2)
#################### Figure S5 Plots of the functional residual and traditional residuals against the covariate########################

p2_unif<-fresplot(fr2,px1,title = "(a) Functional residuals on the uniform scale",scale = "uniform",
                  yl=0,yp=1,xl=-4,xp=4)
p2_norm<-fresplot(fr2,px1,title = "(b) Functional residuals on the normal scale",scale = "normal",
                  xl=-4,xp=4,yl=-3,yp=3)

p1_norm_quard<-fresplot(fr1,px1,title = "(a)",scale = "normal",
                        xl=-3,xp=3)

p1_deviance<-ggplot(d1, aes(x=px1, y=deviance1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+theme(plot.title = element_text(size=12),axis.title=element_text(size=12))+
  xlab("X")+ylab("")+labs(title="Deviance residuals")+ylim(-4,4)
p2_deviance<-ggplot(d2, aes(x=px1, y=deviance2)) + 
  geom_point()+theme(plot.title = element_text(size=12),axis.title=element_text(size=12))+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+labs(title="(c) Deviance Residuals")+ylim(-4,4)

p1_pearson<-ggplot(d1, aes(x=px1, y=pearson1)) + 
  geom_point()+theme(plot.title = element_text(size=12),axis.title=element_text(size=12))+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+labs(title="Pearson residuals")+ylim(-4,4)
p2_pearson<-ggplot(d2, aes(x=px1, y=pearson2)) + 
  geom_point()+theme(plot.title = element_text(size=12),axis.title=element_text(size=12))+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+labs(title="(d) Pearson residuals")+ylim(-4,4)

multiplot(p2_unif,p2_deviance,p2_norm,p2_pearson,cols=2)

#######################Figure S6 Fn-Fn plots when the working model is specified correctly########################
fullsampleff<-ffplot(fr2,title = "(a)")
lessthan0list<-which(px1<0)
lessthan0fr<-fr2[lessthan0list,]
lesssampleff<-ffplot(lessthan0fr,title = "(b)")
multiplot(fullsampleff,lesssampleff,cols=2)


##################################################################################
#####################Example S1 Missing of the higher order term#################
##################################################################################
set.seed(3)
px1<-rnorm(1000,0,0.5)
n <- 1000
px2<-px1^2
px3<-px1^3

plinearp<-0.8-0.2*px1+0.5*px2-0.5*px3# link only for link function. linear predictor

plambda<-exp(plinearp)

py<-c()
for (i in 1:length(px1)) {
  py[i]<-rpois(1,plambda[i])
}

summary(py)
pmodel1<-glm(py~px1+px2,family = "poisson")
pmodel2<-glm(py~px1+px2+px3,family = "poisson")
fittedy1<-pmodel1$fitted.values
fittedy2<-pmodel2$fitted.values

#################### Functional residuals #############
fr1q<-fresiduals(pmodel1)
p1_norm_cubic<-fresplot(fr1q,px1,title = "(b) ",scale = "normal",
                        xl=-1.5,xp=1.5)

############################################################################
###########Figure S18 (a) quadratic term is not included ######
###########Figure S18 (b) cubic term is not included ########################
############################################################################
multiplot(p1_norm_quard,p1_norm_cubic,cols=2)


#################################################################################
############    Example 6    zero inflation        ##############################
#################################################################################
set.seed(3)
n<-1000
px1<-rnorm(1000,0,0.8)

plinearp<-1+1*px1# only for link function. linear predictor
plambda<-exp(plinearp)
p0<-exp(1+0.2*px1)/(exp(1+0.2*px1)+1)
py<-rzipois(n,lambda=plambda,pstr0 = p0)
pdata<-cbind.data.frame(px1,py)

pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)#Regular model
pmodel2<-zeroinfl(py~px1,data = pdata)# Zero-inflated Poisson model

#################### Functional residuals #############

zffr1<-fresiduals(pmodel1)
zffr2<-fresiduals(pmodel2)

#######################Figure S7 Functional-residual-vs-covariate plots and Fn-Fn plots in the presence of excessive zeros.########################

p2_norm<-fresplot(zffr2,px1,title = "(b) Zero-Inflated Poisson model",scale = "normal",
                  xl=-2.5,xp=2.5)

p1_norm<-fresplot(zffr1,px1,title = "(a) Regular Poisson model",scale = "normal",
                  xl=-2.5,xp=2.5)

ff1<-ffplot(zffr1,title = "(c) Regular Poisson model")
ff2<-ffplot(zffr2,title = "(d) Zero-Inflated Poisson model")

multiplot(p1_norm,ff1,p2_norm,ff2,cols=2)



#################################################################
############  Example 7 Dispersed Poisson Model #################
#################################################################
set.seed(3)
n<-1000
px1<-rnorm(1000,0,1)
rpois.od<-function (n, lambda,d) {
  if (d==1)
    rpois(n, lambda)
  else
    rnbinom(n, size=(lambda/(d-1)), mu=lambda)
}
plinearp<-1.2+1.3*px1# link only for link function. linear predictor
f<-7
plambda<-exp(plinearp)
mean(plambda)
py<-rpois.od(n,plambda,f)
pdata<-cbind.data.frame(px1,py)
pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)
pmodel2<-glm(py~px1,family = quasipoisson)
#################### Functional residuals ##################
overfr1<-fresiduals(pmodel1)
overfr2<-fresiduals(pmodel2)
####################### Figure S8 Functional-residual-vs-covariate plots and Fn-Fn plots in the presence of over-dispersion.########################

p2_norm<-fresplot(overfr2,px1,title = "(b) Quasi-Poisson model",scale = "normal",
                  xl=-2.5,xp=2.8,yl=-3,yp=3)

p1_norm<-fresplot(overfr1,px1,title = "(a) Regular Poisson model",scale = "normal",
                  xl=-2.5,xp=2.8,yl=-3,yp=3)

ff1<-ffplot(overfr1,title = "(c) Regular Poisson model")
ff2<-ffplot(overfr2,title = "(d) Quasi-Poisson model")
grid.arrange(p1_norm,p2_norm,ff1,ff2,nrow=2)


################################################################# 
############# Example 8 Semi-parametric Poisson model############
################################################################# 
set.seed(3)
n<-1000
px1<-rnorm(1000,0,1)

rpois.od<-function (n, lambda,d) {
  if (d==1)
    rpois(n, lambda)
  else
    rnbinom(n, size=(lambda/(d-1)), mu=lambda)
}
plinearp<-1.2+1.3*sin(px1)-0.8*px1# link only for link function. linear predictor
f<-7 # over dispersion parameter

plambda<-exp(plinearp)
mean(plambda)
py<-rpois.od(n,plambda,f)
pdata<-cbind.data.frame(px1,py)

pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)
# regular Poisson model
pmodel2<-gam(py~s(px1),family = poisson(link=log),data = pdata)
# Generalized additive Poisson model
pmodel3_gam<-gam(py~s(px1),family = quasipoisson,data = pdata)
# Generalized additive Quasi-Poisson model
gamfr1<-fresiduals(pmodel1)
gamfr2<-fresiduals(pmodel2)
gamfr3<-fresiduals(pmodel3_gam)


####################### Figure S9 Functional-residual-vs-covariate plots ((a)-(c)) and Fn-Fn plots ((d)-(f)) for
####################### building a semiparametric model for count data with a sine effect of the predictor ##################

p2_norm<-fresplot(gamfr2,px1,title = "(b) Generalized additive Poisson model",scale = "normal",
                  xl=-2.5,xp=3,yl=-3,yp=3,xlabs = "")
  

p1_norm<-fresplot(gamfr1,px1,title = "(a) Regular Poisson model",scale = "normal",
                  xl=-2.5,xp=3,yl=-3,yp=3,xlabs = "")

p3_norm<-fresplot(gamfr3,px1,title = "(c) Generalized additive quasi-Poisson model",scale = "normal",
                  xl=-2.5,xp=3,yl=-3,yp=3,xlabs = "")

ff1<-ffplot(gamfr1,title = "(d) Regular Poisson model")
ff2<-ffplot(gamfr2,title = "(e) Generalized additive Poisson model")
ff3<-ffplot(gamfr3,title = "(f) Generalized additive quasi-Poisson model")

grid.arrange(p1_norm,p2_norm,p3_norm,ff1,ff2,ff3,nrow=2)


##################################################################################
######################Example S2 Missing of a covariate############################
##################################################################################
set.seed(3)
n<-1000
px1<-rnorm(1000,0,0.8)

px2<-rnorm(1000,-1,1)
px3<-rnorm(1000,0.8,0.9)
plinearp<-0.5+0.25*px1+0.5*px2# link only for link function. linear predictor
plambda<-exp(plinearp)

py<-c()
for (i in 1:length(px1)) {
  py[i]<-rpois(1,plambda[i])
}


pmodel1<-glm(py~px1,family = "poisson")
pmodel2<-glm(py~px1+px2,family = "poisson")
#################### Functional residuals ##################
fr1<-fresiduals(pmodel1)
fr2<-fresiduals(pmodel2)

##################### Figure S18 Functional-residual-vs-covariate plots when a covariate X2 is correlated
##################### with the count response Y#########################

p2_norm<-fresplot(fr2,px3,
                  title = "(b)",
                  scale = "normal",
                  xl=-2,xp=4,xlabs = expression(X[3]))

p1_norm<-fresplot(fr1,px2,
                  title = "(a)",
                  scale = "normal",
                  xl=-4,xp=2,xlabs = expression(X[2]))
multiplot(p1_norm,p2_norm,cols = 2)


##################################################################################
######################Example S3 Missing of the interaction term##################
##################################################################################
set.seed(33)
n<-1000
px1<-rnorm(n,0.5,1)

px2<-rnorm(n,-1,0.7)
px3<-px1*px2
plinearp<--0.1+0.8*px1-0.5*px2+0.6*px3# link only for link function. linear predictor
plambda<-exp(plinearp)
summary(plambda)
py<-c()
for (i in 1:length(px1)) {
  py[i]<-rpois(1,plambda[i])
}

pmodel1<-glm(py~px1+px2,family = "poisson")
pmodel2<-glm(py~px1+px2+px3,family = "poisson")

#################### Functional residuals ##################
fr1<-fresiduals(pmodel1)
fr2<-fresiduals(pmodel2)

##################### Figure S19 Functional-residual-vs-covariate plots before and after the 
##################### interaction term is included in the Poisson model for the count data#########################

p2_norm<-fresplot(fr2,px3,
                  title = "(b)",
                  scale = "normal",
                  xl=-4,xp=4,xlabs = expression(X[1]*X[2]))

p1_norm<-fresplot(fr1,px3,
                  title = "(a)",
                  scale = "normal",
                  xl=-4,xp=4,xlabs = expression(X[1]*X[2]))
multiplot(p1_norm,p2_norm,cols=2)