###################adjacent-category model examples 1-4###########################
source("./R_functions/ffplot.R")
source("./R_functions/fresiduals.R")
source("./R_functions/fresplot.R")
library(VGAM)
library(brglm2)
library(ggplot2)
library(gridExtra)
library(np)
library(MASS)
library(PAsso)
##################################################################################
################example 1 & 2 Missing of the quadratic term#######################
##################################################################################


set.seed(3)


n<-1000
x1<-rnorm(1000,0,1)
x2<-x1^2

# linear preictor
linearp1<-1.5*x1-x2
linearp2<-1.5+1.5*x1-x2
linearp3<--1+1.5*x1-x2
linearp4<-1+1.5*x1-x2

p1<-1/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p2<-exp(-linearp1)/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p3<-exp(-(linearp1+linearp2))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p4<-exp(-(linearp1+linearp2+linearp3))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p5<-exp(-(linearp1+linearp2+linearp3+linearp4))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
pr<-cbind(p1,p2,p3,p4,p5)

y<-c()
for (i in 1:length(x1)) {
  y[i] <- sample(c(1,2,3,4,5), 1, replace=TRUE, prob=pr[i,]) 
}
testdata<-cbind.data.frame(x1,x2,y)

# model 1 is missing the x2.
# model 2 is specified correctly.

model1<- vglm(testdata$y~testdata$x1,
              family=acat(reverse=TRUE, parallel=TRUE))
proby1<-fitted(model1)
model2<- vglm(testdata$y~testdata$x1 + testdata$x2,
              family=acat(reverse=TRUE, parallel=TRUE))
proby2<-fitted(model2)

################Functional residuals#################
fr1<-fresiduals(model1)
fr2<-fresiduals(model2)



################Probability-scale residuals##################
sign1<-rep(0,length(y))
sign2<-rep(0,length(y))
P_sy1<-c()
P_sy2<-c()
P_gy1<-c()
P_gy2<-c()
i<-1
j<-1
y<-as.numeric(y)
for(i in 1: length(x1)){
  if(y[i]!=1 && y[i]!=5){
    
    P_sy1[i]<- sum(proby1[i,1:(y[i]-1)])
    P_sy2[i]<- sum(proby2[i,1:(y[i]-1)])
    P_gy1[i]<- sum(proby1[i,(y[i]+1):5])
    P_gy2[i]<-sum(proby2[i,(y[i]+1):5])
  }else{
    if(y[i]==5){
      P_gy1[i]<- 0
      P_sy1[i]<- 1-proby1[i,5]
      
      P_gy2[i]<- 0
      P_sy2[i]<- 1-proby2[i,5]
    }
    if(y[i]==1){
      P_sy1[i]<- 0
      P_gy1[i]<- 1-proby1[i,1]
      P_sy2[i]<- 0
      P_gy2[i]<- 1-proby2[i,1]
    }
  }
  sign1[i]<- P_gy1[i]-P_sy1[i]
  sign2[i]<- P_gy2[i]-P_sy2[i]
}



################Generalized residuals##################
cum.prob1<-matrix(NA,nrow = nrow(proby1),ncol = ncol(proby1)+1)
cum.prob2<-matrix(NA,nrow = nrow(proby2),ncol = ncol(proby2)+1)
cum.prob1[,1]<-rep(0,nrow(proby1))
cum.prob2[,1]<-rep(0,nrow(proby1))
for (i in 2:6) {
  cum.prob1[,i]<-cum.prob1[,i-1]+proby1[,i-1]
  cum.prob2[,i]<-cum.prob2[,i-1]+proby2[,i-1]
}


Pj1<- sapply(1:n, function(x) proby1[x,y[x]])
Fj1<- sapply(1:n, function(x) cum.prob1[x,y[x]+1])
Fj1_1<- sapply(1:n, function(x) cum.prob1[x,y[x]])
fj1<- dnorm(qnorm(Fj1))
fj1_1<- dnorm(qnorm(Fj1_1))
g1<-(fj1_1-fj1)/Pj1

Pj2<- sapply(1:n, function(x) proby2[x,y[x]])
Fj2<- sapply(1:n, function(x) cum.prob2[x,y[x]+1])
Fj2_1<- sapply(1:n, function(x) cum.prob2[x,y[x]])
fj2<- dnorm(qnorm(Fj2))
fj2_1<- dnorm(qnorm(Fj2_1))
g2<-(fj2_1-fj2)/Pj2
################Deviance & Pearson residuals#########
deviance1<-resid(model1)[,1]
pearson1<-resid(model1,type="pearson")[,1]
deviance2<-resid(model2)[,1]
pearson2<-resid(model2,type="pearson")[,1]


################ Figure 4: Functional-residual-vs-covariate plots and existing residual-vs-covariate plots 
################ when the working model is specified correctly
################ S1: Functional-residual-vs-covariate plots and other residual-vs-covariate plots 
################ when the quadratic term is missing ####################
otherres1<-cbind.data.frame(x1,sign1,deviance1,pearson1,g1)
otherres2<-cbind.data.frame(x1,sign2,deviance2,pearson2,g2)


########### Figure 4, 5, and S1########
fresplot1_unif <- fresplot(fr1,
                           x1,
                           title="(a) Functional residuals on the uniform scale",
                           scale="uniform",xl = -3, xp = 3,yl = 0, yp = 1)

fresplot2_unif <- fresplot(fr2,
                           x1,
                           title="(a) Functional residuals on the uniform scale",
                           scale="uniform",xl = -2, xp = 2,yl = 0, yp = 1)

fresplot1_norm <- fresplot(fr1,
                           x1,
                           title="(b) Functional residuals on the normal scale",
                           scale="normal",xl = -2, xp = 2)

fresplot2_norm <- fresplot(fr2,
                           x1,
                           title="(b) Functional residuals on the normal scale",
                           scale="normal",xl = -2, xp = 2)

p2_deviance<-ggplot(otherres2, aes(x=x1, y=deviance2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-25,25)+
  labs(title = "(c) Deviance residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_pearson<-ggplot(otherres2, aes(x=x1, y=pearson2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(d) Pearson residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_deviance<-ggplot(otherres1, aes(x=x1, y=deviance1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-15,15)+
  labs(title = "(c) Deviance residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_pearson<-ggplot(otherres1, aes(x=x1, y=pearson1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(d) Pearson residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_sign<-ggplot(otherres1, aes(x=x1, y=sign1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(e) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_sign<-ggplot(otherres2, aes(x=x1, y=sign2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(e) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_g<-ggplot(otherres1, aes(x=x1, y=g1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(f) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_g<-ggplot(otherres2, aes(x=x1, y=g2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab("X")+ylab("")+ylim(-5,5)+
  labs(title = "(f) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

# Figure S1 
grid.arrange(fresplot1_unif,fresplot1_norm,p1_deviance,p1_pearson,p1_sign,p1_g,ncol=2)
# Figure 4 
grid.arrange(fresplot2_unif,fresplot2_norm,p2_deviance,p2_pearson,p2_sign,p2_g,ncol=2)

################ Figure 5 
ff1 <- ffplot(fr2,title="(a)") #full sample
rangelist<-which(x1<0)
fr2sub <- fr2[rangelist,] 
ff2 <- ffplot(fr2sub,title="(b)") 
grid.arrange(ff1,ff2,ncol=2)




##################################################################################
################example 2 Missing of the cubic term###############################
##################################################################################
set.seed(3)
x1<-rnorm(1000,0,1)
x2<-x1^2
x3<-x1^3
n<-1000
linearp1<--1+2*x1-x2-1.5*x3# link only for link function. linear predictor
linearp2<-1.5+2*x1-x2-1.5*x3
linearp3<-2+2*x1-x2-1.5*x3
linearp4<-3+2*x1-x2-1.5*x3
p1<-1/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p2<-exp(-linearp1)/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p3<-exp(-(linearp1+linearp2))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p4<-exp(-(linearp1+linearp2+linearp3))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p5<-exp(-(linearp1+linearp2+linearp3+linearp4))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
pr<-cbind(p1,p2,p3,p4,p5)

y<-c()

for (i in 1:length(x1)) {
  y[i] <- sample(c(1,2,3,4,5), 1, replace=TRUE, prob=pr[i,]) 
}
model1<- vglm(y~x1+x2,
              family=acat(reverse=TRUE, parallel=TRUE))
proby1<-fitted(model1)
############ Figure S2 Functional-residual-vs-covariate plot when the cubic term is missing ##########
fr1 <- fresiduals(model1)
p1_norm <- fresplot(fr1,
                    x1,
                    title=" ",
                    scale="normal",xl = -2, xp = 2)


##################################################################################
################example 3 Missing of the covariate term###########################
##################################################################################
n<-1000
set.seed(5)
x1<-rnorm(n,0,1)
x2<-rnorm(n,-1,0.8)
beta<-c(1.5,1,0)
x3<-rnorm(n,0.5,1)
linearp1<--1+beta[1]*x1+beta[2]*x2# link only for link function. linear predictor
linearp2<--2+beta[1]*x1+beta[2]*x2
linearp3<-0.5+beta[1]*x1+beta[2]*x2
linearp4<-2+beta[1]*x1+beta[2]*x2
p1<-1/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p2<-exp(-linearp1)/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p3<-exp(-(linearp1+linearp2))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p4<-exp(-(linearp1+linearp2+linearp3))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p5<-exp(-(linearp1+linearp2+linearp3+linearp4))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
pr<-cbind(p1,p2,p3,p4,p5)
y<-c()

for (i in 1:length(x1)) {
  y[i] <- sample(c(1,2,3,4,5), 1, replace=TRUE, prob=pr[i,]) 
}


model1<- vglm(y~x1,
              family=acat(reverse=TRUE, parallel=TRUE))
proby1<-fitted(model1)

model2<- vglm(y~x1+x2,
              family=acat(reverse=TRUE, parallel=TRUE))
proby2<-fitted(model2)
############## Functions for Yang's method##########################
listvec <- function(x) {
  x[1]:x[2]
}

### \hat{U} y is the outcome, q0=P(y<=1), q1=P(y<=2)
marginm <- function(x, y, q0, q1, q2, q3,h) {
  n <- length(y)
  p1 <- cbind(q0, q1, q2, q3)
  ind1 <- apply(abs(p1 - x), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - x)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - x)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= (ind1 - 1))) / sum(wei)
  l
}

marginm <- Vectorize(marginm, "x")

### Bandwidth selection
bandwidthord <- function(y, q0, q1, q2, q3) {
  bw <- npregbw(ydat = c(1 * (y == 0), 1 * (y <= 1), 1 * (y <= 2), 
                         1 * (y <= 3)), 
                xdat = c(q0, q1, q2, q3), ckertype = "epanechnikov")
  return(bw$bw)
}

q0_1<-proby1[, 1]
q1_1<-proby1[, 1]+proby1[, 2]
q2_1<-proby1[, 1]+proby1[, 2]+ proby1[, 3]
q3_1<-proby1[, 1]+proby1[, 2]+proby1[, 3]+proby1[, 4]
y_quasi <- y-1

h1 <- bandwidthord(y = y_quasi, q0 = q0_1, q1 = q1_1, q2=q2_1,q3=q3_1)

q0_2<-proby2[, 1]
q1_2<-proby2[, 1]+proby2[, 2]
q2_2<-proby2[, 1]+proby2[, 2]+ proby2[, 3]
q3_2<-proby2[, 1]+proby2[, 2]+proby2[, 3]+proby2[, 4]

h2 <- bandwidthord(y = y_quasi, q0 = q0_2, q1 = q1_2, q2=q2_2, q3=q3_2)

x_vals <- seq(0, 1, length.out = 100)

# Calculate y values using your marginm function
y_vals_2 <- marginm(x_vals, y = y_quasi,
                    q0 = q0_2, q1 = q1_2, q2 = q2_2, q3 = q3_2, h = h2)
y_vals_1 <- marginm(x_vals, y = y_quasi,
                    q0 = q0_1, q1 = q1_1, q2 = q2_1, q3 = q3_1, h = h1)

# Create a data frame for plotting
data <- data.frame(x = x_vals, y2= y_vals_2, y1=y_vals_1)
# Plot using ggplot2
############## (Deviance,pearson, ProbScale, Generalized) ######
############## Probability-scale residuals ######
sign1<-rep(0,length(x1))
sign2<-rep(0,length(x1))
P_sy1<-c()
P_sy2<-c()
P_gy1<-c()
P_gy2<-c()
i<-1
j<-1
y<-as.numeric(y)
for(i in 1: length(x1)){
  if(y[i]!=1 && y[i]!=5){
    
    P_sy1[i]<- sum(proby1[i,1:(y[i]-1)])
    P_sy2[i]<- sum(proby2[i,1:(y[i]-1)])
    P_gy1[i]<- sum(proby1[i,(y[i]+1):5])
    P_gy2[i]<-sum(proby2[i,(y[i]+1):5])
  }else{
    if(y[i]==5){
      P_gy1[i]<- 0
      P_sy1[i]<- 1-proby1[i,5]
      
      P_gy2[i]<- 0
      P_sy2[i]<- 1-proby2[i,5]
    }
    if(y[i]==1){
      P_sy1[i]<- 0
      P_gy1[i]<- 1-proby1[i,1]
      P_sy2[i]<- 0
      P_gy2[i]<- 1-proby2[i,1]
    }
  }
  sign1[i]<- P_gy1[i]-P_sy1[i]
  sign2[i]<- P_gy2[i]-P_sy2[i]
}

############## Deviance residuals###########
deviance1<-resid(model1)[,1]
deviance2<-resid(model2)[,1]
############## Pearson residuals#########
pearson1<-resid(model1,type="pearson")[,1]
pearson2<-resid(model2,type="pearson")[,1]

############## Generalized residuals#########
cum.prob1<-matrix(NA,nrow = nrow(proby1),ncol = ncol(proby1)+1)
cum.prob2<-matrix(NA,nrow = nrow(proby2),ncol = ncol(proby2)+1)
cum.prob1[,1]<-rep(0,nrow(proby1))
cum.prob2[,1]<-rep(0,nrow(proby1))
for (i in 2:6) {
  cum.prob1[,i]<-cum.prob1[,i-1]+proby1[,i-1]
  cum.prob2[,i]<-cum.prob2[,i-1]+proby2[,i-1]
}


Pj1<- sapply(1:n, function(x) proby1[x,y[x]])
Fj1<- sapply(1:n, function(x) cum.prob1[x,y[x]+1])
Fj1_1<- sapply(1:n, function(x) cum.prob1[x,y[x]])
fj1<- dnorm(qnorm(Fj1))
fj1_1<- dnorm(qnorm(Fj1_1))
g1<-(fj1_1-fj1)/Pj1

Pj2<- sapply(1:n, function(x) proby2[x,y[x]])
Fj2<- sapply(1:n, function(x) cum.prob2[x,y[x]+1])
Fj2_1<- sapply(1:n, function(x) cum.prob2[x,y[x]])
fj2<- dnorm(qnorm(Fj2))
fj2_1<- dnorm(qnorm(Fj2_1))
g2<-(fj2_1-fj2)/Pj2
############## Functional residual ##########
fr1<-fresiduals(model1)
fr2<-fresiduals(model2)


############## Surrogate residual###############


y.f<-as.factor(y)

cummodel<-polr(y.f~x1,method = "logistic")
surro_cum<-PAsso::surrogate(cummodel)
summary(cummodel)
datasuro<-cbind.data.frame(surro_cum,x2,x3)

pcum_x2<-ggplot(datasuro, aes(x=x2, y=surro_cum)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[2]))+ylab("")+ylim(-5,15)+
  labs(title = "(c) Surrogate residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

pcum_x3<-ggplot(datasuro, aes(x=x3, y=surro_cum)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[3]))+ylab("")+ylim(-5,15)+
  labs(title = "(d) Surrogate residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

############ Figure S3 Residual-vs-covariate plots when X2 is correlated 
############ whereas X3 is not ########################
otherres1<-cbind.data.frame(x2,x3,sign1,deviance1,pearson1,g1)
otherres2<-cbind.data.frame(x2,x3,sign2,deviance2,pearson2,g2)

p1_norm <- fresplot(fr1,x2,title = "(a) Functional residuals",
                    scale = "normal",xl=-3.5,
                    xp=1.5,xlabs = expression(X[2]))

p2_norm <- fresplot(fr1,x3,title = "(b) Functional residuals",
                    scale = "normal",xl=-2.5,
                    xp=4,xlabs = expression(X[3]))


p1_deviance_x2<-ggplot(otherres1, aes(x=x2, y=deviance1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[2]))+ylab("")+ylim(-25,25)+
  labs(title = "(a) Deviance residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_pearson1_x2<-ggplot(otherres1, aes(x=x2, y=pearson1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[2]))+ylab("")+ylim(-10,10)+
  labs(title = "(c) Pearson residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))


p1_PS_x2<-ggplot(otherres1, aes(x=x2, y=sign1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[2]))+ylab("")+ylim(-1,1)+
  labs(title = "(e) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_gr_x2<-ggplot(otherres1, aes(x=x2, y=g1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[2]))+ylab("")+ylim(-3,3)+
  labs(title = "(c) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))


p1_PS_x3<-ggplot(otherres1, aes(x=x3, y=sign1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[3]))+ylab("")+ylim(-1,1)+
  labs(title = "(f) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_gr_x3<-ggplot(otherres1, aes(x=x3, y=g1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[3]))+ylab("")+ylim(-3,3)+
  labs(title = "(d) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_deviance_x3<-ggplot(otherres1, aes(x=x3, y=deviance1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[3]))+ylab("")+ylim(-25,25)+
  labs(title = "(b) Deviance residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_pearson_x3<-ggplot(otherres1, aes(x=x3, y=pearson1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[3]))+ylab("")+ylim(-5,5)+
  labs(title = "(d) Pearson residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p_quasi1 <- ggplot(data, aes(x = x, y = y1)) + 
  geom_line(size = 2) + # Change 'size' for line width
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "(g)Quasi-empirical residual distribution function", 
       x = "s", y = expression(hat(U) * "(s)")) +
  theme_bw() +xlim(0,1)+ylim(0,1)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

# Figure S3
grid.arrange(p1_norm,p2_norm,p1_deviance_x2,p1_deviance_x3,
             p1_pearson1_x2,p1_pearson_x3,ncol=2)

############ Figure S21 Residual-vs-covariate plots comparison with newly developed methods############
grid.arrange(p1_norm,p2_norm,
             pcum_x2,pcum_x3,
             p1_PS_x2,p1_PS_x3,p_quasi1,ncol=2)

##################################################################################
################example 4 Missing of the interaction term#########################
##################################################################################

n<-1000
set.seed(5)
x1<-rnorm(1000,0,1)
x2<-rnorm(1000,-1,0.8)
beta<-c(1,2)
x3<-x1*x2#interaction term
linearp1<--1+beta[1]*x1+beta[2]*x2+2*x3# link only for link function. linear predictor
linearp2<--2+beta[1]*x1+beta[2]*x2+2*x3
linearp3<-0.5+beta[1]*x1+beta[2]*x2+2*x3
linearp4<-2+beta[1]*x1+beta[2]*x2+2*x3
p1<-1/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p2<-exp(-linearp1)/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p3<-exp(-(linearp1+linearp2))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p4<-exp(-(linearp1+linearp2+linearp3))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
p5<-exp(-(linearp1+linearp2+linearp3+linearp4))/(1+exp(-linearp1)+exp(-(linearp1+linearp2))+exp(-(linearp1+linearp2+linearp3))+exp(-(linearp1+linearp2+linearp3+linearp4)))
pr<-cbind(p1,p2,p3,p4,p5)
y<-c()

for (i in 1:length(x1)) {
  y[i] <- sample(c(1,2,3,4,5), 1, replace=TRUE, prob=pr[i,]) 
}

model1<- vglm(y~x1+x2,
              family=acat(reverse=TRUE, parallel=TRUE))
proby1<-fitted(model1)

model2<- vglm(y~x1+x2+x3,
              family=acat(reverse=TRUE, parallel=TRUE))
proby2<-fitted(model2)
####Deviance,pearson, ProbScale, Generalized and more###############

############## Yang's method##########################
listvec <- function(x) {
  x[1]:x[2]
}

### \hat{U} y is the outcome, q0=P(y<=1), q1=P(y<=2)
marginm <- function(x, y, q0, q1, q2, q3,h) {
  n <- length(y)
  p1 <- cbind(q0, q1, q2, q3)
  ind1 <- apply(abs(p1 - x), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - x)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - x)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= (ind1 - 1))) / sum(wei)
  l
}

marginm <- Vectorize(marginm, "x")

### Bandwidth selection
bandwidthord <- function(y, q0, q1, q2, q3) {
  bw <- npregbw(ydat = c(1 * (y == 0), 1 * (y <= 1), 1 * (y <= 2), 
                         1 * (y <= 3)), 
                xdat = c(q0, q1, q2, q3), ckertype = "epanechnikov")
  return(bw$bw)
}

q0_1<-proby1[, 1]
q1_1<-proby1[, 1]+proby1[, 2]
q2_1<-proby1[, 1]+proby1[, 2]+ proby1[, 3]
q3_1<-proby1[, 1]+proby1[, 2]+proby1[, 3]+proby1[, 4]
y_quasi <- y-1

h1 <- bandwidthord(y = y_quasi, q0 = q0_1, q1 = q1_1, q2=q2_1,q3=q3_1)

q0_2<-proby2[, 1]
q1_2<-proby2[, 1]+proby2[, 2]
q2_2<-proby2[, 1]+proby2[, 2]+ proby2[, 3]
q3_2<-proby2[, 1]+proby2[, 2]+proby2[, 3]+proby2[, 4]

h2 <- bandwidthord(y = y_quasi, q0 = q0_2, q1 = q1_2, q2=q2_2, q3=q3_2)

x_vals <- seq(0, 1, length.out = 100)

# Calculate y values using your marginm function
y_vals_2 <- marginm(x_vals, y = y_quasi,
                    q0 = q0_2, q1 = q1_2, q2 = q2_2, q3 = q3_2, h = h2)
y_vals_1 <- marginm(x_vals, y = y_quasi,
                    q0 = q0_1, q1 = q1_1, q2 = q2_1, q3 = q3_1, h = h1)

# Create a data frame for plotting
data <- data.frame(x = x_vals, y2= y_vals_2, y1=y_vals_1)

############## Probability-scale residuals ##########
sign1<-rep(0,length(x1))
sign2<-rep(0,length(x1))
P_sy1<-c()
P_sy2<-c()
P_gy1<-c()
P_gy2<-c()
i<-1
j<-1
y<-as.numeric(y)
for(i in 1: length(x1)){
  if(y[i]!=1 && y[i]!=5){
    
    P_sy1[i]<- sum(proby1[i,1:(y[i]-1)])
    P_sy2[i]<- sum(proby2[i,1:(y[i]-1)])
    P_gy1[i]<- sum(proby1[i,(y[i]+1):5])
    P_gy2[i]<-sum(proby2[i,(y[i]+1):5])
  }else{
    if(y[i]==5){
      P_gy1[i]<- 0
      P_sy1[i]<- 1-proby1[i,5]
      
      P_gy2[i]<- 0
      P_sy2[i]<- 1-proby2[i,5]
    }
    if(y[i]==1){
      P_sy1[i]<- 0
      P_gy1[i]<- 1-proby1[i,1]
      P_sy2[i]<- 0
      P_gy2[i]<- 1-proby2[i,1]
    }
  }
  sign1[i]<- P_gy1[i]-P_sy1[i]
  sign2[i]<- P_gy2[i]-P_sy2[i]
}

############## Deviance residual######
deviance1<-resid(model1)[,1]
deviance2<-resid(model2)[,1]
############## Pearson residual########
pearson1<-resid(model1,type="pearson")[,1]
pearson2<-resid(model2,type="pearson")[,1]

############## Generalized residual #########
cum.prob1<-matrix(NA,nrow = nrow(proby1),ncol = ncol(proby1)+1)
cum.prob2<-matrix(NA,nrow = nrow(proby2),ncol = ncol(proby2)+1)
cum.prob1[,1]<-rep(0,nrow(proby1))
cum.prob2[,1]<-rep(0,nrow(proby1))
for (i in 2:6) {
  cum.prob1[,i]<-cum.prob1[,i-1]+proby1[,i-1]
  cum.prob2[,i]<-cum.prob2[,i-1]+proby2[,i-1]
}


Pj1<- sapply(1:n, function(x) proby1[x,y[x]])
Fj1<- sapply(1:n, function(x) cum.prob1[x,y[x]+1])
Fj1_1<- sapply(1:n, function(x) cum.prob1[x,y[x]])
fj1<- dnorm(qnorm(Fj1))
fj1_1<- dnorm(qnorm(Fj1_1))
g1<-(fj1_1-fj1)/Pj1

Pj2<- sapply(1:n, function(x) proby2[x,y[x]])
Fj2<- sapply(1:n, function(x) cum.prob2[x,y[x]+1])
Fj2_1<- sapply(1:n, function(x) cum.prob2[x,y[x]])
fj2<- dnorm(qnorm(Fj2))
fj2_1<- dnorm(qnorm(Fj2_1))
g2<-(fj2_1-fj2)/Pj2

otherres1<-cbind.data.frame(x3,sign1,deviance1,pearson1,g1)
otherres2<-cbind.data.frame(x3,sign2,deviance2,pearson2,g2)
############## Functional residual ####
fr1<-fresiduals(model1)
fr2<-fresiduals(model2)
############## Surrogate residual########################
y.f<-as.factor(y)

cummodel<-polr(y.f~x1,method = "logistic")
surro_cum<-PAsso::surrogate(cummodel)
summary(cummodel)
datasuro<-cbind.data.frame(surro_cum,x2,x3)

pcum_x2<-ggplot(datasuro, aes(x=x2, y=surro_cum)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-5,15)+
  labs(title = "(c) Surrogate residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

pcum_x3<-ggplot(datasuro, aes(x=x3, y=surro_cum)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-5,15)+
  labs(title = "(d) Surrogate residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))
#######################Figure S4 Residual-vs-covariate plots before and after the interaction term is included#######################################

p1_norm<-fresplot(fr1,x3,title="(a) Functional residuals",scale = "normal",
                  xl=-5,xp=4,yl=-2,yp=2,
                  xlabs = expression(X[1]*X[2]))
 
p2_norm<-fresplot(fr2,x3,title="(b) Functional residuals",scale = "normal",
                  xlabs = expression(X[1]*X[2]),yl=-2,yp=2,xl=-5,xp=6)
  
p1_deviance_x3<-ggplot(otherres1, aes(x=x3, y=deviance1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+
  labs(title = "(c) Deviance residuals")+ylim(-600,600)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p1_pearson1_x3<-ggplot(otherres1, aes(x=x3, y=pearson1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+
  labs(title = "(e) Pearson residuals")+ylim(-21,21)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))


p1_PS_x3<-ggplot(otherres1, aes(x=x3, y=sign1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-1,1)+
  labs(title = "(e) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_PS_x3<-ggplot(otherres2, aes(x=x3, y=sign2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-1,1)+
  labs(title = "(f) Probability-scale residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))


p1_gr_x3<-ggplot(otherres1, aes(x=x3, y=g1)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-3,3)+
  labs(title = "(c) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_gr_x3<-ggplot(otherres2, aes(x=x3, y=g2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+ylim(-3,3)+
  labs(title = "(d) Generalized residuals")+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))


p_quasi1 <- ggplot(data, aes(x = x, y = y1)) + 
  geom_line(size = 2) + # Change 'size' for line width
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "(g)Quasi-empirical residual distribution function", 
       x = "s", y = expression(hat(U) * "(s)")) +
  theme_bw() +xlim(0,1)+ylim(0,1)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p_quasi2 <- ggplot(data, aes(x = x, y = y2)) + 
  geom_line(size = 2) + # Change 'size' for line width
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "(h)Quasi-empirical residual distribution function", 
       x = "s", y = expression(hat(U) * "(s)")) +
  theme_bw() +xlim(0,1)+ylim(0,1)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))



p2_deviance_x3<-ggplot(otherres2, aes(x=x3, y=deviance2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+
  labs(title = "(d) Deviance residuals")+ylim(-600,600)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

p2_pearson_x3<-ggplot(otherres2, aes(x=x3, y=pearson2)) + 
  geom_point()+
  geom_smooth(method=loess, se=FALSE)+
  geom_hline(yintercept=0,linetype="dashed", color = "red")+
  xlab(expression(X[1]*X[2]))+ylab("")+
  labs(title = "(f) Pearson residuals")+ylim(-21,21)+
  theme(plot.title = element_text(size=12),axis.title=element_text(size=12))

grid.arrange(p1_norm,p2_norm,p1_deviance_x3,p2_deviance_x3,
             p1_pearson1_x3,p2_pearson_x3,ncol=2)
#######################Figure S22 Residual-vs-covariate plots comparison with newly developed methods########
grid.arrange(p1_norm,p2_norm,pcum_x2,pcum_x3,p1_PS_x3,p2_PS_x3,
             p_quasi1,p_quasi2,ncol=2)

