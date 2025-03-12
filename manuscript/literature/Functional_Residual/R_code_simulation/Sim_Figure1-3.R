#########################Figure 1############################
# Load the necessary library
library(ggplot2)
library(gridExtra)
# Define the parameters of the uniform distribution
a <- 0   # Lower bound
b <- 0.27   # Upper bound

# Create a sequence of values from the lower bound to the upper bound
x <- seq(a, 1, length.out = 1000)

# Calculate the CDF values for each x
cdf <- pmin(pmax((x - a) / (b - a), 0), 1)

# Create a data frame with x and CDF values
df <- data.frame(x, cdf)

# Plot the CDF using ggplot2
p_1.0<-ggplot(df, aes(x, cdf)) +
  geom_line() +
  labs(x = "t") +ylab("")+
  ggtitle(expression(paste("(b) Res(t ; y = 0, x = 1)")))


c <- 0.95   # Lower bound
d <- 1   # Upper bound

# Create a sequence of values from the lower bound to the upper bound
x.2 <- seq(0, 1, length.out = 1000)

# Calculate the CDF values for each x
cdf.2 <- pmin(pmax((x.2 - c) / (d - c), 0), 1)

# Create a data frame with x and CDF values
df.2 <- data.frame(x.2, cdf.2)

# Plot the CDF using ggplot2
p__1.1<-ggplot(df.2, aes(x.2, cdf.2)) +
  geom_line() +
  labs(x = "t") + ylab("")+
  ggtitle(expression(paste("(a) Res(t ; y = 1, x = -1)")))
grid.arrange(p__1.1,p_1.0,ncol=2)


#########################Figure 2############################
library(viridis)
library(MASS)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(plyr)
library(VGAM)
library(pscl)
library(ggpointdensity)
set.seed(3)

n<-998
ex<-rnorm(998,0,1)
ex<-sort(ex)
elink<--1+2*ex
py1<-exp(elink)/(exp(elink)+1)
ey<-c()
py<-cbind(1-py1,py1)
for (i in 1:length(ex)) {
  ey[i] <- sample(c(0,1), 1, replace=TRUE, prob=py[i,]) 
}
x<-c(-1,1)
yobs<-c(1,0)
ex<-c(ex,x)
ey<-c(ey,yobs)

emodel<-glm(ey~ex,family = "binomial")

residuals.glm(emodel,type="pearson")[999:1000]

range1<-matrix(c(0.95,1,0,0.27),nrow=2,byrow = T)
range1<-as.data.frame(range1)
range1<-cbind(x,range1)
enumbers1<-matrix(NA,nrow=2,ncol = 101)
for (h in 1:2) {
  for (a in 1:ncol(enumbers1)) {
    enumbers1[h,a]<-range1[h,2]+(range1[h,3]-range1[h,2])/100*(a-1)
  }
}
enumbers1<-as.vector(enumbers1)
exforplot<-rep(x,101)
plotdata<-cbind.data.frame(exforplot,enumbers1)


example_heat<-ggplot(plotdata, aes(exforplot,enumbers1)) +
  geom_pointdensity(adjust=0.2,method="kde2d") +
  scale_color_viridis(name = "density")+xlab("X")+ylab("")+xlim(-2,2)+
  labs(title = "(a) Functional residuals")
example_heat

yfitted<-c(1-0.95,1-0.27)


pearsonresidual<-(yobs-yfitted)/sqrt(yfitted*(1-yfitted))

point2<-cbind.data.frame(x,pearsonresidual)
colnames(point2)<-c("X1","Point2")
point2forexample<-ggplot(point2, aes(x=X1, y=Point2)) + 
  geom_point()+
  xlab("X")+ylab("")+xlim(-2,2)+
  labs(title = "(b) Pearson (point) residuals")


grid.arrange(example_heat,point2forexample,ncol=2,widths = c(1/2.5,1/3))
#########################Figure 3############################
set.seed(3)
x= seq(-4,4, length.out = 1000)
y= dnorm(x)+0.1

library(shape)
library(grid)

par(mar=c(2,2,2,2))
par(mfrow=c(1,2))
plot(x, y, type='l', frame.plot = T, axes = F, ylab = "", xlab = "", lwd=2, ylim = c(0, 0.5))
axis(1, at= c(-5, -1.5, 0, 5), pos=0.08, tick = T, labels = F)
text(x=c(-1.5, 0), y= c(0.06, 0.06, 0.06), c(expression(y),expression(alpha+beta*x)))
lines(x= c(0, 0), y=c(0.07, dnorm(0)+0.5))
lines(x= c(-1.5, 0), y=c(dnorm(-1.5)+0.1, dnorm(-1.5)+0.1), lty=3, lwd=2)
lines(x=c(-1.5,-1.5) , y=c(0.08,dnorm(-1.5)+0.1), lty=3, lwd=2)
text(x=-0.75, y=dnorm(-1.5)+0.12, expression( r == y-(alpha+beta*x)),cex=1.2)
title(main = "(a) Point residual for continuous data",cex.main=1.8,sub=" ",adj=0)

x1 <- seq(-3,-1.38,length.out = 1000)
y1 <- rep(0.35,1000)
plot(x1,y1, type='l', frame.plot = T, axes = F, ylab = "", xlab = "", lwd=2, ylim = c(0, 0.5),xlim=c(-3,3))
axis(1, at= c(-3,-1.38 , 3), pos=0.1, tick = T, labels = F)
lines(x= c(-3, -1.38), y=c(0.35,0.35),col="red",lwd=8)
lines(x= c(-1.38, 3), y=c(0.35,0.35), lty=3, lwd=2)
lines(x= c(-1.38, -1.38), y=c(0.35,0.1), lty=3, lwd=2)
lines(x= c(-3, -3), y=c(0.35,0.1), lty=3, lwd=2)
lines(x= c(3, 3), y=c(0.35,0.1), lty=3, lwd=2)
text(x=c(-3,0,3), y= c(0.08, 0.08, 0.08), 
     c(0,expression(paste(pi,"(0;x) = 1/(",1+exp(alpha+beta*x),")",sep="")),1),cex=1.2)
text(x=-1.2, y=0.4, expression(paste("Res(t)=Pr{",u <= t,"|y=0}")),cex=1.5)
text(x=-2.19,y=0.37,"{",srt = 270,cex=10,family = 'Helvetica Neue UltraLight')
title(main = "(b) Functional residual for discrete data",cex.main=1.8,sub=" ",adj = 0)
##1250*550 width*height






