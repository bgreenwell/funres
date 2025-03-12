
############0802
library(Rmisc)
library(plyr)
library(ggplot2)
library(VGAM)
library(pscl)
library(ggpointdensity)
library(MASS)
library(ExtDist)
library(vcd)
library(parallel)
library(pbmcapply)

# use 9 cores
num_cores <- detectCores() - 1

FDR<-function(meanres){
  p <- c()
  for (c in 1:ncol(meanres)) {
    p[c]<-2*min(sum(meanres[,c]>=0)/nrow(meanres),1-sum(meanres[,c]>=0)/nrow(meanres))
  }
  p<-sort(p[2:50],decreasing = F)
  result<-c(sum(p<=0.05*c(1:length(p))/length(p)),sum(p<=0.1*c(1:length(p))/length(p)))
  return(result)
}
# function to apply our method

pearson_chisq_test <- function(model) {
  pearson_residuals <- residuals(model, type = "pearson")
  pearson_chi_squared <- sum(pearson_residuals^2)
  df <- df.residual(model)
  p_value <- pchisq(pearson_chi_squared, df, lower.tail = FALSE)
  return(p_value)
}
# pearson test with default dispersion 1
pearson_chisq_testadj <- function(model) {
  pearson_residuals <- residuals(model, type = "pearson")
  
  pearson_chi_squared <- sum(pearson_residuals^2)/summary(model)$dispersion
  df <- df.residual(model)
  p_value <- pchisq(pearson_chi_squared, df, lower.tail = FALSE)
  return(p_value)
}
# adjusted pearson test with dispersion 


t1<-seq(0,1,0.02)



##########original
set.seed(3)

run_simulation_zero.oz <- function(sim, n, nn, t1) {
  meanres1B <- matrix(NA, nrow = 2000, ncol = 51)
  meanres2B <- matrix(NA, nrow = 2000, ncol = 51)
  
  px1<-rnorm(n,0,0.8)
  
  plinearp<-1+1*px1# link only for link function. linear predictor
  plambda<-exp(plinearp)
  p0<-exp(1+0.2*px1)/(exp(1+0.2*px1)+1)
  py<-rzipois(n,lambda=plambda,pstr0 = p0)
  zero<-mean(py==0)
  pdata<-cbind.data.frame(px1,py)
  pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)
  pmodel2<-zeroinfl(py~px1,data = pdata)
  
  pearson1<-residuals(pmodel1,type="pearson")
  pearson2<-residuals(pmodel2,type="pearson")
  
  
  fittedy1<-predict(pmodel1,type="response")
  
  range1 <- as.data.frame(cbind(round(ppois(py - 1, fittedy1),8), 
                                round(ppois(py, fittedy1),8)))
  
  
  u2<-exp(coef(pmodel2)[1]+coef(pmodel2)[2]*px1)
  pi2<-exp(coef(pmodel2)[3]+coef(pmodel2)[4]*px1)/(exp(coef(pmodel2)[3]+coef(pmodel2)[4]*px1)+1)
  
  range2 <- as.data.frame(cbind(round(pzipois(py - 1, lambda = u2,pstr0 = pi2),8),
                                round(pzipois(py, lambda = u2,pstr0 = pi2),8)))
  
  
  meanres1 <- sapply(t1, function(t) {
    res1 <- punif(t, min = range1$V1, max = range1$V2)
    mean(res1)
  })
  
  meanres2 <- sapply(t1, function(t) {
    res2 <- punif(t, min = range2$V1, max = range2$V2)
    mean(res2)
  })
  
  for (B in 1:2000) {
    index<-sample(1:n,nn,replace = T)
    pdataB<-pdata[index,]
    model1B<-glm(py~px1,family = "poisson",data = pdataB)
    model2B<-zeroinfl(py~px1,data = pdataB)
    
    fittedy1B<-model1B$fitted.values
    
    
    range1B <- as.data.frame(cbind(round(ppois(pdataB$py - 1, fittedy1B),8), 
                                   round(ppois(pdataB$py, fittedy1B),8)))
    u2B<-exp(coef(model2B)[1]+coef(model2B)[2]*pdataB$px1)
    pi2B<-exp(coef(model2B)[3]+
                coef(model2B)[4]*pdataB$px1)/(exp(coef(model2B)[3]+
                                                    coef(model2B)[4]*pdataB$px1)+1)
    
    range2B <- as.data.frame(cbind(round(pzipois(pdataB$py - 1, lambda = u2B,pstr0 = pi2B),8)
                                   , round(pzipois(pdataB$py, lambda = u2B,pstr0 = pi2B),8)))
    
    
    
    meanres1B[B,] <- sapply(t1, function(t) {
      res1B <- punif(t, min = range1B$V1, max = range1B$V2)
      mean(res1B)
    })
    
    meanres1B[B,]<-meanres1B[B,]-t1
    
    meanres2B[B,] <- sapply(t1, function(t) {
      res2B <- punif(t, min = range2B$V1, max = range2B$V2)
      mean(res2B)
    })
    meanres2B[B,]<-meanres2B[B,]-t1
  }
  
  meanres1B<-as.data.frame(meanres1B)
  meanres2B<-as.data.frame(meanres2B)
  
  FDRresult1<-FDR(meanres1B)
  FDRresult2<-FDR(meanres2B)
  Pearson1<-pearson_chisq_test(pmodel1)
  Pearson2<-pearson_chisq_test(pmodel2)
  
  list(FDRresult1 = FDRresult1, 
       FDRresult2 = FDRresult2, 
       Pearson1 = Pearson1, 
       Pearson2 = Pearson2,
       zero=zero)
  
}
#parameters from Example 6

Ozero_results.200 <- pbmclapply(1:1000, 
                                      run_simulation_zero.oz, 
                                      n = 200, 
                                      nn = 200, t1 = t1, 
                                      mc.cores = num_cores)



Ozero_results.300 <- pbmclapply(1:1000, 
                                run_simulation_zero.oz, 
                                n = 300, 
                                nn = 300, t1 = t1, 
                                mc.cores = num_cores)


Ozero_results.400 <- pbmclapply(1:1000, 
                                run_simulation_zero.oz, 
                                n = 400, 
                                nn = 400, t1 = t1, 
                                mc.cores = num_cores)



Ozero_results.500 <- pbmclapply(1:1000, 
                                run_simulation_zero.oz, 
                                n = 500, 
                                nn = 500, t1 = t1, 
                                mc.cores = num_cores)

# results for simulations panel B
save(Ozero_results.200,file="./paper/Ozero_results.200.rda")
save(Ozero_results.300,file="./paper/Ozero_results.300.rda")
save(Ozero_results.400,file="./paper/Ozero_results.400.rda")
save(Ozero_results.500,file="./paper/Ozero_results.500.rda")


#####zeroinflated with reduced zeros########



run_simulation_zero <- function(sim, n, nn, t1) {
  meanres1B <- matrix(NA, nrow = 2000, ncol = 51)
  meanres2B <- matrix(NA, nrow = 2000, ncol = 51)
  
  px1<-rnorm(n,0,0.8)
  
  plinearp<-1+1*px1# link only for link function. linear predictor
  plambda<-exp(plinearp)
  p0<-exp(-2-1*px1)/(exp(-2-1*px1)+1)
  py<-rzipois(n,lambda=plambda,pstr0 = p0)
  zero<-mean(py==0)
  pdata<-cbind.data.frame(px1,py)
  pmodel1<-glm(py~px1,family = poisson(link=log),data = pdata)
  pmodel2<-zeroinfl(py~px1,data = pdata)
  
  pearson1<-residuals(pmodel1,type="pearson")
  pearson2<-residuals(pmodel2,type="pearson")
  
  
  fittedy1<-predict(pmodel1,type="response")
  
  range1 <- as.data.frame(cbind(round(ppois(py - 1, fittedy1),8), 
                                round(ppois(py, fittedy1),8)))
  
  
  u2<-exp(coef(pmodel2)[1]+coef(pmodel2)[2]*px1)
  pi2<-exp(coef(pmodel2)[3]+coef(pmodel2)[4]*px1)/(exp(coef(pmodel2)[3]+coef(pmodel2)[4]*px1)+1)
  
  range2 <- as.data.frame(cbind(round(pzipois(py - 1, lambda = u2,pstr0 = pi2),8),
                                round(pzipois(py, lambda = u2,pstr0 = pi2),8)))
  
  
  meanres1 <- sapply(t1, function(t) {
    res1 <- punif(t, min = range1$V1, max = range1$V2)
    mean(res1)
  })
  
  meanres2 <- sapply(t1, function(t) {
    res2 <- punif(t, min = range2$V1, max = range2$V2)
    mean(res2)
  })
  
  for (B in 1:2000) {
    index<-sample(1:n,nn,replace = T)
    pdataB<-pdata[index,]
    model1B<-glm(py~px1,family = "poisson",data = pdataB)
    model2B<-zeroinfl(py~px1,data = pdataB)
    
    fittedy1B<-model1B$fitted.values
    
    
    range1B <- as.data.frame(cbind(round(ppois(pdataB$py - 1, fittedy1B),8), 
                                   round(ppois(pdataB$py, fittedy1B),8)))
    u2B<-exp(coef(model2B)[1]+coef(model2B)[2]*pdataB$px1)
    pi2B<-exp(coef(model2B)[3]+
                coef(model2B)[4]*pdataB$px1)/(exp(coef(model2B)[3]+
                                                    coef(model2B)[4]*pdataB$px1)+1)
    
    range2B <- as.data.frame(cbind(round(pzipois(pdataB$py - 1, lambda = u2B,pstr0 = pi2B),8)
                                   , round(pzipois(pdataB$py, lambda = u2B,pstr0 = pi2B),8)))
    
    
    
    meanres1B[B,] <- sapply(t1, function(t) {
      res1B <- punif(t, min = range1B$V1, max = range1B$V2)
      mean(res1B)
    })
    
    meanres1B[B,]<-meanres1B[B,]-t1
    
    meanres2B[B,] <- sapply(t1, function(t) {
      res2B <- punif(t, min = range2B$V1, max = range2B$V2)
      mean(res2B)
    })
    meanres2B[B,]<-meanres2B[B,]-t1
  }
  
  meanres1B<-as.data.frame(meanres1B)
  meanres2B<-as.data.frame(meanres2B)
  
  FDRresult1<-FDR(meanres1B)
  FDRresult2<-FDR(meanres2B)
  Pearson1<-pearson_chisq_test(pmodel1)
  Pearson2<-pearson_chisq_test(pmodel2)
  
  list(FDRresult1 = FDRresult1, 
       FDRresult2 = FDRresult2, 
       Pearson1 = Pearson1, 
       Pearson2 = Pearson2,
       zero=zero)
  
}
num_cores <- detectCores() - 1

Poisson_Less_zero_results.200 <- pbmclapply(1:1000, 
                                      run_simulation_zero, 
                                      n = 200, 
                                      nn = 200, t1 = t1, 
                                      mc.cores = num_cores)

Poisson_Less_zero_results.300 <- pbmclapply(1:1000, 
                                      run_simulation_zero, 
                                      n = 300, 
                                      nn = 300, t1 = t1, 
                                      mc.cores = num_cores)
Poisson_Less_zero_results.400 <- pbmclapply(1:1000, 
                                      run_simulation_zero, 
                                      n = 400, 
                                      nn = 400, t1 = t1, 
                                      mc.cores = num_cores)
Poisson_Less_zero_results.500 <- pbmclapply(1:1000, 
                                      run_simulation_zero, 
                                      n = 500, 
                                      nn = 500, t1 = t1, 
                                      mc.cores = num_cores)

# results for simulations panel A

save(Poisson_Less_zero_results.200,
     file="./paper/Poisson_Less_zero_results.200.rda")
save(Poisson_Less_zero_results.300,
     file="./paper/Poisson_Less_zero_results.300.rda")
save(Poisson_Less_zero_results.400,
     file="./paper/Poisson_Less_zero_results.400.rda")
save(Poisson_Less_zero_results.500,
     file="./paper/Poisson_Less_zero_results.500.rda")



#########overdispersion Panel A

run_simulation_over <- function(sim, n, nn, t1) {
  meanres1B <- matrix(NA, nrow = 2000, ncol = 51)
  meanres2B <- matrix(NA, nrow = 2000, ncol = 51)
  
  px1<-rnorm(n,0,1)
  plinearp<-1.2+1.3*px1# link only for link function. linear predictor
  f<-7
  plambda<-exp(plinearp)
  py<-rnbinom(n,size=f,mu=plambda)
  pdata<-cbind.data.frame(px1,py)
  pmodel1<-glm(py~px1,family = "poisson", data = pdata)
  pmodel2<-glm.nb(py~px1,data = pdata)
  #pearson1<-residuals(pmodel1,type="pearson")
  #pearson2<-residuals(pmodel2,type="pearson")
  
  
  fittedy1<-predict(pmodel1,type="response")
  fittedy2<-predict(pmodel2,type="response")
  theta_hat<-pmodel2$theta
  range1 <- as.data.frame(cbind(round(ppois(py - 1, fittedy1),6),
                                round(ppois(py, fittedy1),6)))
  

  range2<-as.data.frame(cbind(pnbinom(py-1,size=theta_hat,mu=fittedy2),
                  pnbinom(py,size=theta_hat,mu=fittedy2)))


  meanres1 <- sapply(t1, function(t) {
    res1 <- punif(t, min = range1$V1, max = range1$V2)
    mean(res1)
  })
  
  meanres2 <- sapply(t1, function(t) {
    res2 <- punif(t, min = range2$V1, max = range2$V2)
    mean(res2)
  })
  
  for (B in 1:2000) {
    index<-sample(1:n,nn,replace = T)
    pdataB<-pdata[index,]
    model1B<-glm(py~px1,family = "poisson",data = pdataB)
    model2B<-glm.nb(py~px1,data = pdataB)
    fittedy1B<-model1B$fitted.values
    fittedy2B<-model2B$fitted.values
    theta_hatB<-model2B$theta
    range1B <- as.data.frame(cbind(round(ppois(pdataB$py - 1, fittedy1B),7), 
                                   round(ppois(pdataB$py, fittedy1B),7)))
    
      range2B<-as.data.frame(cbind(pnbinom(pdataB$py-1,size=theta_hatB,
                             mu=fittedy2B),
                     pnbinom(pdataB$py,size=theta_hatB,
                             mu=fittedy2B)))
    

    
    meanres1B[B,] <- sapply(t1, function(t) {
      res1B <- punif(t, min = range1B$V1, max = range1B$V2)
      mean(res1B)
    })
    
    meanres1B[B,]<-meanres1B[B,]-t1
    
    meanres2B[B,] <- sapply(t1, function(t) {
      res2B <- punif(t, min = range2B$V1, max = range2B$V2)
      mean(res2B)
    })
    meanres2B[B,]<-meanres2B[B,]-t1
  }
  
  meanres1B<- as.data.frame(meanres1B)
  meanres2B<- as.data.frame(meanres2B)
  FDRresult1<-FDR(meanres1B)
  FDRresult2<-FDR(meanres2B)
  #Pearson1adj<-pearson_chisq_testadj(pmodel1)
  #Pearson2adj<-pearson_chisq_testadj(pmodel2)
  #Pearson1<-pearson_chisq_test(pmodel1)
  #Pearson2<-pearson_chisq_test(pmodel2)

  
  list(FDRresult1 = FDRresult1, 
       FDRresult2 = FDRresult2) 
  #Pearson1 = Pearson1, 
  #Pearson2 = Pearson2,
  #Pearson1adj=Pearson1adj,
  #Pearson2adj=Pearson2adj)
  
}

############ run until collect 1000 valid results #############
set.seed(3)
run_simulation_over.200 <- function(sim, n, nn, t1) {
  tryCatch({
    meanres1B <- matrix(NA, nrow = 2000, ncol = 51)
    meanres2B <- matrix(NA, nrow = 2000, ncol = 51)
    
    px1 <- rnorm(n, 0, 1)
    plinearp <- 1.2 + 1.3 * px1
    f <- 7
    plambda <- exp(plinearp)
    py <- rnbinom(n, size = f, mu = plambda)
    pdata <- cbind.data.frame(px1, py)
    pmodel1 <- glm(py ~ px1, family = "poisson", data = pdata)
    pmodel2 <- glm.nb(py ~ px1, data = pdata)
    
    fittedy1 <- predict(pmodel1, type = "response")
    fittedy2 <- predict(pmodel2, type = "response")
    theta_hat <- pmodel2$theta
    
    range1 <- as.data.frame(cbind(
      round(ppois(py - 1, fittedy1), 6),
      round(ppois(py, fittedy1), 6)
    ))
    
    range2 <- as.data.frame(cbind(
      pnbinom(py - 1, size = theta_hat, mu = fittedy2),
      pnbinom(py, size = theta_hat, mu = fittedy2)
    ))
    
    meanres1 <- sapply(t1, function(t) {
      res1 <- punif(t, min = range1$V1, max = range1$V2)
      mean(res1)
    })
    
    meanres2 <- sapply(t1, function(t) {
      res2 <- punif(t, min = range2$V1, max = range2$V2)
      mean(res2)
    })
    
    for (B in 1:2000) {
      index <- sample(1:n, nn, replace = TRUE)
      pdataB <- pdata[index, ]
      model1B <- glm(py ~ px1, family = "poisson", data = pdataB)
      model2B <- glm.nb(py ~ px1, data = pdataB)
      fittedy1B <- model1B$fitted.values
      fittedy2B <- model2B$fitted.values
      theta_hatB <- model2B$theta
      
      range1B <- as.data.frame(cbind(
        round(ppois(pdataB$py - 1, fittedy1B), 7),
        round(ppois(pdataB$py, fittedy1B), 7)
      ))
      
      range2B <- as.data.frame(cbind(
        pnbinom(pdataB$py - 1, size = theta_hatB, mu = fittedy2B),
        pnbinom(pdataB$py, size = theta_hatB, mu = fittedy2B)
      ))
      
      meanres1B[B, ] <- sapply(t1, function(t) {
        res1B <- punif(t, min = range1B$V1, max = range1B$V2)
        mean(res1B)
      })
      
      meanres1B[B, ] <- meanres1B[B, ] - t1
      
      meanres2B[B, ] <- sapply(t1, function(t) {
        res2B <- punif(t, min = range2B$V1, max = range2B$V2)
        mean(res2B)
      })
      
      meanres2B[B, ] <- meanres2B[B, ] - t1
    }
    
    meanres1B <- as.data.frame(meanres1B)
    meanres2B <- as.data.frame(meanres2B)
    FDRresult1 <- FDR(meanres1B)
    FDRresult2 <- FDR(meanres2B)
    
    list(FDRresult1 = FDRresult1, FDRresult2 = FDRresult2)
  }, error = function(e) {
    NULL  # Return NULL on error
  })
}

valid_results <- list()

while (length(valid_results) < 1000) {
  overdis.200 <- pbmclapply(1:1000, run_simulation_over.200, 
                            n = 200, nn = 200, t1 = t1, mc.cores = 9)
  
  # Filter out any NULL results
  valid_results <- c(valid_results, overdis.200[!sapply(overdis.200, is.null)])
  
  # Trim the list if more than 1000 results are accumulated
  if (length(valid_results) > 1000) {
    valid_results <- valid_results[1:1000]
  }
}


# results for simulations panel C

save(valid_results,file="./paper/overdis.200.rda")



overdis.300<- pbmclapply(1:1000, 
                         run_simulation_over, 
                         n = 300, 
                         nn = 300, t1 = t1, 
                         mc.cores = 8)
set.seed(3)
overdis.400<- pbmclapply(1:1000, 
                         run_simulation_over, 
                         n = 400, 
                         nn = 400, t1 = t1, 
                         mc.cores = 8)

overdis.500<- pbmclapply(1:1000, 
                         run_simulation_over, 
                         n = 500, 
                         nn = 500, t1 = t1, 
                         mc.cores = 8)


# results for simulations panel C

save(overdis.300,file="./paper/overdis.300.rda")
save(overdis.400,file="./paper/overdis.400.rda")
save(overdis.500,file="./paper/overdis.500.rda")

###########################LOAD DATA####################
load("Ozero_results.200.rda")

Ozero_FDR_1_200<- sapply(Ozero_results.200, 
                         function(x) x$FDRresult1)
Ozero_FDR_2_200<- sapply(Ozero_results.200, 
                         function(x) x$FDRresult2)
Ozero_Pearson_1_200<-sapply(Ozero_results.200,
                           function(x) x$Pearson1)
Ozero_Pearson_2_200<-sapply(Ozero_results.200, 
                           function(x) x$Pearson2)

sum(Ozero_Pearson_1_200<=0.05)/1000
sum(Ozero_Pearson_1_200<=0.1)/1000
sum(Ozero_Pearson_2_200<=0.05)/1000
sum(Ozero_Pearson_2_200<=0.1)/1000

sum(Ozero_FDR_1_200[1,]>0)/1000
sum(Ozero_FDR_1_200[2,]>0)/1000
sum(Ozero_FDR_2_200[1,]>0)/1000
sum(Ozero_FDR_2_200[2,]>0)/1000

############
Ozero_FDR_1_300<- sapply(Ozero_results.300, 
                         function(x) x$FDRresult1)
Ozero_FDR_2_300<- sapply(Ozero_results.300, 
                         function(x) x$FDRresult2)
Ozero_Pearson_1_300<-sapply(Ozero_results.300,
                            function(x) x$Pearson1)
Ozero_Pearson_2_300<-sapply(Ozero_results.300, 
                            function(x) x$Pearson2)

sum(Ozero_Pearson_1_300<=0.05)/1000
sum(Ozero_Pearson_1_300<=0.1)/1000
sum(Ozero_Pearson_2_300<=0.05)/1000
sum(Ozero_Pearson_2_300<=0.1)/1000

sum(Ozero_FDR_1_300[1,]>0)/1000
sum(Ozero_FDR_1_300[2,]>0)/1000
sum(Ozero_FDR_2_300[1,]>0)/1000
sum(Ozero_FDR_2_300[2,]>0)/1000
##########

Ozero_FDR_1_400<- sapply(Ozero_results.400, 
                         function(x) x$FDRresult1)
Ozero_FDR_2_400<- sapply(Ozero_results.400, 
                         function(x) x$FDRresult2)
Ozero_Pearson_1_400<-sapply(Ozero_results.400,
                            function(x) x$Pearson1)
Ozero_Pearson_2_400<-sapply(Ozero_results.400, 
                            function(x) x$Pearson2)

sum(Ozero_Pearson_1_400<=0.05)/1000
sum(Ozero_Pearson_1_400<=0.1)/1000
sum(Ozero_Pearson_2_400<=0.05)/1000
sum(Ozero_Pearson_2_400<=0.1)/1000

sum(Ozero_FDR_1_400[1,]>0)/1000
sum(Ozero_FDR_1_400[2,]>0)/1000
sum(Ozero_FDR_2_400[1,]>0)/1000
sum(Ozero_FDR_2_400[2,]>0)/1000

###########

Ozero_FDR_1_500<- sapply(Ozero_results.500, 
                         function(x) x$FDRresult1)
Ozero_FDR_2_500<- sapply(Ozero_results.500, 
                         function(x) x$FDRresult2)
Ozero_Pearson_1_500<-sapply(Ozero_results.500,
                            function(x) x$Pearson1)
Ozero_Pearson_2_500<-sapply(Ozero_results.500, 
                            function(x) x$Pearson2)

sum(Ozero_Pearson_1_500<=0.05)/1000
sum(Ozero_Pearson_1_500<=0.1)/1000
sum(Ozero_Pearson_2_500<=0.05)/1000
sum(Ozero_Pearson_2_500<=0.1)/1000

sum(Ozero_FDR_1_500[1,]>0)/1000
sum(Ozero_FDR_1_500[2,]>0)/1000
sum(Ozero_FDR_2_500[1,]>0)/1000
sum(Ozero_FDR_2_500[2,]>0)/1000

########16%-34.5% zeros

newzero_FDR_1_200<- sapply(poissonzero_results.200, 
                         function(x) x$FDRresult1)
newzero_FDR_2_200<- sapply(poissonzero_results.200, 
                         function(x) x$FDRresult2)
newzero_Pearson_1_200<-sapply(poissonzero_results.200,
                            function(x) x$Pearson1)
newzero_Pearson_2_200<-sapply(poissonzero_results.200, 
                            function(x) x$Pearson2)

sum(newzero_Pearson_1_200<=0.05)/1000
sum(newzero_Pearson_1_200<=0.1)/1000
sum(newzero_Pearson_2_200<=0.05)/1000
sum(newzero_Pearson_2_200<=0.1)/1000

sum(newzero_FDR_1_200[1,]>0)/1000
sum(newzero_FDR_1_200[2,]>0)/1000
sum(newzero_FDR_2_200[1,]>0)/1000
sum(newzero_FDR_2_200[2,]>0)/1000

############300

newzero_FDR_1_300<- sapply(poissonzero_results.300, 
                           function(x) x$FDRresult1)
newzero_FDR_2_300<- sapply(poissonzero_results.300, 
                           function(x) x$FDRresult2)
newzero_Pearson_1_300<-sapply(poissonzero_results.300,
                              function(x) x$Pearson1)
newzero_Pearson_2_300<-sapply(poissonzero_results.300, 
                              function(x) x$Pearson2)

sum(newzero_Pearson_1_300<=0.05)/1000
sum(newzero_Pearson_1_300<=0.1)/1000
sum(newzero_Pearson_2_300<=0.05)/1000
sum(newzero_Pearson_2_300<=0.1)/1000

sum(newzero_FDR_1_300[1,]>0)/1000
sum(newzero_FDR_1_300[2,]>0)/1000
sum(newzero_FDR_2_300[1,]>0)/1000
sum(newzero_FDR_2_300[2,]>0)/1000


############400

newzero_FDR_1_400<- sapply(poissonzero_results.400, 
                           function(x) x$FDRresult1)
newzero_FDR_2_400<- sapply(poissonzero_results.400, 
                           function(x) x$FDRresult2)
newzero_Pearson_1_400<-sapply(poissonzero_results.400,
                              function(x) x$Pearson1)
newzero_Pearson_2_400<-sapply(poissonzero_results.400, 
                              function(x) x$Pearson2)

sum(newzero_Pearson_1_400<=0.05)/1000
sum(newzero_Pearson_1_400<=0.1)/1000
sum(newzero_Pearson_2_400<=0.05)/1000
sum(newzero_Pearson_2_400<=0.1)/1000

sum(newzero_FDR_1_400[1,]>0)/1000
sum(newzero_FDR_1_400[2,]>0)/1000
sum(newzero_FDR_2_400[1,]>0)/1000
sum(newzero_FDR_2_400[2,]>0)/1000
#################500 

newzero_FDR_1_500<- sapply(poissonzero_results.500, 
                           function(x) x$FDRresult1)
newzero_FDR_2_500<- sapply(poissonzero_results.500, 
                           function(x) x$FDRresult2)
newzero_Pearson_1_500<-sapply(poissonzero_results.500,
                              function(x) x$Pearson1)
newzero_Pearson_2_500<-sapply(poissonzero_results.500, 
                              function(x) x$Pearson2)

sum(newzero_Pearson_1_500<=0.05)/1000
sum(newzero_Pearson_1_500<=0.1)/1000
sum(newzero_Pearson_2_500<=0.05)/1000
sum(newzero_Pearson_2_500<=0.1)/1000

sum(newzero_FDR_1_500[1,]>0)/1000
sum(newzero_FDR_1_500[2,]>0)/1000
sum(newzero_FDR_2_500[1,]>0)/1000
sum(newzero_FDR_2_500[2,]>0)/1000
############overdispersion





###300
over_FDR_1_300<- sapply(overdis.300, 
                        function(x) x$FDRresult1)
over_FDR_2_300<- sapply(overdis.300, 
                        function(x) x$FDRresult2)

sum(over_FDR_1_300[1,]>0)/1000
sum(over_FDR_1_300[2,]>0)/1000
sum(over_FDR_2_300[1,]>0)/1000
sum(over_FDR_2_300[2,]>0)/1000

###400
over_FDR_1_400<- sapply(overdis.400, 
                        function(x) x$FDRresult1)
over_FDR_2_400<- sapply(overdis.400, 
                        function(x) x$FDRresult2)

sum(over_FDR_1_400[1,]>0)/1000
sum(over_FDR_1_400[2,]>0)/1000
sum(over_FDR_2_400[1,]>0)/1000
sum(over_FDR_2_400[2,]>0)/1000

###500
over_FDR_1_500<- sapply(overdis.500, 
                        function(x) x$FDRresult1)
over_FDR_2_500<- sapply(overdis.500, 
                        function(x) x$FDRresult2)

sum(over_FDR_1_500[1,]>0)/1000
sum(over_FDR_1_500[2,]>0)/1000
sum(over_FDR_2_500[1,]>0)/1000
sum(over_FDR_2_500[2,]>0)/1000


#######200

over_FDR_1_200<- sapply(valid_results, 
                        function(x) x$FDRresult1)
over_FDR_2_200<- sapply(valid_results, 
                        function(x) x$FDRresult2)
sum(over_FDR_1_200[1,]>0)/1000
sum(over_FDR_1_200[2,]>0)/1000
sum(over_FDR_2_200[1,]>0)/1000
sum(over_FDR_2_200[2,]>0)/1000


