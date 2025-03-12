ffplot.envelop<-function(model,
                         B=2000,
                         alpha=0.05,
                         title=""){
  # Define the sequence for t1
  t <- seq(0, 1, 0.02)
  # Initialize vectors to store results
  res <- c()
  meanres <- c()
  
  fresiduals<-fresiduals(model)
  rangefortest <- as.data.frame(fresiduals)
  n<-nn<-nrow(rangefortest)
  # Iterate over each value in t1
  for (i in 1:length(t)) {
    for (h in 1:nrow(rangefortest)) {
      res[h]<-punif(t[i],min=rangefortest[h,1],
                    max=rangefortest[h,2])
    }
    meanres[i]<-mean(res)
  }
  model_class <- class(model)[1]
  length <- dim(data)[1]
  
  
  ########## Bootstrapping for probable envelop ##############
  meanresB <- matrix(NA, nrow = B, ncol = 51)
    
    if (model_class == "vglm") {
      for (b in 1:B) {
      index<-sample(1:n,nn,replace = T)
      y_values <- unlist(apply(model@y, 1, function(row) which(row == 1)))
      x_values <- model@x[,-1]
      dataB<-cbind(y_values,x_values)
      dataB<- dataB[index,]
      modelB <- vglm(y_values~.,family=acat(reverse=TRUE, parallel=TRUE),data = dataB)
      fresidB<-fresiduals(modelB)
      fresidB<- as.data.frame(fresidB)
      
      meanresB[b,] <- sapply(t, function(t) {
        resB <- punif(t, min = fresidB[,1], max = fresidB[,2])
        mean(resB)
      })
      meanresB[b,]<-meanresB[b,]-t
      }
    } else if (model_class == "glm"){
      if(model$family[1]=="poisson"){
        for (b in 1:B) {
        index<-sample(1:n,nn,replace = T)
        data<-model$model
        dataB <- data[index,]
        yB<-dataB[,1]
        xB<-as.matrix(dataB[,-1])
        pmodelB <- glm(yB~xB,family = poisson(link=log), data = dataB)
        fresidB<-fresiduals(pmodelB)
        fresidB<- as.data.frame(fresidB)
        
        meanresB[b,] <- sapply(t, function(t) {
          resB <- punif(t, min = fresidB[,1], max = fresidB[,2])
          mean(resB)
        })
        meanresB[b,]<-meanresB[b,]-t
        }
      }else if(model$family[1]=="quasipoisson"){
        for (b in 1:B) {
          index<-sample(1:n,nn,replace = T)
          data<-model$model
          dataB <- data[index,]
          pmodelB <- glm(dataB[,1]~dataB[,-1],family = quasipoisson, data = dataB)
          
          fresidB<-fresiduals(pmodelB)
          fresidB<- as.data.frame(fresidB)
          
          meanresB[b,] <- sapply(t, function(t) {
            resB <- punif(t, min = fresidB[,1], max = fresidB[,2])
            mean(resB)
          })
          meanresB[b,]<-meanresB[b,]-t
        }
      }else{
        message("Unknown glm model type: ", model$family[1])
      }
      
    }else if(model_class == "zeroinfl"){
      for (b in 1:B) {
        index<-sample(1:n,nn,replace = T)
        data<-model$model
        dataB <- data[index,]
        pmodelB <- zeroinfl(dataB[,1]~dataB[,-1], data = dataB)
        
        fresidB<-fresiduals(pmodelB)
        fresidB<- as.data.frame(fresidB)
        
        meanresB[b,] <- sapply(t, function(t) {
          resB <- punif(t, min = fresidB[,1], max = fresidB[,2])
          mean(resB)
        })
        meanresB[b,]<-meanresB[b,]-t
      }
    }else {message("Unknown model type: ", model_class)}
    

  meanresB<-as.data.frame(meanresB)
  
  interval<-matrix(NA,nrow=51,ncol=2)
  l<-alpha/2
  u<-1-alpha/2
  for (j in 1:51) {
    interval[j,]<-quantile(meanresB[,j],probs = c(l,u))
  }
  interval <- as.data.frame(interval)
  ffdata<-cbind(interval,t,meanres)
  
  intervalLine1 <- ggplot(interval, aes(t)) +
    geom_line(aes(y = V1 + t), colour = "black", linetype = "dotted") +
    geom_line(aes(y = V2 + t), colour = "black", linetype = "dotted") +
    geom_line(aes(y = meanres), colour = "black", size = 1.2) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = title,
         x = "t")+ylab(expression(bar(Res)(t)))
  return(intervalLine1)
}
