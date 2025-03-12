fresiduals <- function(model) {
  model_class <- class(model)[1]
  if (model_class == "vglm") {

    
    y<-as.numeric(apply(model@y, 1, function(t) colnames(model@y)[which.max(t)]))
    y_values<-y-min(y)+1
    
    probs <- cbind.data.frame(rep(0,nrow(model@y)),fitted(model))
    
    # Initialize a matrix to store cumulative probabilities
    result <- matrix(NA, nrow = length(y_values), ncol = 2)
    
    # Loop through each observation
    for (i in 1:length(y_values)) {
      # Calculate the cumulative sum of probabilities for the range before and including the current class
      result[i, ] <- c(sum(probs[i, 1:y_values[i]]), sum(probs[i, 1:(y_values[i]+1)]))
    }
    
    return(result)
  } else if (model_class == "glm" | model_class == "Gam" | model_class == "gam"){
    if(model$family[1]=="poisson"){
    fitted_y<-model$fitted.values
    ob_y <- model$y
    result<-matrix(NA,nrow=length(ob_y),ncol = 2)
    for (i in 1:length(ob_y)) {
      result[i,]<-c(ppois(ob_y[i]-1,fitted_y[i]),
                    ppois(ob_y[i],fitted_y[i]))
    }
    return(result)
    }else if(model$family[1]=="quasipoisson"){
      fitted_y<-model$fitted.values
      ob_y <- model$y
      s_model<-summary(model)
      disper <- s_model$dispersion
      result<-matrix(NA,nrow=length(ob_y),ncol = 2)
      for (i in 1:length(ob_y)) {
        result[i,]<-c(pnbinom(ob_y[i]-1,size=fitted_y[i]/disper,mu=fitted_y[i]),
                      pnbinom(ob_y[i],size=fitted_y[i]/disper,mu=fitted_y[i]))
      }
      return(result)
    }else{
      message("Unknown glm model type: ", model$family[1])
      }
    
  }else if(model_class == "zeroinfl"){
    s_model<-summary(model)
    py <- model$y
    x <- model$model[,-1]
    u <- exp(s_model$coefficients$count[,1][-1]*x + s_model$coefficients$count[,1][1])
    pi <-  exp(s_model$coefficients$zero[,1][-1] * x+
                 s_model$coefficients$zero[,1][1])/(exp(s_model$coefficients$zero[,1][-1]* x+
                                                          s_model$coefficients$zero[,1][1])+1)
    rangez <- cbind(pzipois(py - 1, lambda = u,pstr0 = pi)
                                   , pzipois(py, lambda = u,pstr0 = pi))
    return(rangez)
  }else if(model_class == "tsglm"){
    if(model$distr=="nbinom"){
      fitted<-model$fitted.values
      disp<-1/model$sigmasq
      y<-model$response
      rangets<-matrix(NA,nrow = length(fitted),ncol = 2)
      for (i in 1:length(fitted)) {
        rangets[i,]<-c(pnbinom(y[i]-1,
                                   size=disp,
                                   mu=fitted[i]),
                           pnbinom(y[i],
                                   size=disp,
                                   mu=fitted[i]))}
      return(rangets)
    }else{message("Unknown model distribution: ", model$distr)}
  }else{message("Unknown model type: ", model_class)}
}
