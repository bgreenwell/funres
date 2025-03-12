ffplot <- function(fresiduals, 
                   title="Fn-Fn Plot") {
  # Define the sequence for t1
  t <- seq(0, 1, 0.001)
  
  # Initialize vectors to store results
  res <- c()
  meanres <- c()
  
  # Convert range1 to a data frame if not already
  rangefortest <- as.data.frame(fresiduals)
  
  # Iterate over each value in t1
  for (i in 1:length(t)) {
    for (h in 1:nrow(rangefortest)) {
      res[h]<-punif(t[i],min=rangefortest[h,1],
                    max=rangefortest[h,2])
    }
    meanres[i]<-mean(res)
  }
  # Combine t1 and meanres1 into a data frame
  resdata <- cbind.data.frame(t, meanres)
  
  tpoints <- ggplot(resdata, aes(x=t, y=meanres)) + 
    geom_point()+
    geom_abline(intercept=0,slope=1,linetype="dashed", color = "red")+
    labs(title = title)+ylab(expression(bar(Res)(t)))+xlab("t")+
    theme(plot.title = element_text(size=12),axis.title=element_text(size=12))
  
  return(tpoints)
  
}
