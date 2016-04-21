summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

data=read.csv('~/Documents/R/OHI/aboriginal_food_gas.csv')




sumSE=summarySE(data, measurevar="gas", groupvars=c("year"))
jpeg(paste('~/Documents/R/OHI/Figures/gas.jpg',sep=""),height=1200,width=1800,res=400,qual=100)
plot(sumSE$year,sumSE$gas,xlab="Year",ylab="Gas Prices ($ per 100L)","l")
dev.off()



sumSE=summarySE(data, measurevar="est_RFNB", groupvars=c("year"))
jpeg(paste('~/Documents/R/OHI/Figures/eRFNB.jpg',sep=""),height=1200,width=1800,res=400,qual=100)
plot(sumSE$year,sumSE$est_RFNB,xlab="Year",ylab="Estimated RFNB ($)","l")
dev.off()



sumSE=summarySE(data, measurevar="gas", groupvars=c("year","name"))

plot(sumSE$year[sumSE$name=="Bande des Innus de Pessamit"],sumSE$gas[sumSE$name=="Bande des Innus de Pessamit"])
