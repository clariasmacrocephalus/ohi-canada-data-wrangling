### Food prices ###
library(foreign)
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")
RFNB=read.csv("~/R/OHI/RFNB.csv")
InterCity=read.csv("~/R/OHI/03260015-eng.csv")
InterCity=InterCity[InterCity$COMMODITY=="Food",]
cityLL=read.csv("~/R/OHI/InterCity_LL.csv")

#merge inuit_data with RFNB
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)


#calculate distance from nearest "intercity index"
library(fields)
distmat=rdist.earth(cbind(ID_RFNB$Long_X,ID_RFNB$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
ID_RFNB$nearcity=0
for(i in 1:dim(ID_RFNB)[1]){
  (distmat==apply(distmat,1,function(x) min(x)))[i,]
  ID_RFNB$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
}
ID_RFNB$nc_dist=0
for(i in 1:dim(ID_RFNB)[1]){
  index=cityLL$InterCity==ID_RFNB$nearcity[i]
  ID_RFNB$nc_dist[i]=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F)  
}

#mean RFNB
ID_RFNB$avgRFNB=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$avgRFNB[i]=mean(c(ID_RFNB$X2005[i],ID_RFNB$X2006[i],ID_RFNB$X2007[i],ID_RFNB$X2008[i],ID_RFNB$X2009[i],ID_RFNB$X2010[i]),na.rm=T)
}


plot(ID_RFNB$avgRFNB,ID_RFNB$nc_dist)
fit=lm(ID_RFNB$avgRFNB~ID_RFNB$nc_dist,ID_RFNB)
summary(fit) # show results



plot(ID_RFNB$nc_dist,ID_RFNB$X2005,ylim=c(200,500),xlim=c(500,3000))
points(ID_RFNB$nc_dist,ID_RFNB$X2006,pch=2)
points(ID_RFNB$nc_dist,ID_RFNB$X2007,pch=3)
points(ID_RFNB$nc_dist,ID_RFNB$X2008,pch=4)
points(ID_RFNB$nc_dist,ID_RFNB$X2009,pch=5)
points(ID_RFNB$nc_dist,ID_RFNB$X2010,pch=6)

lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),IC_CPI=rep(NA,dim(ID_RFNB)[1]*6),distance=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB[i,42+y]
    lm_data$IC_CPI[index]=InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(ID_RFNB$nearcity[i])]
    lm_data$distance[index]=ID_RFNB$nc_dist[i]
    lm_data$population[index]=ID_RFNB$POPULATION[i]
  }
}
lm_data$RFNB[lm_data$RFNB==0]=NA

#fit <- lm(lm_data$RFNB ~ lm_data$distance, data=lm_data)
#summary(fit) # show results


fit <- lm(lm_data$RFNB ~ 0+lm_data$distance, data=lm_data)
plot(lm_data$distance,lm_data$RFNB)#,ylim=c(200,500),xlim=c(500,3000))
abline(c(0,coef(fit)))
summary(fit)
fit <- lm(lm_data$RFNB ~ lm_data$population, data=lm_data)
plot(lm_data$population,lm_data$RFNB)#,ylim=c(200,500),xlim=c(500,3000))
abline(coef(fit))
summary(fit)
fit <- lm(lm_data$RFNB ~ lm_data$IC_CPI, data=lm_data)
plot(lm_data$IC_CPI,lm_data$RFNB)#,ylim=c(200,500),xlim=c(500,3000))
abline(coef(fit))
summary(fit)
fit <- lm(lm_data$RFNB ~ 0+lm_data$distance + lm_data$IC_CPI, data=lm_data)
summary(fit) # show results


#fit <- lm(lm_data$RFNB~lm_data$distance+I(lm_data$distance^2)+I(lm_data$distance^3)+lm_data$IC_CPI+I(lm_data$IC_CPI^2)+I(lm_data$IC_CPI^3), data=lm_data)
#summary(fit) # show results

#fit <- lm(lm_data$RFNB~lm_data$distance+I(lm_data$distance^2), data=lm_data)
#summary(fit) # show results

### estimate food price for all inuit communities##############################
distmat=rdist.earth(cbind(inuit_data$Long_X,inuit_data$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
inuit_data$nearcity=0
for(i in 1:dim(inuit_data)[1]){
  (distmat==apply(distmat,1,function(x) min(x)))[i,]
  inuit_data$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
}
inuit_data$nc_dist=0
for(i in 1:dim(inuit_data)[1]){
  index=cityLL$InterCity==inuit_data$nearcity[i]
  inuit_data$nc_dist[i]=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F)  
}
#estimate for each year
inuit_data$est_RFNB2012=0
inuit_data$est_RFNB2011=0
inuit_data$est_RFNB2010=0
inuit_data$est_RFNB2009=0
inuit_data$est_RFNB2008=0
inuit_data$est_RFNB2007=0
inuit_data$est_RFNB2006=0
inuit_data$est_RFNB2005=0
for(y in 1:8){
  for(i in 1:dim(inuit_data)[1]){
    inuit_data[i,50-y]=as.numeric(fit$coefficients[1]*inuit_data$nc_dist[i]+fit$coefficients[2]*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])])
    #inuit_data[i,50-y]=as.numeric(fit$coefficients[1]+fit$coefficients[2]*inuit_data$nc_dist[i]+fit$coefficients[3]*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])])
  }
}
#merge inuit_data with RFNB again
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)
lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),est_RFNB=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB[i,52+y]
    lm_data$est_RFNB[index]=ID_RFNB[i,50-y]
  }
}
fit <- lm(lm_data$est_RFNB ~ lm_data$RFNB, data=lm_data)
summary(fit) # show results
plot(lm_data,xlab="Revised Northern Food Basket ($)",ylab="Estimated Revised Northern Food Basket ($)",ylim=c(250,500),xlim=c(250,500))
lines(cbind(c(0,500),c(0,500)))
abline(coef(fit))



#######################################################################################################################################################
## split data set
#######################################################################################################################################################
### Food prices ###
library(foreign)
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")
RFNB=read.csv("~/R/OHI/RFNB.csv")
InterCity=read.csv("~/R/OHI/03260015-eng.csv")
InterCity=InterCity[InterCity$COMMODITY=="Food",]
cityLL=read.csv("~/R/OHI/InterCity_LL.csv")

#merge inuit_data with RFNB
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)


#calculate distance from nearest "intercity index"
library(fields)
distmat=rdist.earth(cbind(ID_RFNB$Long_X,ID_RFNB$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
ID_RFNB$nearcity=0
for(i in 1:dim(ID_RFNB)[1]){
  (distmat==apply(distmat,1,function(x) min(x)))[i,]
  ID_RFNB$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
}
ID_RFNB$nc_dist=0
for(i in 1:dim(ID_RFNB)[1]){
  index=cityLL$InterCity==ID_RFNB$nearcity[i]
  ID_RFNB$nc_dist[i]=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F)  
}

#split data set
ID_RFNB2=ID_RFNB
ID_RFNB2[,]=NA
ID_RFNB1=ID_RFNB2
for(i in 1:dim(ID_RFNB)[1]){
  if(sum(is.na(ID_RFNB[i,43:48]))>2){
    ID_RFNB1[i,]=ID_RFNB[i,]
  } else {
    ID_RFNB2[i,]=ID_RFNB[i,]
  }
}
fits=data.frame(b=rep(NA,dim(ID_RFNB)[1]),m=rep(NA,dim(ID_RFNB)[1]))
for(i in 1:dim(ID_RFNB)[1]){
  x=InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(ID_RFNB$nearcity[i])]
  y=as.numeric(ID_RFNB2[i,43:48])
  if(sum(is.na(y))<4){
    lm_data=data.frame(cbind(y,x))
    fit=lm(lm_data$y~0+lm_data$x,data=lm_data)
    fits[i,]=c(0,fit$coefficients[1])
  }
}
m=mean(fits$m,na.rm=T)
b=mean(fits$b,na.rm=T)

lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),IC_CPI=rep(NA,dim(ID_RFNB)[1]*6),distance=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB2[i,42+y]
    lm_data$IC_CPI[index]=InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(ID_RFNB$nearcity[i])]
    lm_data$distance[index]=ID_RFNB$nc_dist[i]
    lm_data$population[index]=ID_RFNB$POPULATION[i]
  }
}
fit <- lm(lm_data$RFNB ~ lm_data$IC_CPI, data=lm_data)
plot(lm_data$IC_CPI,lm_data$RFNB)#,ylim=c(200,500),xlim=c(500,3000))
abline(b,m)
abline(fit)
summary(fit)

###### correct for CPI ############
ID_RFNB$C_RFNB2010=0
ID_RFNB$C_RFNB2009=0
ID_RFNB$C_RFNB2008=0
ID_RFNB$C_RFNB2007=0
ID_RFNB$C_RFNB2006=0
ID_RFNB$C_RFNB2005=0
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    ID_RFNB[i,57-y]=ID_RFNB[i,42+y]-m*(100-InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(ID_RFNB$nearcity[i])])
  }
}

ID_RFNB$C_RFNBmean=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$C_RFNBmean[i]=mean(as.numeric(ID_RFNB[i,51:56]),na.rm=T)
}
fit <- lm(ID_RFNB$C_RFNBmean ~ ID_RFNB$nc_dist, data=ID_RFNB)
plot(ID_RFNB$nc_dist,ID_RFNB$C_RFNBmean)
abline(fit)
summary(fit)

### estimate food price for all inuit communities##############################
distmat=rdist.earth(cbind(inuit_data$Long_X,inuit_data$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
inuit_data$nearcity=0
for(i in 1:dim(inuit_data)[1]){
  (distmat==apply(distmat,1,function(x) min(x)))[i,]
  inuit_data$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
}
inuit_data$nc_dist=0
for(i in 1:dim(inuit_data)[1]){
  index=cityLL$InterCity==inuit_data$nearcity[i]
  inuit_data$nc_dist[i]=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F)  
}
#estimate for each year
inuit_data$est_RFNB2012=0
inuit_data$est_RFNB2011=0
inuit_data$est_RFNB2010=0
inuit_data$est_RFNB2009=0
inuit_data$est_RFNB2008=0
inuit_data$est_RFNB2007=0
inuit_data$est_RFNB2006=0
inuit_data$est_RFNB2005=0
for(y in 1:8){
  for(i in 1:dim(inuit_data)[1]){
    inuit_data[i,50-y]=as.numeric(fit$coefficients[1]+inuit_data$nc_dist[i]*fit$coefficients[2]-m*(100-InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])]))
  }
}
#merge inuit_data with RFNB again
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)
lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),est_RFNB=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB[i,52+y]
    lm_data$est_RFNB[index]=ID_RFNB[i,50-y]
  }
}
fit <- lm(lm_data$est_RFNB ~ lm_data$RFNB, data=lm_data)
summary(fit) # show results
plot(lm_data,xlab="Revised Northern Food Basket ($)",ylab="Estimated Revised Northern Food Basket ($)",ylim=c(250,500),xlim=c(250,500))
lines(cbind(c(0,500),c(0,500)))
abline(coef(fit))

#######################################################################################################################################################
## include nearest entry point
#######################################################################################################################################################
### Food prices ###
library(foreign)
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")
RFNB=read.csv("~/R/OHI/RFNB.csv")
InterCity=read.csv("~/R/OHI/03260015-eng.csv")
InterCity=InterCity[InterCity$COMMODITY=="Food",]
cityLL=read.csv("~/R/OHI/InterCity_LL.csv")

#merge inuit_data with RFNB
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name")


#calculate distance from nearest "intercity index" and "entry point"
library(fields)
distmat=rdist.earth(cbind(ID_RFNB$Long_X,ID_RFNB$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
ID_RFNB$nearcity=0
ID_RFNB$nc_dist=0
ID_RFNB$nearentry=0
ID_RFNB$ne_dist=0
ID_RFNB$avg_neRFNB=0
ID_RFNB$avg_neRFNB=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
  index=cityLL$InterCity==ID_RFNB$nearcity[i]
  ID_RFNB$nc_dist[i]=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F) 
  index=RFNB$Region==ID_RFNB$Region[i]&RFNB$Type=="Entry"
  distmat2=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(RFNB$long[index],RFNB$lat[index]),miles=F)
  ID_RFNB$nearentry[i]=cbind(as.character(RFNB$Name[index]))[distmat2==min(distmat2)]
  ID_RFNB$ne_dist[i]=min(distmat2)
  ID_RFNB$avg_neRFNB[i]=mean(as.numeric(RFNB[RFNB$Region==ID_RFNB$Region[i]&RFNB$Name==ID_RFNB$nearentry[i],7:12]),na.rm=T) 
  ID_RFNB$avg_RFNB[i]=mean(as.numeric(ID_RFNB[i,45:50]),na.rm=T) 
}

fit <- lm(ID_RFNB$avg_RFNB ~ ID_RFNB$POPULATION+ID_RFNB$nc_dist+ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB, data=ID_RFNB)
summary(fit) # show results
fit <- lm(ID_RFNB$avg_RFNB ~ ID_RFNB$POPULATION+ID_RFNB$nc_dist+ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB, data=ID_RFNB)
summary(fit) # show results

#i=rep(NA,dim(ID_RFNB)[1]*6)
#lm_data=data.frame(RFNB=i,IC_CPI=i,population=i,nc_dist=i,ne_dist=i,avg_neRFNB=i)
#for(y in 1:6){
#  for(i in 1:dim(ID_RFNB)[1]){
#    index=((y-1)*dim(ID_RFNB)[1])+i
#    lm_data$RFNB[index]=ID_RFNB[i,42+y]
#    lm_data$IC_CPI[index]=InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(ID_RFNB$nearcity[i])]
#    lm_data$nc_dist[index]=ID_RFNB$nc_dist[i]
#    lm_data$population[index]=ID_RFNB$POPULATION[i]
#    lm_data$ne_dist[index]=ID_RFNB$ne_dist[i]
#    lm_data$avg_neRFNB[index]=ID_RFNB$avg_neRFNB[i]
#  }
#}
#fit <- lm(lm_data$RFNB ~ lm_data$IC_CPI+lm_data$population+lm_data$nc_dist+lm_data$ne_dist+lm_data$avg_neRFNB, data=lm_data)
#summary(fit) # show results


#calculate distance from nearest "intercity index" and "entry point" for inuit_data
library(fields)
inuit_data$RFNBRegion=0
inuit_data$nearcity=0
inuit_data$nc_dist=0
inuit_data$nearentry=0
inuit_data$ne_dist=0
inuit_data$avg_neRFNB=0

for(i in 1:dim(inuit_data)[1]){
  if(sum(inuit_data$NAME[i]==ID_RFNB$NAME)>0){
    inuit_data$RFNBRegion[i]=as.character(ID_RFNB$Region[inuit_data$NAME[i]==ID_RFNB$NAME])
  }
}

unique(inuit_data$NAME[inuit_data$RFNBRegion==0])
inuit_data$RFNBRegion[inuit_data$NAME=="Ivujivik"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Aklavik"]="Northwest Territories"
inuit_data$RFNBRegion[inuit_data$NAME=="Bathurst Inlet"]="Kitikmeot Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Umingmaktuuq"]="Kitikmeot Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Kuujjuaraapik"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Quaqtaq"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Resolute Bay"]="Baffin Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Aupaluk"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Killiniq"]="Nunavik Region"


                                        

distmat=rdist.earth(cbind(inuit_data$Long_X,inuit_data$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
for(i in 1:dim(inuit_data)[1]){
  inuit_data$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
  index=cityLL$InterCity==inuit_data$nearcity[i]
  inuit_data$nc_dist[i]=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F) 
  index=RFNB$Region==inuit_data$RFNBRegion[i]&RFNB$Type=="Entry"
  distmat2=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(RFNB$long[index],RFNB$lat[index]),miles=F)
  inuit_data$nearentry[i]=cbind(as.character(RFNB$Name[index]))[distmat2==min(distmat2)]
  inuit_data$ne_dist[i]=min(distmat2)
  inuit_data$avg_neRFNB[i]=mean(as.numeric(RFNB[RFNB$Region==inuit_data$RFNBRegion[i]&RFNB$Name==inuit_data$nearentry[i],7:12]),na.rm=T) 
}

#split data set
ID_RFNB2=ID_RFNB
ID_RFNB2[,]=NA
ID_RFNB1=ID_RFNB2
for(i in 1:dim(ID_RFNB)[1]){
  if(sum(is.na(ID_RFNB[i,59:64]))>2){
    ID_RFNB1[i,]=ID_RFNB[i,]
  } else {
    ID_RFNB2[i,]=ID_RFNB[i,]
  }
}
fits=data.frame(b=rep(NA,dim(ID_RFNB)[1]),m=rep(NA,dim(ID_RFNB)[1]))
for(i in 1:dim(ID_RFNB)[1]){
  x=InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(ID_RFNB$nearcity[i])]
  y=as.numeric(ID_RFNB2[i,43:48])
  if(sum(is.na(y))<4){
    lm_data=data.frame(cbind(y,x))
    fit=lm(lm_data$y~0+lm_data$x,data=lm_data)
    fits[i,]=c(0,fit$coefficients[1])
  }
}
m=mean(fits$m,na.rm=T)
b=mean(fits$b,na.rm=T)

#estimate for each year
inuit_data$est_RFNB2012=0
inuit_data$est_RFNB2011=0
inuit_data$est_RFNB2010=0
inuit_data$est_RFNB2009=0
inuit_data$est_RFNB2008=0
inuit_data$est_RFNB2007=0
inuit_data$est_RFNB2006=0
inuit_data$est_RFNB2005=0
for(y in 1:8){
  for(i in 1:dim(inuit_data)[1]){
    inuit_data[i,54-y]=as.numeric(fit$coefficients[1]
                                  +fit$coefficients[2]*inuit_data$POPULATION[i]
                                  +fit$coefficients[3]*inuit_data$nc_dist[i]
                                  +fit$coefficients[4]*inuit_data$ne_dist[i]
                                  +fit$coefficients[5]*inuit_data$avg_neRFNB[i]
                                  #+fit$coefficients[6]*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])]
                                  m*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])]
                                  
                                  )
  }
}
#merge inuit_data with RFNB again
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)
lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),est_RFNB=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB[i,58+y]
    lm_data$est_RFNB[index]=ID_RFNB[i,54-y]
  }
}
fit <- lm(lm_data$est_RFNB ~ lm_data$RFNB, data=lm_data)
summary(fit) # show results
plot(lm_data,xlab="Revised Northern Food Basket ($)",ylab="Estimated Revised Northern Food Basket ($)",ylim=c(250,500),xlim=c(250,500))
lines(cbind(c(0,500),c(0,500)))
abline(coef(fit))


#######################################################################################################################################################
## include nearest entry point and split data set
#######################################################################################################################################################
### Food prices ###
library(foreign)
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")
RFNB=read.csv("~/R/OHI/RFNB.csv")
InterCity=read.csv("~/R/OHI/03260015-eng.csv")
InterCity=InterCity[InterCity$COMMODITY=="Food",]
cityLL=read.csv("~/R/OHI/InterCity_LL.csv")

#merge inuit_data with RFNB
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name")


#calculate distance from nearest "intercity index" and "entry point"
library(fields)
distmat=rdist.earth(cbind(ID_RFNB$Long_X,ID_RFNB$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
ID_RFNB$nearcity=0
ID_RFNB$nc_dist=0
ID_RFNB$nearentry=0
ID_RFNB$ne_dist=0
ID_RFNB$avg_neRFNB=0
ID_RFNB$avg_neRFNB=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
  index=cityLL$InterCity==ID_RFNB$nearcity[i]
  ID_RFNB$nc_dist[i]=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F) 
  index=RFNB$Region==ID_RFNB$Region[i]&RFNB$Type=="Entry"
  distmat2=rdist.earth(cbind(ID_RFNB$Long_X[i],ID_RFNB$Lat_Y[i]),cbind(RFNB$long[index],RFNB$lat[index]),miles=F)
  ID_RFNB$nearentry[i]=cbind(as.character(RFNB$Name[index]))[distmat2==min(distmat2)]
  ID_RFNB$ne_dist[i]=min(distmat2)
  ID_RFNB$avg_neRFNB[i]=mean(as.numeric(RFNB[RFNB$Region==ID_RFNB$Region[i]&RFNB$Name==ID_RFNB$nearentry[i],7:12]),na.rm=T) 
  ID_RFNB$avg_RFNB[i]=mean(as.numeric(ID_RFNB[i,45:50]),na.rm=T) 
}

#fit <- lm(ID_RFNB$avg_RFNB ~ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
#summary(fit) # show results
fit <- lm(ID_RFNB$avg_RFNB ~0+ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
summary(fit) # show results

#calculate distance from nearest "intercity index" and "entry point" for inuit_data
library(fields)
inuit_data$RFNBRegion=0
inuit_data$nearcity=0
inuit_data$nc_dist=0
inuit_data$nearentry=0
inuit_data$ne_dist=0
inuit_data$avg_neRFNB=0

for(i in 1:dim(inuit_data)[1]){
  if(sum(inuit_data$NAME[i]==ID_RFNB$NAME)>0){
    inuit_data$RFNBRegion[i]=as.character(ID_RFNB$Region[inuit_data$NAME[i]==ID_RFNB$NAME])
  }
}

unique(inuit_data$NAME[inuit_data$RFNBRegion==0])
inuit_data$RFNBRegion[inuit_data$NAME=="Ivujivik"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Aklavik"]="Northwest Territories"
inuit_data$RFNBRegion[inuit_data$NAME=="Bathurst Inlet"]="Kitikmeot Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Umingmaktuuq"]="Kitikmeot Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Kuujjuaraapik"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Quaqtaq"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Resolute Bay"]="Baffin Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Aupaluk"]="Nunavik Region"
inuit_data$RFNBRegion[inuit_data$NAME=="Killiniq"]="Nunavik Region"




distmat=rdist.earth(cbind(inuit_data$Long_X,inuit_data$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
for(i in 1:dim(inuit_data)[1]){
  inuit_data$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
  index=cityLL$InterCity==inuit_data$nearcity[i]
  inuit_data$nc_dist[i]=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F) 
  index=RFNB$Region==inuit_data$RFNBRegion[i]&RFNB$Type=="Entry"
  distmat2=rdist.earth(cbind(inuit_data$Long_X[i],inuit_data$Lat_Y[i]),cbind(RFNB$long[index],RFNB$lat[index]),miles=F)
  inuit_data$nearentry[i]=cbind(as.character(RFNB$Name[index]))[distmat2==min(distmat2)]
  inuit_data$ne_dist[i]=min(distmat2)
  inuit_data$avg_neRFNB[i]=mean(as.numeric(RFNB[RFNB$Region==inuit_data$RFNBRegion[i]&RFNB$Name==inuit_data$nearentry[i],7:12]),na.rm=T) 
}

#split data set
ID_RFNB2=ID_RFNB
ID_RFNB2[,]=NA
ID_RFNB1=ID_RFNB2
for(i in 1:dim(ID_RFNB)[1]){
  if(sum(is.na(ID_RFNB[i,45:50]))>2){
    ID_RFNB1[i,]=ID_RFNB[i,]
  } else {
    ID_RFNB2[i,]=ID_RFNB[i,]
  }
}
fits=data.frame(b=rep(NA,dim(ID_RFNB)[1]),m=rep(NA,dim(ID_RFNB)[1]))
for(i in 1:dim(ID_RFNB)[1]){
  x=InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(ID_RFNB$nearcity[i])]
  y=as.numeric(ID_RFNB2[i,45:50])
  if(sum(is.na(y))<4){
    lm_data=data.frame(cbind(y,x))
    fit=lm(lm_data$y~0+lm_data$x,data=lm_data)
    fits[i,]=c(0,fit$coefficients[1])
  }
}
m=mean(fits$m,na.rm=T)
b=mean(fits$b,na.rm=T)

#fit <- lm(ID_RFNB$avg_RFNB ~ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
#summary(fit) # show results
fit <- lm(ID_RFNB$avg_RFNB ~0+ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
summary(fit) # show results 

#estimate for each year
inuit_data$est_RFNB2012=0
inuit_data$est_RFNB2011=0
inuit_data$est_RFNB2010=0
inuit_data$est_RFNB2009=0
inuit_data$est_RFNB2008=0
inuit_data$est_RFNB2007=0
inuit_data$est_RFNB2006=0
inuit_data$est_RFNB2005=0
for(y in 1:8){
  for(i in 1:dim(inuit_data)[1]){
    inuit_data[i,54-y]=as.numeric(#fit$coefficients[1]
                                  +fit$coefficients[1]*inuit_data$ne_dist[i]
                                  +fit$coefficients[2]*inuit_data$avg_neRFNB[i]
                                  +fit$coefficients[3]*inuit_data$nc_dist[i]
                                  +fit$coefficients[4]*inuit_data$POPULATION[i]
                                  #+fit$coefficients[6]*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])]
                                  +m*(mean(InterCity$Value[InterCity$Ref_Date==(2004+y)])-InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])])
                                  )
  }
}
#merge inuit_data with RFNB again
ID_RFNB=merge(inuit_data,RFNB,by.x="NAME",by.y="Name",all=F,incomparables = NULL)
lm_data=data.frame(RFNB=rep(NA,dim(ID_RFNB)[1]*6),est_RFNB=rep(NA,dim(ID_RFNB)[1]*6))
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    index=((y-1)*dim(ID_RFNB)[1])+i
    lm_data$RFNB[index]=ID_RFNB[i,58+y]
    lm_data$est_RFNB[index]=ID_RFNB[i,54-y]
  }
}
fit <- lm(lm_data$est_RFNB ~ lm_data$RFNB, data=lm_data)
summary(fit) # show results
plot(lm_data,xlab="Revised Northern Food Basket ($)",ylab="Estimated Revised Northern Food Basket ($)",ylim=c(250,500),xlim=c(250,500))
lines(cbind(c(0,500),c(0,500)))
abline(coef(fit))