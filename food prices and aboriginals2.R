#######################################################################################################################################################
## include nearest entry point, split data set for CPI and avg entry point RFNB as intercept
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
ID_RFNB$avg_RFNB=0
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

###### correct for avg_neRFNB ############
ID_RFNB$C_RFNB2010=0
ID_RFNB$C_RFNB2009=0
ID_RFNB$C_RFNB2008=0
ID_RFNB$C_RFNB2007=0
ID_RFNB$C_RFNB2006=0
ID_RFNB$C_RFNB2005=0
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    ID_RFNB[i,63-y]=ID_RFNB[i,44+y]-ID_RFNB$avg_neRFNB[i]#-m*(100-InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(ID_RFNB$nearcity[i])])
  }
}
ID_RFNB$avg_CRFNB=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$avg_CRFNB[i]=mean(as.numeric(ID_RFNB[i,57:62]),na.rm=T) 
}

#fit <- lm(ID_RFNB$avg_RFNB ~ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
#summary(fit) # show results
fit <- lm(ID_RFNB$avg_CRFNB ~0+ID_RFNB$ne_dist+ID_RFNB$nc_dist, data=ID_RFNB)
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
  if(sum(is.na(ID_RFNB[i,57:62]))>1){
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
    fit=lm(lm_data$y~lm_data$x,data=lm_data)
    fits[i,]=c(fit$coefficients[1],fit$coefficients[2])
  }
}
m=mean(fits$m,na.rm=T)
b=mean(fits$b,na.rm=T)

#fit <- lm(ID_RFNB$avg_RFNB ~ID_RFNB$ne_dist+ID_RFNB$avg_neRFNB+ID_RFNB$nc_dist+ID_RFNB$POPULATION, data=ID_RFNB)
#summary(fit) # show results
fit <- lm(ID_RFNB$avg_CRFNB ~0+ID_RFNB$ne_dist+ID_RFNB$nc_dist, data=ID_RFNB)
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
    index=InterCity$GEO==inuit_data$nearcity[i]&InterCity$Ref_Date==(2005)|InterCity$Ref_Date==(2006)|InterCity$Ref_Date==(2007)|InterCity$Ref_Date==(2008)|InterCity$Ref_Date==(2009)|InterCity$Ref_Date==(2010)
    inuit_data[i,54-y]=as.numeric(inuit_data$avg_neRFNB[i] #fit$coefficients[1]
      +fit$coefficients[1]*inuit_data$ne_dist[i]
      #+fit$coefficients[2]*inuit_data$avg_neRFNB[i]
      +fit$coefficients[2]*inuit_data$nc_dist[i]
      #+fit$coefficients[4]*inuit_data$POPULATION[i]
      #+fit$coefficients[6]*InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])]
      +m*(mean(InterCity$Value[index])-InterCity$Value[InterCity$Ref_Date==(2004+y)&InterCity$GEO==(inuit_data$nearcity[i])])
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
abline(coef(fit), lty=2)
