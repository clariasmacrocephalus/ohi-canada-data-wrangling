#######################################################################################################################################################
## include nearest entry point, split data set for CPI and avg entry point RFNB as intercept
#######################################################################################################################################################
### Food prices ###
library(foreign)
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")

RFNB=read.csv("RFNB.csv")
InterCity=read.csv("03260015-eng.csv")
CPI=read.csv("03260021-eng.csv")
CPI=CPI[CPI$COMM=="Food",]
InterCity=InterCity[InterCity$COMMODITY=="Food",]
CPI=CPI[CPI$COMM=="Food",]
cityLL=read.csv("InterCity_LL.csv")

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
ID_RFNB$avg_ICCPI=0
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
  ID_RFNB$avg_ICCPI[i]=mean(as.numeric(InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(ID_RFNB$nearcity[i])]))
}

#split data set
ID_RFNB2=ID_RFNB
ID_RFNB2[,]=NA
ID_RFNB1=ID_RFNB2
for(i in 1:dim(ID_RFNB)[1]){
  if(sum(is.na(ID_RFNB[i,45:50]))>1){
    ID_RFNB1[i,]=ID_RFNB[i,]
  } else {
    ID_RFNB2[i,]=ID_RFNB[i,]
  }
}
index=CPI$GEO=="Canada"&CPI$Ref_Date==(2005)|CPI$Ref_Date==(2006)|CPI$Ref_Date==(2007)|CPI$Ref_Date==(2008)|CPI$Ref_Date==(2009)|CPI$Ref_Date==(2010)
baseCPI=mean(CPI$Value[index])
fits=data.frame(b=rep(NA,dim(ID_RFNB)[1]),m=rep(NA,dim(ID_RFNB)[1]))
for(i in 1:dim(ID_RFNB)[1]){
  x=CPI$Value[CPI$Ref_Date>=2005&CPI$Ref_Date<=2010&CPI$GEO=="Canada"]-baseCPI
  y=as.numeric(ID_RFNB2[i,45:50])
  if(sum(is.na(y))<4){
    lm_data=data.frame(cbind(y,x))
    fit=lm(lm_data$y~lm_data$x,data=lm_data)
    #plot(x,y)
    #abline(coef(fit))
    fits[i,]=c(fit$coefficients[1],fit$coefficients[2])
  }
}

b=mean(fits$b,na.rm=T)
m=mean(fits$m,na.rm=T)

###### correct for temporal National level CPI ############

ID_RFNB$C_RFNB2010=0
ID_RFNB$C_RFNB2009=0
ID_RFNB$C_RFNB2008=0
ID_RFNB$C_RFNB2007=0
ID_RFNB$C_RFNB2006=0
ID_RFNB$C_RFNB2005=0
for(y in 1:6){
  for(i in 1:dim(ID_RFNB)[1]){
    ID_RFNB[i,64-y]=ID_RFNB[i,44+y]-m*(CPI$Value[CPI$Ref_Date==(2004+y)&CPI$GEO=="Canada"]-baseCPI)   
  }
}

ID_RFNB$avg_CRFNB=0
for(i in 1:dim(ID_RFNB)[1]){
  ID_RFNB$avg_CRFNB[i]=mean(as.numeric(ID_RFNB[i,58:63]),na.rm=T) 
}

fit <- lm(avg_CRFNB ~avg_neRFNB+ne_dist+nc_dist+POPULATION+avg_ICCPI, data=ID_RFNB)
summary(fit) # show results
AIC(fit)
#fit <- lm(avg_CRFNB ~avg_neRFNB+ne_dist+nc_dist+avg_ICCPI, data=ID_RFNB)
#summary(fit) # show results
#AIC(fit)
#fit <- lm(avg_CRFNB ~avg_neRFNB+ne_dist+avg_ICCPI, data=ID_RFNB)
#summary(fit) # show results
#AIC(fit)
#fit <- lm(avg_CRFNB ~avg_neRFNB+ne_dist+nc_dist, data=ID_RFNB)
#summary(fit) # show results
#AIC(fit)
fit2 <- lm(avg_CRFNB ~nc_dist+POPULATION+avg_ICCPI, data=ID_RFNB)
summary(fit) # show results
AIC(fit)

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
  inuit_data$avg_ICCPI[i]=mean(as.numeric(InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(ID_RFNB$nearcity[i])]))
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
    inuit_data[i,55-y]=as.numeric(fit$coefficients[1]
                                  +fit$coefficients[2]*inuit_data$avg_neRFNB[i]
                                  +fit$coefficients[3]*inuit_data$ne_dist[i]
                                  +fit$coefficients[4]*inuit_data$nc_dist[i]
                                  +fit$coefficients[5]*inuit_data$POPULATION[i]
                                  +fit$coefficients[6]*inuit_data$avg_ICCPI[i]
                                  +m*(CPI$Value[CPI$GEO=="Canada"&CPI$Ref_Date==(2004+y)]-baseCPI)
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
fit3 <- lm(lm_data$est_RFNB ~ lm_data$RFNB, data=lm_data)
summary(fit3) # show results
plot(lm_data,xlab="Revised Northern Food Basket ($)",ylab="Estimated Revised Northern Food Basket ($)",ylim=c(250,500),xlim=c(250,500))
lines(cbind(c(0,500),c(0,500)))
abline(coef(fit3), lty=2)

################################################# estimate First Nations RFNB prices from fit2 ############################
FN_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/FirstNations.dbf")
CPI=read.csv("~/Documents/R/OHI/03260021-eng.csv")
CPI=CPI[CPI$COMM=="Food",]
gas=read.csv("~/Documents/R/OHI/03260009-eng.csv")
gas$year=as.numeric(substr(gas$Ref_Date, 1, 4))
#limit First Nations communities to 300km from shore
FN_data=FN_data[FN_data$Distance<300000,]
#calculate distance from nearest "intercity index"
library(fields)
distmat=rdist.earth(cbind(FN_data$Long_X,FN_data$Lat_Y),cbind(cityLL$Longitude,cityLL$Latitude),miles=F)
FN_data$nearcity=0
FN_data$nc_dist=0
ID_RFNB$avg_ICCPI=0
for(i in 1:dim(FN_data)[1]){
  FN_data$nearcity[i]= as.character(cityLL$InterCity[(distmat==apply(distmat,1,function(x) min(x)))[i,]])
  index=cityLL$InterCity==FN_data$nearcity[i]
  FN_data$nc_dist[i]=rdist.earth(cbind(FN_data$Long_X[i],FN_data$Lat_Y[i]),cbind(cityLL$Longitude[index],cityLL$Latitude[index]),miles=F) 
  FN_data$avg_ICCPI[i]=mean(as.numeric(InterCity$Value[InterCity$Ref_Date>=2005&InterCity$Ref_Date<=2010&InterCity$GEO==(FN_data$nearcity[i])]))
}
for(yi in (1979:2013)-1978){
  
  inuit_data$est_RFNByi=0
  inuit_data$gas=0
  for(i in 1:dim(inuit_data)[1]){
    inuit_data$est_RFNByi[i]=as.numeric(fit$coefficients[1]
                                  +fit$coefficients[2]*inuit_data$avg_neRFNB[i]
                                  +fit$coefficients[3]*inuit_data$ne_dist[i]
                                  +fit$coefficients[4]*inuit_data$nc_dist[i]
                                  +fit$coefficients[5]*inuit_data$POPULATION[i]
                                  +fit$coefficients[6]*inuit_data$avg_ICCPI[i]
                                  +m*(CPI$Value[CPI$GEO=="Canada"&CPI$Ref_Date==(yi+1978)]-baseCPI)
    )
    inuit_data$gas[i]=mean(gas$Value[gas$year==yi+1978&gas$GEO==inuit_data$nearcity[i]])
  }
  FN_data$est_RFNByi=0
  FN_data$gas=0
  for(i in 1:dim(FN_data)[1]){
    FN_data$est_RFNByi[i]=as.numeric(fit2$coefficients[1]
                                        +fit2$coefficients[2]*FN_data$nc_dist[i]
                                        +fit2$coefficients[3]*as.numeric(FN_data$REGISTERED[i])
                                        +fit2$coefficients[4]*FN_data$avg_ICCPI[i]
                                        +m*(CPI$Value[CPI$GEO=="Canada"&CPI$Ref_Date==(yi+1978)]-baseCPI)
    )
    FN_data$gas[i]=mean(gas$Value[gas$year==yi+1978&gas$GEO==FN_data$nearcity[i]])
  }
  if(yi==1){
    final_data=data.frame(cbind(name=c(as.character(inuit_data$NAME),as.character(FN_data$BAND_NAME)),
                        type=c(rep("Inuit",length(inuit_data$NAME)),rep("First Nations",length(FN_data$BAND_NAME))),
                        latitude=c(inuit_data$Lat_Y,FN_data$Lat_Y),
                        longitude=c(inuit_data$Long_X,FN_data$Long_X),
                        population=c(inuit_data$POPULATION,FN_data$REGISTERED),
                        year=rep(yi+1978,length(inuit_data$NAME)+length(FN_data$BAND_NAME)),
                        gas=c(inuit_data$gas,FN_data$gas),
                        est_RFNB=c(inuit_data$est_RFNB,FN_data$est_RFNB)
                        ))
  } else {
    final_data=rbind(final_data,data.frame(cbind(name=c(as.character(inuit_data$NAME),as.character(FN_data$BAND_NAME)),
                                type=c(rep("Inuit",length(inuit_data$NAME)),rep("First Nations",length(FN_data$BAND_NAME))),
                                latitude=c(inuit_data$Lat_Y,FN_data$Lat_Y),
                                longitude=c(inuit_data$Long_X,FN_data$Long_X),
                                population=c(inuit_data$POPULATION,FN_data$REGISTERED),
                                year=rep(yi+1978,length(inuit_data$NAME)+length(FN_data$BAND_NAME)),
                                gas=c(inuit_data$gas,FN_data$gas),
                                est_RFNB=c(inuit_data$est_RFNB,FN_data$est_RFNB))
    ))
  }
  print(yi+1978)
}
write.csv(final_data,'~/Documents/R/OHI/aboriginal_food_gas.csv')

mean(CPI$Value[CPI$Ref_Date==1979])
mean(CPI$Value[CPI$Ref_Date==2013])
1/(mean(gas$Value[gas$year==1979])/mean(gas$Value[gas$year==2013]))
1/(mean(CPI$Value[CPI$Ref_Date==1979])/mean(CPI$Value[CPI$Ref_Date==2013]))

1/(mean(gas$Value[gas$year==1979])/mean(gas$Value[gas$year==1981]))
1/(mean(CPI$Value[CPI$Ref_Date==1979])/mean(CPI$Value[CPI$Ref_Date==1981]))

1/(mean(gas$Value[gas$year==1981])/mean(gas$Value[gas$year==1999]))
1/(mean(CPI$Value[CPI$Ref_Date==1981])/mean(CPI$Value[CPI$Ref_Date==1999]))


