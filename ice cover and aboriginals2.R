library(rgdal)
library(foreign)
library(PBSmapping)
library(maptools)
y=readOGR("C:/Users/Remi-Work/Documents/ArcGIS","Inuit")
inuit_data=read.dbf("C:/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")

#projection <- CRS("+proj=lcc +lon_0=-92.8667 +units=m")
#projection <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
projection = CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#projection=CRS("+proj=ups")
inuit=spTransform(y,projection)



### create circle function
circle <- function(lonlatpoint, radius) {
  travelvector <- as.data.frame(cbind(direction = seq(0, 2*pi, by=2*pi/100), magnitude = radius))
  Rearth <- 6372795
  Dd <- travelvector$magnitude / Rearth
  Cc <- travelvector$direction
  
  if (class(lonlatpoint) == "SpatialPoints") {
    lata <- coordinates(lonlatpoint)[1,2] * (pi/180)
    lona <- coordinates(lonlatpoint)[1,1] * (pi/180)
  }
  else {
    lata <- lonlatpoint[2] * (pi/180)
    lona <- lonlatpoint[1] * (pi/180)
  }
  latb <- asin(cos(Cc) * cos(lata) * sin(Dd) + sin(lata) 
               * cos(Dd))
  dlon <- atan2(cos(Dd) - sin(lata) * sin(latb), sin(Cc) 
                * sin(Dd) * cos(lata))
  lonb <- lona - dlon + pi/2
  
  lonb[lonb >  pi] <- lonb[lonb >  pi] - 2 * pi
  lonb[lonb < -pi] <- lonb[lonb < -pi] + 2 * pi
  
  latb <- latb * (180 / pi)
  lonb <- lonb * (180 / pi)
  
  cbind(longitude = lonb, latitude = latb)
}


### Calculated overlapping area Ice+ring

filelist=list.files("C:/Users/Remi-Work/Documents/ArcGIS/NSIDC Sea Ice/shapefiles",pattern=".shp")
filelist=substr(filelist,1,23)
cover=data.frame(matrix(NA,nrow=length(filelist),ncol=length(inuit_data$Long_X)))
cover$filename=cover[,1]

for(t in 1:length(filelist)){
  x=readOGR("C:/Users/Remi-Work/Documents/ArcGIS/NSIDC Sea Ice/shapefiles",layer=as.character(filelist[t]))
  ice=spTransform(x,projection)
  for(i in 1:length(inuit_data$Long_X)){
    c1 = circle(cbind(inuit_data$Long_X[i], inuit_data$Lat_Y[i]),300000)
    r1 = rbind(c1, c1[1, ])  # join
    P1 = Polygon(r1)
    Ps1 = Polygons(list(P1), ID = "a")
    Ps2 = SpatialPolygons(list(Ps1),c(1:length(Ps1)),CRS("+proj=longlat +datum=WGS84 +x_0=0 +y_0=0"))
    SPDF = SpatialPolygonsDataFrame(Ps2, data.frame(N = c("one"), row.names = c("a")))      
    ring=spTransform(SPDF,projection)  
    #plot(ice)
    #plot(ring,add=T)
    #title(paste("t=",as.character(t),"i=",as.character(i)))
    print(paste("t=",as.character(t),"i=",as.character(i)))
    ice2=combinePolys(SpatialPolygons2PolySet(ice))
    ring2=combinePolys(SpatialPolygons2PolySet(ring))
    res=try(combinePolys(joinPolys(ice2,ring2,"INT")),silent=T)
    if(class(res)=="try-error"){
      cover[t,i]=0
    }else{
      overlap=res
      cover[t,i]=sum(calcArea(overlap, rollup=1)$area)
    }

    cover$filename[t]=filelist[t]
  }
}
cover$year=substr(cover$filename,10,13)
cover$month=substr(cover$filename,14,15)




### calculate mean trend
cover$mean=cover$month
for(i in 1:length(filelist)){
  cover$mean[i]=mean(as.numeric(cover[i,1:length(inuit_data$Long_X)]))
}
plot(c(1:length(filelist)),cover$mean,"l")



### calculate yearly pattern ##################################################################################
years=unique(cover$year)[2:length(unique(cover$year))]
ice_index_byyear=data.frame(matrix(NA,nrow=length(years),ncol=length(inuit_data$Long_X)))
for(j in 1:length(inuit_data$Long_X)){
  for(i in 1:length(years)){
    index=as.numeric(cover$year)==years[i]
    ice_index_byyear[i,j]=mean(cover[index,j],na.rm=T)
  }
}
ice_index_byyear$mean=rowMeans(ice_index_byyear)
plot(years,ice_index_byyear$mean,"l")


### calculate percent cover
pcover=ice_index_byyear
for(i in 1:length(inuit_data$Long_X)){
  pcover[,i]=ice_index_byyear[,i]/max(cover[,i])
  #plot(c(1:length(filelist)),pcover[,i],"l")
}

pcover$mean=rowMeans(pcover[,1:53])
plot(years,pcover$mean,"l")

### standardize by max pcover
pcover2=ice_index_byyear
for(i in 1:length(inuit_data$Long_X)){
  pcover2[,i]=pcover2[,i]/max(pcover2[,i])
}

pcover2$mean=rowMeans(pcover2[,1:53])
plot(years,pcover2$mean,"l")







### calculate monthly pattern ########################################################################
years=unique(cover$year)[2:length(unique(cover$year))]
range=data.frame(matrix(NA,nrow=12,ncol=length(inuit_data$Long_X)))
variance=data.frame(matrix(NA,nrow=12,ncol=length(inuit_data$Long_X)))


for(m in 1:12){
  cover_m=cover[as.numeric(cover$month)==m,]
  ice_index_bymonth=data.frame(matrix(NA,nrow=length(years),ncol=length(inuit_data$Long_X)))
  for(j in 1:length(inuit_data$Long_X)){
    for(i in 1:length(years)){
      index=as.numeric(cover_m$year)==years[i]
      ice_index_bymonth[i,j]=mean(cover_m[index,j],na.rm=T)
    }
    variance[m,j]=var(ice_index_bymonth[,j],na.rm=T)
  }
  
  
  ### calculate percent cover
  pcover_m=ice_index_bymonth
  for(i in 1:length(inuit_data$Long_X)){
    pcover_m[,i]=ice_index_bymonth[,i]/max(cover[,i])
  }
  pcover_m$mean=rowMeans(pcover_m)
  plot(years,pcover_m$mean,"l")
  title(m)
  print(min(pcover_m$mean,na.rm=T)-max(pcover_m$mean,na.rm=T))
}


### calculate month of max var
var_m=variance[1,]
for(i in 1:length(inuit_data$Long_X)){
  var_m[i]=c(1:12)[variance[,i]==max(variance[,i])]
}
### calculate monthly pattern
years=unique(cover$year)[2:length(unique(cover$year))]

ice_index_bymonthmaxvar=data.frame(matrix(NA,nrow=length(years),ncol=length(inuit_data$Long_X)))
for(j in 1:length(inuit_data$Long_X)){
  for(i in 1:length(years)){
    index=as.numeric(cover$year)==years[i]&as.numeric(cover$month)==as.numeric(var_m[j])
    if(sum(index)==1){
    ice_index_bymonthmaxvar[i,j]=cover[index,j]
    }
  }
}


### calculate percent cover
pcover_mmaxvar=ice_index_bymonth
for(i in 1:length(inuit_data$Long_X)){
  pcover_mmaxvar[,i]=ice_index_bymonthmaxvar[,i]/max(cover[,i])
}
pcover_mmaxvar$mean=rowMeans(pcover_mmaxvar,na.rm=T)
plot(years,pcover_mmaxvar$mean,"l")
