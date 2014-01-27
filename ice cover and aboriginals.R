library(rgdal)
library(foreign)
x=readOGR("/media/remi/Windows/Users/Remi-Work/Documents/ArcGIS/NSIDC Sea Ice/shapefiles","extent_N_197811_polygon")
y=readOGR("/media/remi/Windows/Users/Remi-Work/Documents/ArcGIS/","Inuit")
inuit_data=read.dbf("/media/remi/Windows/Users/Remi-Work/Documents/ArcGIS/Inuit.dbf")

projection <- CRS("+proj=lcc +lon_0=-92.8667")
ice=spTransform(x,projection)
inuit=spTransform(y,projection)

library(raster)

r2 <- projectRaster(r1,crs=projection)


plot(ice,axes=T)
plot(inuit,add=T)

library(maptools)
projection <- "+proj=lcc +lon_0=-92.8667"
readShapeSpatial