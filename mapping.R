library(marmap)
library(maps)

getNOAA.bathy(lon1 = -150, lon2 = -40, lat1 = 40, lat2 = 90,resolution = 10) -> Canada
#detach("package:marmap", unload=TRUE)
summary(Canada)
r1 <- as.raster(Canada)
projection <- "+proj=lcc +lon_0=-92.8667"
library(raster)

r2 <- projectRaster(r1,crs=projection)
as.bathy(r2) -> Canada2

plot.bathy(Canada2, image=T,bpal=gray(c(0.7,0.5,0)),deep = c(-9000, -300, 0), shallow = c(-300, 0, 0),step = c(9000, 300, 0), lwd = c(0.8, 0.8, 1),col = c("lightgrey", "darkgrey", "black"),lty = c(1, 1, 1), drawlabel = c(FALSE, FALSE, FALSE))

p <- rasterToPolygons(r1, dissolve=TRUE)
library(rgdal)
writeOGR(p,"C:/Users/Remi-Work/Documents/ArcGIS/bathy","testShape",driver="ESRI Shapefile",)