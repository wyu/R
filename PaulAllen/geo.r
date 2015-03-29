require(RgoogleMaps)
library(sp)
library(dismo)

df$geocode_string = with(df, paste(city, state, country, sep = ", "))
df$geocode_string = "Seattle, WA, USA"
coords_latlong = geocode(df$geocode_string)
geocode("Gopalpur, India")
geocode("Mumbai, India")
geocode("New  Delhi, India")


#define the part of the world you want to plot. Here the area around my former home.
lat_c<-51.47393
lon_c<-7.22667
bb<-qbbox(lat = c(lat_c[1]+0.01, lat_c[1]-0.01), lon = c(lon_c[1]+0.03, lon_c[1]-0.03))

lat=c(48,64) 
lon=c(-140,-110) 
center=c(mean(lat), mean(lon))
zoom=5
terrmap=GetMap(center=center, zoom=zoom, maptype="terrain", destfile = "terrain.png")
tmp <- PlotOnStaticMap(terrmap, lat = NA, lon = NA,
                       col=c( 'red','blue','green'), add=FALSE)

data("meuse", package = "sp", envir = environment())


m<-bubbleMap(meuse,zcol='zinc');
# download the tile from OSM
OSM.map<-GetMap.OSM(lonR=bb$lonR, latR=bb$latR, scale = 20000, destfile="bochum.png")
image(OSM.map)
#Add some coordinates
lat<- c(51.47393, 51.479021)
lon<- c(7.22667, 7.222526)
val <- c(0, 255)

#function to adjust the coordinates
lat_adj<-function(lat, map){(map$BBOX$ll[1]-lat)/(map$BBOX$ll[1]-map$BBOX$ur[1])}
lon_adj<-function(lon, map){(map$BBOX$ll[2]-lon)/(map$BBOX$ll[2]-map$BBOX$ur[2])}

PlotOnStaticMap(OSM.map, lat = lat_adj(lat, OSM.map), lon = lon_adj(lon, OSM.map), col=rgb(255,0, val,90,maxColorValue=255),pch=16,cex=4)

dev.print(jpeg,"test.jpeg", width=1204, height=644, units="px")