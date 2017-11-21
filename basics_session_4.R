###21.11.17###

#load all packages 
#library()....



#build your own functions to redo them again
#check your data automatically
#if statement (if something is true, do something)
a <- sqrt(2)
#if a multiplied with a is unequal to 2 then print something
if(a*a != 2)#!= means unequal to
{
print("R is great!")
}

b <- sqrt(25)
if(b*b!=25)
{
print("geht nicht")
}
#spatial analysis: if e.g. resolution is not x, then resample

#while statement
j <- 0
while(j<1)
  {
j <- j+0.1;print(j)
}

#creating own function
#add commonly used analysis to a function
#myfunction <- function(arg1,arg2, ...){
  #statements
  #return(objects)
  #}

myownfunction <- function(x,y){
  z <- x+y
  return(z)
  }

#or
myownfunction <- function(x,y){
  x+y
}

myownfunction(4,3)

#importance for RS, e.g. for NDVI
fun_ndvi <- function(nir,red) {(nir-red)/(nir+red)}

#import raster#hier wird aber nur ein Band hochgeladen, ausgabe zeigt an, dass es ursprünglich 5 Bänder waren
raster_example <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif")
raster_example

#raster imports just one band at a time:
band_1 <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif",band=1)
band_2 <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif",band=2)
band_3 <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif",band=3)
band_4 <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif",band=4)
band_5 <- raster("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif",band=5)

#combine rasters of identical dimensions from raster objects
allbands <- stack(band_1,band_2,band_3,band_4,band_5)

#...or from different files
stacked <- stack(c(""))

#brick imports all bands of a single file
allbands <- brick("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif")
allbands

#stack images or drop one
allPlus <- stack(allbands,band_3)
#or
allPlus <- addLayer(allbands,band_3)
#or removing one layer

#..................

cellStats(band_1,mean)


#plotting raster objects
plotRGB(allbands,3,2,1)
plotRGB(allbands,3,2,1,stretch="lin")

install.packages("ggplot2")
library(ggplot2)
library(raster)
library(sp)
library(RStoolbox)
ggRGB(allbands,3,2,1,stretch="lin")
ggR(allbands,layer=4,stretch="hist")#was macht maxpixels??????
ggR(allbands,layer=4,maxpixels=1e6,stretch="hist")#ggR is just for one band
ggR(allbands, layer=1,stretch="lin",geom_raster=TRUE)+scale_fill_gradient(low="blue",high="green")

#export raster - overwrite if already existant
writeRaster(allbands,datatype='FLT4S',filename='new_raster_exampe.tif',format="GTiff",overwrite=TRUE)
getwd()#look where new file was saved

#export a picture to GoogleEarth
#only works for geographic coordinates, lat lon
#KML(img,filename,col=rainbow(255),maxpixels=100000)

#crop the data to a smaller rectangular extent use crop()
plot(band_3)
ext <- drawExtent()#draw an extent on the monitor (North-West corner and South-East corner)
band_3_crop <- crop(band_3,ext)#ext is an object of class extent
#grow and shrink extents by multiplying
ext*2#grows ext in all four directions
plot(band_3_crop)#plot ext

#raster algebra
#band calculation
raster_sd <- calc(allbands,fun=sd)#calculate standard deviation
plot(raster_sd)
#adding a calculation into a function
fun <- function(x) {x/10}
raster_output <- calc(allbands,fun)
plot(raster_output)

#set NA values to -999
fun <- function(x) {x[is.na(x)] <- -999;return(x)}
raster_output <- calc(allbands,fun)
plot(raster_output)

#refer to single layers
raster_output <- calc(allbands,fun=function(x){x[1]+x[2]*x[3]})

#regression analysis
#raster_1 and raster_2 have 5 layers; coefficients[2] is the slope
raster12 <- stack(allbands,allbands)#actually two different raster data for regression analysis!
fun <- function(x) {lm(x[1:5]~x[6:10])$coefficients[2]}
raster_output <- calc(raster12,fun)

#write your permanent results directly to disk when calculating them
calc(allbands,fun=sd,filename="allbands_sd.grd")

#esc ausführen bricht laufende Berechnung der Konsole ab

#raster calculation
raster_output <- overlay(allbands)

#transforming raster data
aggregated <- aggregate(allbands,fact=10,fun=mean)


#NDVI clculaion
#lsat <- brick("")
data(lsat)
plot(lsat)
ndvi <- (lsat[[4]]-lsat[[3]])/(lsat[[4]]+lsat[[3]])
plot(ndvi)                               

ndvi <- (band_4-band_3)/(band_4+band_3)
plot(ndvi)

#if image is bigger the following way is better using overlay()
ndvi <- overlay(band_4,band_3,fun=function(nir,red){(nir-red)/(nir+red)})
ndnvi <- calc(lsat,fun=function(x){(x[,4]-x[,3])/(x[,4]+x[,3])})

#####hier wiederholen#seite 41 im pdf

#savi computation with automatic data export
savi <- overlay(band_4,band_3,fun=function(nir,red){(nir-red)/(nir+red+0.5)*(1+0.5)},filename="savi.tif",format="GTiff")

#rvi <- calc(lsat,fun=function(nir,red){(nir/red)})
#plot(rvi)

#msavi <- overlay(band_4,band_3,fun=function(nir,red){2*nir+1-sqrt((2*nir+1)^2-)})

#alternative command
ndvi <- spectralIndices(lsat,red="B3_dn",nir="B4_dn",indices="NDVI")
plot(ndvi)

#compute all indices using RED and NIR
VIS <- spectralIndices(lsat,red="B3_dn",nir="B4_dn")#????????mit welchen daten ausprobieren
plot(VIS)
