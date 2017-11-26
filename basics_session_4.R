#######21.11.17###################################
###Raster Data in R###############################

#load all packages 
library(RStoolbox)
library(raster)

##some more handy functionality##

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

#check your data automatically
#while statement
j <- 0
while(j<1)
  {
j <- j+0.1;print(j)
}
#spatial analysis: ????????????????????????

#creating own function and being able to redo it again
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

#importance for RS = definition of certain calculations, regular operations etc. avoids duplicating code
#e.g. for NDVI
fun_ndvi <- function(nir,red) {(nir-red)/(nir+red)}

###in R three different raster representations exist:
##raster()single-layer raster
##brick()multi-layer raster from one file
##stack()multi-layer raster from separate files (same extent/resolution)

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
#stacked <- stack(c("path to your raster.tif","path to your raster.grd"))

#brick imports all bands of a single file
allbands <- brick("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif")
allbands

##creation of layer 'stacks' of data or removal of single layers
#stack images or drop one
allPlus <- stack(allbands,band_3)
#or
allPlus <- addLayer(allbands,band_3)
#or removing one layer
#allWithout <- dropLayer(allbands,#index_position_of_layer)

#after import: check and plot data
allbands#check data by calling its name
str(allbands)#check structure
#????????????#check coordinate system
#????????????#check band value statistics
inMemory(allbands)

#raster statistics#?????????????????????????????
cellStats(band_1,mean)
summary(ndvi)#statistics overview based on sample of pixels (can be changed using: maxsamp)
zonal()#zonal statistics/patch-wise analyses
quantile()#compute quantiles
freq()#count unique values(equivalent to table())


##plotting raster objects
plotRGB(allbands,3,2,1)#displaying your multi-spectral data on RGB
plotRGB(allbands,3,2,1,stretch="lin")#maybe a colour stretch is needed

#a ggplot2 option using the commands provided by package "RStools" - ggplot2 library is required
install.packages("ggplot2")
library(ggplot2)
library(raster)
library(sp)
library(RStoolbox)
ggRGB(allbands,3,2,1,stretch="lin")#RGB plot with a linear stretch
#ggR is just for one band
ggR(allbands,layer=4,stretch="hist")#single layer greyscale#was macht maxpixels??????
ggR(allbands,layer=4,maxpixels=1e6,stretch="hist")#single layer greyscale
ggR(allbands, layer=1,stretch="lin",geom_raster=TRUE)+scale_fill_gradient(low="blue",high="green")#single layer map to user defined legend

#export raster - overwrite if already existant
writeRaster(allbands,datatype='FLT4S',filename='new_raster_exampe.tif',format="GTiff",overwrite=TRUE)
getwd()#look where new file was saved

#export a picture to GoogleEarth
#only works for geographic coordinates, lat long
#KML(img,filename,col=rainbow(255),maxpixels=100000)

#to crop the data to a smaller rectangular extent use crop()
plot(band_3)
ext <- drawExtent()#draw an extent on the monitor (North-West corner and South-East corner)
band_3_crop <- crop(band_3,ext)#ext is an object of class extent
#grow and shrink extents by multiplying
ext*2#grows ext in all four directions
plot(band_3_crop)#plot ext

##raster algebra

#band calculation - using calc()
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
raster12 <- stack(allbands,allbands)#actually two different raster data (raster_1 and raster_2) for regression analysis!
fun <- function(x) {lm(x[1:5]~x[6:10])$coefficients[2]}
raster_output <- calc(raster12,fun)

#write your permanent results directly to disk when calculating them
calc(allbands,fun=sd,filename="allbands_sd.grd")

#esc ausführen bricht laufende Berechnung der Konsole ab

#raster calculation - using overlay() with two or more raster#????????????
#raster_output <- overlay(raster_1,raster_2,fun=function(x,y){return(x+y)})

#overlay with function#????????????????
#output <- overlay(input1,input2,fun=function(x1_pointing_to_input_1,x2_opinting_to_input_2){(x1-x2)})

#overlay can also be used to export data#????????????????????
#overlay(raster1,raster2,fun=functionX,filename="my_raster_output.grd")

#use more bands#?????????????
#raster_output <- overlay(raster_1,raster_2,raster_3,fun=function(x,y,z){return(x*y*z)})

##transforming raster data: reproject and resample
aggregated <- aggregate(allbands,fact=10,fun=mean)

#resample to different resolution or origin#?????????????????
#resampled <- resample(allbands,#my_target_raster,method="bilinear")

#reproject to different projection
reprojected <- reprojectRaster(allbands,crs="+proj=longlat+ellps=WGS84+datum=WGS84+no_defs",method="bilinear")

##vegetation indices in R##

#NDVI calculaion
#import all layers and point to single bands with [[]]
#lsat <- brick("path to your file name.tif")
data(lsat)
plot(lsat)
ndvi <- (lsat[[4]]-lsat[[3]])/(lsat[[4]]+lsat[[3]])
plot(ndvi)                               

#or import single band rasters and point to individual objects
ndvi <- (band_4-band_3)/(band_4+band_3)
plot(ndvi)

#if image is bigger the following way is better using overlay(): single layers(object class: RasterLayer)
ndvi <- overlay(band_4,band_3,fun=function(nir,red){(nir-red)/(nir+red)})
#calc()multilayered (object class: RasterStack or RasterBrick)
ndnvi <- calc(lsat,fun=function(x){(x[,4]-x[,3])/(x[,4]+x[,3])})

#or same function but also importing the resulting VI (Vegetation Indices) image
#savi computation with automatic data export
savi <- overlay(band_4,band_3,fun=function(nir,red){(nir-red)/(nir+red+0.5)*(1+0.5)},filename="savi.tif",format="GTiff")#soil-adjusted VI

#combine NDVI calculation with a function
fun_ndvi <- function(nir,red){(nir-red)/(nir+red)}
ndvi <- overlay(band_4,band_3,fun=fun_ndvi)

#make sure your function can digest a matrix or vector (vector for processing a single layer, matrix otherwise)
#calc(...,forcefun=TRUE)#do not loop/query every pixel individually. Vectorize! and set
#x <- lsat[1:10,]#get an example chunk and start from there

##Vegetation Indices (VI)##
#rvi <- calc(lsat,fun=function(nir,red){(nir/red)})#gleiches Ergebnis wie in nächster Zeile?
rvi <- overlay(band_4,band_3,fun=function(nir,red){nir/red})#ratio VI
plot(rvi)

msavi <- overlay(band_4,band_3,fun=function(nir,red){(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))})#modified soil-adjustet VI
plot(msavi)

#alternative command
ndvi <- spectralIndices(lsat,red="B3_dn",nir="B4_dn",indices="NDVI")
plot(ndvi)

#compute all indices using RED and NIR
VIs <- spectralIndices(lsat,red="B3_dn",nir="B4_dn")
plot(VIs)

#computing of more indices

#look into indices that use not only RED and NIR bands
