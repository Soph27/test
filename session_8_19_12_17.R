### session 8, 19.12.17 ############################
### classifications and script best practice ############

#fun#################################################################
install.packages("fun")
library(fun)
if(.Platform$OS.type=="windows")x11()else x11(type="Xlib")
mine_sweeper()
# or gomoku()

install.packages("sudoku")
library(sudoku)
playSudoku()
# or play chess
#######################################################################
install.packages("BRRR")
library(BRRR)
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("brooke-watson/BRRR")
skrrrahh("drummaboy")
skrrrahh("snoop")
skrrrahh(41) # or choose a number of sound

# create if schleife and insert sound e.g. if error occurs.....
# add a sound to a succesfully passed analysis ("yeah!")
# alternative package is beepr
#######################################################################
# gganimate allows to animate a ggplot
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
install.packages("gapminder")
library(gapminder)
library(ggplot2)
theme_set(theme_bw())

#install stand alone software imageMagick on PC, then it should work
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()
#install.packages("magick")
#library(magick)
library(gganimate)
gganimate(p)
#..........
# animate a raster stack (plots) or stats of an NDVI time series
# animate increasing accuracies with more sample points
##########################################################################################
#GEOS: 46 functions
library(rgeos)
library(raster)
# some functions of rgeos package:
gArea() # calculate area of given geometry, always in unit of given projection
gBuffer() # 
gCentroid() # get centre of geometry
gContains() # is one feature contained in the other one? (e.g. are points within polygon?)
gCrosses() # do features like lines cross?
gOverlaps() # do e.g. lines/polygons overlap?
gDistance() # distance between geometries, distance in unit of projection
gEnvelope() # 
gInterpolate() # 
gLength(trainingdaita[trainingdata$id==8,]) #
gNearestPoints() #
# Applied spatial data analysis with R: chapter 5.2 handling and combining features
# useful GIS functions are e.g. buffer, clipping
##############################################################################################
#sf
#p.52: point can never cover a polygon
##############################################################################################
### land cover change analysis ###
# method to use depends on kind of landscape you have and aim of study
# superClass() for postclassification and multidate classification
## postclassification (analysis)
#..................????????????????????????????

## multidate (analysis): don`t map land cover classes but land cover change (-->change classes)
#..................????????????????????????????

## change vector (analysis) with rasterCVA()
x88 <- brick("data/raster_data/final/p224r63_1988.gri")
x11 <- brick("data/raster_data/final/p224r63_2011.gri")

cva_88_11 <- rasterCVA(x88[[3:4]],x11[[3:4]]) # change vector analysis using RED & NIR band

# tasseledCap analysis producing greeness, wetness & brightness:
tc_88 <- tasseledCap(x88[[c(1:5,7)]],sat="Landsat5TM")
tc_11 <- tasseledCap(x11[[c(1:5,7)]],sat="Landsat5TM")

cva_tc_88_11 <- rasterCVA(tc_88[[2:3]],tc_11[[2:3]]) # change vector analysis using wetness & greeness

# plotting options
# define your own colour palette:
jet.colors_angle <- colorRampPalette(c("#00007F","blue","grey","lightgrey","red","yellow","#7FFF7F","#366C36"))
jpeg("path/to/file/angle.jpg",1000,450)
ggR(cva_angle_agg,geom_raster=T)+scale_fill_gradient(colors=jet.colors_angle(100))+theme(legend.key.size=unit(2,"cm"),legend.title=element_text(size=rel(2)),legend.text=element_text(size=rel(2)))+xlab("")+ylab("")
#dev.off()
# you can use html colours(#000000) or others
  
## --> change detection in R:
##################################################################################################
# further R packages: bfastspatial, LandsatLinkr, ShapeSelectForst, greenbrown, landTrendr, MODIStools, modis
################################################################################################
# using unicode UTF-8 code units for characters
cat(c("\u4D\u65\u72\u72\u79\u20\u43\u68\u72\u69\u73\u74\u6D\u61\u73\u0A\u74\u6F\u20\u61\u6c\u6c\u20\u45\u41\u47\u4C\u45\u20\u73\u74\u75\u64\u65\u6E\u74\u73\u21\u0A\u0A",unlist(lapply(c(1:17*2-1,rep(3,6)),function(x)
  cat(rep("\u20",(37-x)/2),".",rep("\u23", x), ".\n", sep="")))))

cat(c("Merry christmas Sophie!",unlist(lapply(c(1:17*2-1,rep(3,6)),function(x)
  cat(rep("\u20",(37-x)/2),".",rep("\u23", x), ".\n", sep="")))))
