### session 8, 19.12.17 ############################
### classifications and script practice ############

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
if(!require(devtools)){isntall.packages(devtools)}
devtools::install_github("brooke-watson/BRRR")
skrrrahh("drummaboy")
skrrrahh("snoop")
skrrrahh(41) # or choose a number of sound

# create if schleife and insert sound e.g. if error occurs.....
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
##############################################################################################
#sf
#p.52: point can never cover a polygon
##############################################################################################
### land cover change analysis ###
# method to use depends on kind of landscape you have and aim of study

## postclassification

## multidate: don`t map land cover classes but land cover change (-->change classes)

## change vector


## --> change detection in R:
################################################################################################
# using unicode UTF-8 code units for characters
cat(c("\u4D\u65\u72\u72\u79\u20\u43\u68\u72\u69\u73\u74\u6D\u61\u73\u0A\u74\u6F\u20\u61\u6c\u6c\u20\u45\u41\u47\u4C\u45\u20\u73\u74\u75\u64\u65\u6E\u74\u73\u21\u0A\u0A",unlist(lapply(c(1:17*2-1,rep(3,6)),function(x)
  cat(rep("\u20",(37-x)/2),".",rep("\u23", x), ".\n", sep="")))))

cat(c("Merry christmas Sophie!",unlist(lapply(c(1:17*2-1,rep(3,6)),function(x)
  cat(rep("\u20",(37-x)/2),".",rep("\u23", x), ".\n", sep="")))))
