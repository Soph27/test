###homework 21_11_17################################################################
###Applied Spatial Data Analysis with R, Chapter 2: Classes for Spatial Data in R###


install.packages("RStoolbox")
library('RStoolbox')
install.packages("car")
library(car)
install.packages("raster")
library(raster)
#install.packages("ctv")
#install.views("Spatial")
#library(ctv)
library(sp)

##2.2_Classes and methods in R##
#R as a calculator

pi*10^2 # circular area

#arithmetic is translated (parsed) into functions (operators) with arguments
"*"(pi,"^"(10,2))

#use vectors of values 
pi*(1:10)^2 # generation of integer sequence of values

#show outcome
x <- pi*10^2 # use of assignment operator
x

print(x) # print of x with default number of digits

print(x,digits=12) 

#show class of object in x
class(x)

#show storage mode of object in x
typeof(x)

#object class determines method, if there is no specific method: default method/generic function
print(x)
plot(x)
summary(x) #key features of object

#non-spatial example from standard dataset "cars"
class(cars)
typeof(cars)

#data.frame has both names and summary methods
names(cars)
summary(cars)
data.frame(cars)

#get information on object
ls(cars)
str(cars)

#formula abstraction
class(dist~speed) # response variable ~ determining variable

#formula usually used as first argument to model fitting functions
lm(dist~speed,data=cars) # linear model

#create factor for speed by cutting into quartiles
cars$qspeed <- cut(cars$speed,breaks=quantile(cars$speed),include.lowest=TRUE) # include.lowest=TRUE nimmt kleinsten Wert in Intervalle mit rein; =FALSE schreibt für kleinsten Wert NA
is.factor(cars$qspeed)
cars

#contrast how plot method displays relationship
plot(dist~speed,data=cars) # between two numerical variables
# ??? plot(dist~qspeed,date=cars) # between a numerical variable and a factor

lm(qspeed~dist,data=cars)

##2.3_Spatial Objects## p.28
library(sp)
getClass("Spatial") # see complete definition of a class (including slot names & types of their content)

getClass("CRS")

#building of a simple spatial object from a bounding box matrix and a missing coordinate reference system
m <- matrix(c(0,0,1,1),ncol=2,dimnames=list(NULL,c("min","max")))
m
crs <- CRS(projargs=as.character(NA))
crs

S <- Spatial(bbox=m,proj4string=crs)
S

#???????????????????????????????????????????????????????????????????????
bb <- matrix(c(350,85,370,95),ncol=2,dimnames=list(NULL,c("min","max")))
bb
Spatial(bb,proj4string=CRS("+proj=longlat"))
# no error appears, why??????????????????????????????????????????????????

##2.4_Spatial Points## p.30

CRAN_df <- read.table("CRAN051001a.txt",header=TRUE)
CRAN_mat <- cbind(CRAN_df$long,CRAN_df$lat) # extract columns long and lat into a matrix
row.names(CRAN_mat) <- 1:nrow(CRAN_mat) # name rows as numbers
CRAN_mat
str(CRAN_mat) # view a digest

getClass("SpatialPoints") # SpatialPoints adds a coords slot to Spatial class; matrix of point coordinates can be inserted

llCRS <- CRS("+proj=longlat +ellps=WGS84") # set projection
CRAN_sp <- SpatialPoints(CRAN_mat,proj4string=llCRS) # matrix with point coordinates
summary(CRAN_sp)

##2.4.1_methods
bbox(CRAN_sp) # first row(west-east range), 2nd (south-north direction)

proj4string(CRAN_sp) # reports projection string contained as crs object in proj4string slot of object
proj4string(CRAN_sp) <- CRS(as.character(NA)) # alter projection
proj4string(CRAN_sp)
proj4string(CRAN_sp) <- llCRS

brazil <- which(CRAN_df$loc=="Brazil") # extract coordinates as numeric matrix # index is used to choose subsets
brazil

coordinates(CRAN_sp)[brazil,] # extract coordinates for Brazil
summary(CRAN_sp[brazil,]) # see summary of extracted parts

south_of_equator <- which(coordinates(CRAN_sp)[,2]<0) # define part of CRAN_sp coordinates
summary(CRAN_sp[-south_of_equator,]) # remove coordinates from object

##2.4.2_Data Frames  for Spatial Point Data## p.33
str(row.names(CRAN_df))

#making our SpatialPointsDataFrame object from a matrix of coordinates and a data frame with or without ID checking
#associate the correct rows of data frame object with their point coordinates
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat,CRAN_df,proj4string=llCRS,match.ID=TRUE) # TRUE=matrix row names are checked against dataframe row names
CRAN_spdf1[4,] # return new SpatiaPointsDataFrame with requested row

str(CRAN_spdf1$loc) # return data frame column
str(CRAN_spdf1[["loc"]]) # return data frame column

s <- sample(nrow(CRAN_df)) # random sample of data frame
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat,CRAN_df[s,],proj4string=llCRS,match.ID=TRUE) # re-order df at random using sample
all.equal(CRAN_spdf2,CRAN_spdf1) # compare objects and test equality
CRAN_spdf2[4,] # same result as df is re-ordered to match the row names of the points

# will not work p.34
CRAN_df1 <- CRAN_df # non matching ID values (created by pasting pairs of letters together)
row.names(CRAN_df1) <- sample(c(outer(letters,letters,paste,sep="")),nrow(CRAN_df1))
CRAN_spdf3 <- SpatialPointsDataFrame(CRAN_mat,CRAN_df1, proj4string=llCRS,match.ID=TRUE)

getClass("SpatialPointsDataFrame") # extends SpatialPoint Class by data (data frame) & coords.nrs (numeric)
# behave as far as possible like data frames, with methods like names and functions like model.frame
names(CRAN_spdf1)

str(model.frame(lat~long,data=CRAN_spdf1),give.attr=FALSE) # model.function returns df with variables relevant for formula use
# give.attr=TRUE shows attributes as sub structures

#give SpatialPointsDataFrame function a SpatialPoints object as its first argument
CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp,CRAN_df)
all.equal(CRAN_spdf4,CRAN_spdf2)

#assign coordinates to a df (modifies original df)
CRAN_df0 <- CRAN_df
coordinates(CRAN_df0) <- CRAN_mat # matrix of coordinates with same number of rows
proj4string(CRAN_df0) <- llCRS
all.equal(CRAN_df0,CRAN_spdf2)

str(CRAN_df0,max.level=2)
#coordinates are moved from data to coords slot (different to creation of SPDF before)
#but otherwise objects are the same:
CRAN_df1 <- CRAN_df
names(CRAN_df1)

coordinates(CRAN_df1) <- c("long","lat")
proj4string(CRAN_df1) <- llCRS
str(CRAN_df1,max.level=2)

#example: loggerhead turtle crossing pacific from Mexico to Japan
turtle_df <- read.csv("seamap105_mod.csv")
summary(turtle_df)

#?????????????????????????????????????????????????????????????????????????????????????????????
#timestamp the observations and re-order input df by timestamp
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date),"%m/%d/%Y %H:%M:%S"),"GMT")
turtle_df1 <- data.frame(turtle_df,timestamp=timestamp)
turtle_df1$lon <- ifelse(turtle_df1$lon<0,turtle_df1$lon+360,turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]
coordinates(turtle_sp) <- c("lon","lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")

##2.5_Spatial Lines## p.37

#list of line objects forms the lines slot of a lines object
getClass("Line")
getClass("Lines")
getClass("SpatialLines")

install.packages("maps") # maps package world database
library(maps)
japan <- map("world","japan",plot=FALSE)
p4s <- CRS("+proj=longlat +ellps=WGS84")

install.packages("maptools")
library(maptools)
SLjapan <- map2SpatialLines(japan,proj4string=p4s) # conversion to SpatialLines object
str(SLjapan,max.level=2)

#return length of Lines slot (how many Line objects it contains) of each Lines object in list in lines slot of SLjapan, simplifying result to a numeric vector
Lines_len <- sapply(slot(SLjapan,"lines"),function(x) length(slot(x,"Lines")))
#use of lapply results in a list
table(Lines_len) # no lines object contains more than 1 line object

volcano_s1 <- ContourLines2SLDF(contourLines(volcano)) # conversion to SpatialLinesDataFrame
volcano_s1
t(slot(volcano_s1,"data")) # t(x) returns the transponse (transponierte matrix) of x
#result: 10 separate contour level labels stored in 'data' slot

llCRS <- CRS("+proj=longlat +ellps=WGS84")
auck_shore <- MapGen2SL("auckland_mapgen.dat",llCRS) # read shoreline data in mapgen format directly into SpatialLines object
summary(auck_shore)

##2.6_Spatial Polygons## p.41

lns <- slot(auck_shore,"lines") # identify lines imported above (represent shoreline around auckland)
#as many are islands, they have identical first and last coordinates
table(sapply(lns,function(x) length(slot(x,"Lines"))))
islands_auck <- sapply(lns,function(x){
  crds <- slot(slot(x,"Lines")[[1]],"coords")
  identical(crds[1,],crds[nrow(crds),])
})
table(islands_auck)

getClass("Polygon")
getClass("Polygons")
getClass("SpatialPolygons")

#build SpatialPolygons object by choosing only lines which are closed polygons
islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl,"lines") # build list of polygon objects for each polygons object
#pass list of these polygons objects to SpatialPolygons function to create SpatialPolygons object
islands_sp <- SpatialPolygons(lapply(list_of_Lines,function(x){Polygons(list(Polygon(slot(slot(x,"Lines")[[1]],"coords"))),ID=slot(x,"ID"))}),proj4string=CRS("+proj=longlat +ellps=WGS84"))
summary(islands_sp)

slot(islands_sp,"plotOrder")
order(sapply(slot(islands_sp,"polygons"),function(x)slot(x,"area")),decreasing=TRUE)

##2.6.1_SpatialPolygonsDataFrame Objects## p.44 'ab hier bis 2.6.2.s????????????????????????????????????????????????
library(maps)
state.map <- map("state",plot=FALSE,fill=TRUE)
IDs <- sapply(strsplit(state.map$names,":"),function(x) x[1])
library(maptools)
#convert boundaries to SpatialPolygons object:
state.sp <- map2SpatialPolygons(state.map,IDs=IDs,proj4string=CRS("+proj=longlat +ellps=WGS84"))

#identifying tag matching to suit df rows to SpatialPolygons
sat <- read.table("state.sat.data_mod.txt",row.names=5,header=TRUE)
str(sat)

id <- match(row.names(sat),row.names(state.sp))
row.names(sat)[is.na(id)] # subset to matched rows of data frame

sat1 <- sat[!is.na(id),]
state.spdf <- SpatialPolygonsDataFrame(state.sp,sat1)
str(slot(state.spdf,"data"))

str(state.spdf,max.level=2)

rownames(sat1)[2] <- "Arizona" # modification of rowname arizona to Arizona
SpatialPolygonsDataFrame(state.sp, sat1) # no match with a polygon identifying tag results in an error

#drop district of columbia'??????????????????????????????????????????????????ß
DC <- "district of columbia"
not_dc <- !(row.names(state.spdf)==DC)
state.spdf1 <- state.spdf[not_dc,]
dim(state.spdf1)
summary(state.spdf1)

##2.6.2_Holes and Ring Direction## p.46 ????????????????????????????????????????????????

MI <- load("high.RData")
manitoulin_sp <- high[[4]]

length(slot(manitoulin_sp,"polygons"))
# checking for holes: hole slot (true/false):
sapply(slot(slot(manitoulin_sp,"polygons")[[1]],"Polygons"),function(x)slot(x,"hole"))
#checking for holes: ring direction (anti-clockwise=hole):
sapply(slot(slot(manitoulin_sp,"polygons")[[1]],"Polygons"),function(x)slot(x,"ringDir"))

library(rgeos)
manitoulin_sp <- createSPComment(manitoulin_sp)
sapply(slot(manitoulin_sp,"polygons"),comment) # assign hole status: 0 for exterior rings

##2.7_SpatialGrid and SpatialPixel Objects##

getClass("GridTopology")

# create GridTopology object from bounding box
bb <- bbox(manitoulin_sp) # bbox of Manitoulin Island vector dataset
bb

cs <- c(0.01,0.01) # cell size of 0.01° in each direction
cc <- bb[1,]+(cs/2) # offset south-west cell centre
cd <- ceiling(diff(t(bb))/cs) # find suitable number of cells in each direction
manitoulin_grd <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=cd) # create GridTopology object
manitoulin_grd

getClass("SpatialGrid")
#SpatialGrid object contains GridTopology and Spatial objects
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd,proj4string=p4s)
summary(manitoulin_SG)

library(rgdal)
auck_el1 <- readGDAL("70042108.tif") # read data from Geotiff into SpatialGridDataFrame object
class(auck_el1) # check class of data object
slot(auck_el1,"grid") # examine the grid slot

slot(auck_el1,"bbox")
object.size(auck_el1)
object.size(slot(auck_el1,"data")) # data slot makes up most of total size

is.na(auck_el1$band1) <- auck_el1$band1<=0 # data below sea level are set to NA
summary(auck_el1$band1)

auck_el2 <- as(auck_el1,"SpatialPixelsDataFrame") #?????????????????????????? von hier
object.size(auck_el2)
object.size(slot(auck_el2,"grid.index"))
object.size(slot(auck_el2,"coords"))
sum(is.na(auck_el1$band1))+nrow(slot(auck_el2,"coords"))

prod(slot(slot(auck_el2,"grid"),"cells.dim"))

auck_el_500 <- auck_el2[auck_el2$band1>500,]
summary(auck_el_500)

object.size(auck_el_500) #?????????????????????????????????????????????????????? bis hier

data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x,meuse.grid$y))
summary(mg_SP)

mg_SPix0 <- SpatialPixels(mg_SP) # pass SpatialPoints object to SpatialPixels function
summary(mg_SPix0)
prod(slot(slot(mg_SPix0,"grid"),"cells.dim")) # product  of cell dimensions of underlying grid
#over half of full grid is not present in SpatialPixels representation (many grid cells lie outide of study area)

mg_SPix1 <- as(mg_SP,"SpatialPixels") # alternative: coerce SpatialPoints to SpatialPixels object
summary(mg_SPix1)

##2.8_Raster Objects and the raster Package## p.54
library(raster)
r <- raster("70042108.tif") # creation of RasterLayer object (by passing name of raster file to the raster function)
class(r)
inMemory(r)
fromDisk(r)
object.size(r)
cellStats(r,max)
cellStats(r,min)
inMemory(r)

#remove values of less than or equal to zero
out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out,filename=tempfile(),overwrite=TRUE)
for(i in 1:bs$n) {
  v <- getValues(r,row=bs$row[i],nrows=bs$nrows[i])
  v[v<=0] <- NA
  writeValues(out,v,bs$row[i])
}
out <- writeStop(out)
cellStats(out,min)
cellStats(out,max)
inMemory(out)

plot(out,col=terrain.colors(100))

#RasterLayer object can be coerced (zwingen) to SpatialGridDataFrame and back again
r1 <- as(out,"SpatialGridDataFrame")
summary(r1)

r2 <- as(r1,"RasterLayer")
summary(r2)








