###homework 06_12_17#################################################################
###Applied Spatial Data Analysis with R, Chapter 4: Spatial Data Import and Export###
library(rgdal)

###4.1 Coordinate Reference Systems### p.84
NEWS <- "http://svn.osgeo.org/metacrs/proj/trunk/proj/NEWS"
PROJ4_NEWS <- readLines(url(NEWS))
lns <- grep("Release Notes|EPSG", PROJ4_NEWS)
head(PROJ4_NEWS[lns])

##4.1.1 Using the EPSG List## p.85
EPSG <- make_EPSG()
EPSG[grep("^# ED50$", EPSG$note), ]

##4.1.2 PROJ.4 CRS Specification## p.86
CRS("+init=epsg:4230")
ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0")
ED50

##4.1.3 Projection and Transformation## p.88
# in this case, input & output coordinates are geographical, hence no need to project:
IJ.east <- as(char2dms("4d31'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x = IJ.east, y = IJ.north),proj4string = ED50)
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
x <- as(dd2dms(coordinates(res)[1]), "character")
y <- as(dd2dms(coordinates(res)[2], TRUE), "character")
cat(x, y, "\n")

# measure distance between matrix of points and a single point; it returns values in km so *1000 to obtain m:
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) *1000 # longlat=TRUE means Great Cirlce distances on the WGS84

library(maptools)
# calculate azimuths on sphere between matrix of points and single point (must be geographic coordinates with north zero & negative azimuths west of north):
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

proj4string(IJ.ED50) <- CRS("+init=epsg:4230")
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) *1000
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

# US National Atlas chose a particular CRS for its view of continental US:
EPSG[grep("Atlas", EPSG$note), 1:2]
CRS("+init=epsg:2163")
# projection has value "laea"

proj <- projInfo("proj") # return several kinds of information in tabular form
proj[proj$name == "laea", ]
# CRS is in Lambert Azimuthal Equal Area Projection

ellps <- projInfo("ellps")
ellps[grep("a=6370997", ellps$major), ]

##4.1.4 Degrees, Minutes, and Seconds## p.90
# convert geographical coordinates to decimal degree form:
IJ.dms.E <- "4d31'00\"E"
IJ.dms.N <- "52d28'00\"N"
# convert these caracter strings to class 'DMS' objects:
IJ_east <- char2dms(IJ.dms.E)
IJ_north <- char2dms(IJ.dms.N)
IJ_east
IJ_north
getSlots("DMS")
# get decimal degrees by coercing from class 'DMS' to class 'numeric' with as():
c(as(IJ_east, "numeric"), as(IJ_north, "numeric"))

##4.2 Vector File Formats## p.91
##4.2.1 Using OGR Drivers in rgdal## p.92
head(ogrDrivers(), n = 10)
vignette("OGR_shape_encoding", package = "rgdal")
scot_dat <- read.table("C:\\Users\\Sophie\\Documents\\test\\scotland.txt", skip = 1)
names(scot_dat) <- c("District", "Observed", "Expected","PcAFF", "Latitude", "Longitude")
getwd()
setwd("C:\\Users\\Sophie\\Documents\\test")
ogrInfo(".","scot") # show summary of layer; "." means current working directory
scot_LL <- readOGR(dsn = ".", layer = "scot") # read/ import from working directory
proj4string(scot_LL) # see if data has coordinate reference system (here: NA)
proj4string(scot_LL) <- CRS("+proj=longlat +ellps=WGS84") # assign suitabe coordinate reference system
sapply(slot(scot_LL, "data"), class)
scot_LL$ID
scot_dat$District

# match data ID and district to combine them with imported polygons:
ID_D <- match(scot_LL$ID, scot_dat$District)
scot_dat1 <- scot_dat[ID_D, ]
row.names(scot_dat1) <- row.names(scot_LL)
library(maptools)
scot_LLa <- spCbind(scot_LL, scot_dat1) # combine data
all.equal(scot_LLa$ID, scot_LLa$District)
names(scot_LLa)

install.packages("spdep")
library(spdep)
O <- scot_LLa$Observed
E <- scot_LLa$Expected
scot_LLa$SMR <- probmap(O, E)$relRisk/100
install.packages("DCluster")
library(DCluster)
scot_LLa$smth <- empbaysmooth(O, E)$smthrr
# project district boundaries to British National Grid:
scot_BNG <- spTransform(scot_LLa, CRS("+init=epsg:27700"))

# export projected scottish data set (including added results) to a shapefile using driver="ESRI Shapefile":
drv <- "ESRI Shapefile"
writeOGR(scot_BNG, dsn = ".", layer = "scot_BNG", driver = drv)
list.files(pattern = "^scot_BNG")
# output now contains a .prj file with fully secified coordinate reference system for British National Grid (to which we projected the data object)

load("geohub.RData")
dsn <- "WFS:http://geohub.jrc.ec.europa.eu/effis/ows"
#ogrListLayers(dsn) # check for available layers # geht nicht mehr, Daten m?ssen gedownloaadet und reingeladen werden:
Fires <- readOGR(".", "fires_120104")
names(Fires)

# create a bounding box covering the relevant parts of Europe:
x <- c(-15, -15, 38, 38, -15)
y <- c(28, 62, 62, 28, 28)
crds <- cbind(x = x, y = y)
bb <- SpatialPolygons(list(Polygons(list(Polygon(coords = crds)),"1")))
# and make a spatial selection of only coastlines and national boundaries from wrld_simpl:
library(maptools)
data(wrld_simpl)
proj4string(bb) <- CRS(proj4string(wrld_simpl))
# falling within the box with gIntersection:
library(rgeos)
slbb <- gIntersection(bb, as(wrld_simpl, "SpatialLines")) # gIntersection=binary topological operator
spl <- list("sp.lines", slbb, lwd = 0.7, col = "khaki4")

# convert the input fire date to a Date object:
Fires$dt <- as.Date(as.character(Fires$FireDate), format = "%d-%m-%Y")
Fires0 <- Fires[-which(coordinates(Fires)[, 2] < 0),] # discard (wegwerfen) any incidents on R?union
Fires1 <- Fires0[order(Fires0$dt),]
install.packages("spacetime")
library(spacetime)
Fires2 <- STIDF(as(Fires1, "SpatialPoints"), Fires1$dt,as(Fires1, "data.frame"))
stplot(as(Fires2,"STI"),number = 3, sp.layout = spl, cex = 0.5) # plot 3 incident maps, conditioned by time quantiles

names(Fires1)[1] <- "name" # re-name the ID column
GR_Fires <- Fires1[Fires1$Country == "GR", ]
writeOGR(GR_Fires, "EFFIS.gpx", "waypoints", driver = "GPX",dataset_options = "GPX_USE_EXTENSIONS=YES")

GR <- readOGR("EFFIS.gpx", "waypoints") # show retrieved values for first incident
GR[1, c(5, 24:28)]

##4.2.2 other import/export functions## p.99
getinfo.shape("scot_BNG.shp") # get info on wether shapefile contains points, lines or polygons

###4.3_Raster file formats### p.100
##4.3.1_Using GDAL drivers in rgdal## p.100
auck_el1 <- readGDAL("70042108.tif") # load Tiff file
summary(auck_el1)

is.na(auck_el1$band1) <- auck_el1$band1 <= 0 | auck_el1$band1 >10000

x <- GDAL.open("70042108.tif")
xx <- getDriver(x)
xx
getDriverLongName(xx)
x
dim(x)
GDAL.close(x)
# here: x is a derivative of a GDALDataset object; data are not in the R workspace, but all their features are there to be read on demand

GDALinfo("70042108.tif") # information about the file to be accessed
brks <- c(0, 10, 20, 50, 100, 150, 200, 300, 400, 500,600, 700)
pal <- terrain.colors(11)
pal
length(pal) == length(brks) - 1
auck_el1$band1 <- findInterval(auck_el1$band1, vec = brks,all.inside = TRUE) - 1
writeGDAL(auck_el1, "demIndex.tif", drivername = "GTiff",type = "Byte", colorTable = list(pal), mvFlag = length(brks) -1)
Gi <- GDALinfo("demIndex.tif", returnColorTable = TRUE)
CT <- attr(Gi, "ColorTable")[[1]]
CT[CT > "#000000"]

install.packages("gstat")
library(gstat)
# output inverse distance weighted interpolated values of Meuse Bank logarithms of zinc ppm as a GeoTiff file:
log_zinc <- idw(log(zinc) ~ 1, meuse, meuse.grid)["var1.pred"]
summary(log_zinc)
writeGDAL(log_zinc, fname = "log_zinc.tif", drivername = "GTiff",type = "Float32", options = "INTERLEAVE=PIXEL")
GDALinfo("log_zinc.tif")
# output file can for example be read into ENVIT directly

# added from online script (without it it is not working):
data(meuse.grid) 
coordinates(meuse.grid) <- c("x", "y") 
gridded(meuse.grid) <- TRUE 
proj4string(meuse.grid) <- CRS("+init=epsg:28992") 
data(meuse) 
coordinates(meuse) <- c("x", "y") 
proj4string(meuse) <- CRS(proj4string(meuse.grid))

Soil <- meuse.grid["soil"]
table(Soil$soil)
Soil$soil <- as.integer(Soil$soil) - 1
Cn <- c("Rd10A", "Rd90C/VII", "Bkd26/VII")
writeGDAL(Soil, "Soil.tif", drivername = "GTiff", type = "Byte",catNames = list(Cn), mvFlag = length(Cn))
Gi <- GDALinfo("Soil.tif", returnCategoryNames = TRUE)
attr(Gi, "CATlist")[[1]]
summary(readGDAL("Soil.tif"))

head(gdalDrivers(), n = 10) # show first 10 drivers on book production platform
writeGDAL(log_zinc, fname = "log_zinc.rda", drivername = "R") # driver for storing portable SpatialGrid-DataFrame objects
GDALinfo("log_zinc.rda")

#from online code:
options(width=55)
GDALinfo("log_zinc.rda")
options(width=70)

unlink("log_zinc.rda*")
#bringt aber irgendwie nichts, klappt trotzdem nicht??????!!!!!!!!!!!!!!!!!!!

# read a raster version of OpenStreetMap19 data for the centre of Bergen, Norway:
service_xml <- "frmt_wms_openstreetmap_tms.xml"
offset <- c(19339000, 34546000)
osm <- readGDAL(service_xml,offset=offset,region.dim=c(2000,2000),output.dim=c(1000, 1000))#????????????????
summary(osm)#????????????????????????????????????????????????????????????????????????????????????????????????

##4.3.2 other import_export functions## p.107

###4.4_Google Earth, Google Maps and other formats### p.108
install.packages("RgoogleMaps")
library(RgoogleMaps) # tools to access Google MapsTdata in image form
myMap <- GetMap(center = c(60.395, 5.322), zoom = 16,destfile = "MyTile2.png", maptype = "mobile")
BB <- do.call("rbind", myMap$BBOX)
dBB <- rev(diff(BB))
DIM12 <- dim(myMap$myTile)[1:2]
cs <- dBB/DIM12
cc <- c(BB[1, 2] + cs[1]/2, BB[1, 1] + cs[2]/2)
GT <- GridTopology(cc, cs, DIM12)
p4s <- CRS("+proj=longlat +datum=WGS84")
SG_myMap <- SpatialGridDataFrame(GT, proj4string = p4s,data = data.frame(r = c(t(myMap$myTile[, , 1])) *255, g = c(t(myMap$myTile[, , 2])) * 255, b = c(t(myMap$myTile[,, 3])) * 255))
#myMap1 <- GetMap.OSM(lonR = c(5.319, 5.328), latR = c(60.392,60.398), scale = 4000, destfile = "MyTile.png")#????????????
#????????????????????????????????????????cant find function GetMap.OSM????????????????????????????????????????????????

install.packages("osmar")
library(osmar) # vector data from open street map available here
api <- osmsource_api()
box <- corner_bbox(5.319, 60.392, 5.328, 60.398)
torget <- get_osm(box, source = api)
torget1 <- as_sp(torget, "lines")
sort(table(torget1$user), decreasing = TRUE)[1:3]

bybane <- find(torget, way(tags(k == "light_rail")))
bybane <- find_down(torget, way(bybane))
bybane <- subset(torget, ids = bybane)
bybane <- as_sp(bybane, "lines")#????????????????????????????????????????????????????????????????????
#writeOGR(Fires[, c("gml_id", "FireDate", "Area_HA")],dsn = "fires.kml", layer = "fires", driver = "KML")

# First we make a polygon to bound the study area and project it to geographical coordinates:
library(maptools)
grd <- as(meuse.grid, "SpatialPolygons")
proj4string(grd) <- CRS(proj4string(meuse))
grd.union <- unionSpatialPolygons(grd, rep("x", length(slot(grd,"polygons"))))
ll <- CRS("+proj=longlat +datum=WGS84")
grd.union.ll <- spTransform(grd.union, ll)

# Next we construct a suitable grid in geographical coordinates, as our target object for export, using the GE_SpatialGrid wrapper function:
llGRD <- GE_SpatialGrid(grd.union.ll)
llGRD_in <- over(llGRD$SG, grd.union.ll) # over() sets grid cells outside the river bank area to NA
# and then discard them by coercion to a SpatialPixelsDataFrame:
llSGDF <- SpatialGridDataFrame(grid = slot(llGRD$SG,"grid"), proj4string = CRS(proj4string(llGRD$SG)),data = data.frame(in0 = llGRD_in))
llSPix <- as(llSGDF, "SpatialPixelsDataFrame")
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
llSPix$pred <- gstat::idw(log(zinc) ~ 1, meuse_ll, llSPix)$var1.pred
png(file = "zinc_IDW.png", width = llGRD$width, height = llGRD$height,bg = "transparent")
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
image(llSPix, "pred", col = bpy.colors(20))
dev.off()
kmlOverlay(llGRD, "zinc_IDW.kml", "zinc_IDW.png")

###4.5_Geographical resources Analysis Support System (GRASS)### p.112
install.packages("spgrass6")
library(spgrass6)
execGRASS("g.region", flags = "p")#???????????????????????????????????????????????????ab hier????????????
spear <- readRAST6(c("elevation.dem", "geology"), cat = c(FALSE,TRUE))
summary(spear)
table(spear$geology)
execGRASS("r.stats", input = "geology", flags = c("quiet","c", "l"))
bugsDF <- readVECT6("bugsites")
vInfo("streams")
streams <- readVECT6("streams", type = "line,boundary",remove.duplicates = FALSE)
summary(bugsDF)#???????????????????????????????????????????????????????????????????????????bis hier???????????

##4.5.1 broad street cholera data## p.118 #????????????????????????????????????????????????????von hier bis Ende???
execGRASS("g.gisenv", set="LOCATION_NAME=snow2")
execGRASS("g.region", rast="snowcost_broad")

sohoSG <- readRAST6(c("snowcost_broad", "snowcost_not_broad"))
buildings <- readVECT6("vsnow4")
proj4string(sohoSG) <- CRS(proj4string(buildings))
deaths <- readVECT6("deaths3")
o <- over(deaths, sohoSG)
library(maptools)
deaths <- spCbind(deaths, o)
deaths$b_nearer <- deaths$snowcost_broad < deaths$snowcost_not_broad
by(deaths$Num_Cases, deaths$b_nearer, sum)
nb_pump <- readVECT6("vpump_not_broad")
b_pump <- readVECT6("vpump_broad")
library(rgeos)
vsnow4buf <- gBuffer(buildings, width = -4)
GRD <- gmeta2grd()
SG <- SpatialGrid(GRD, proj4string = CRS(proj4string(vsnow4buf)))
o <- over(SG, vsnow4buf)
crs <- CRS(proj4string(vsnow4buf)
SGDF <- SpatialGridDataFrame(GRD, proj4string = crs),data = data.frame(o = o))
SGDF$o[is.na(SGDF$o)] <- 2.5
SGDF$o[SGDF$o == 1] <- NA
library(gdistance)
r <- as(SGDF, "RasterLayer")
tr <- transition(r, mean, 8)
d_b_pump <- rSPDistance(tr, deaths, b_pump, theta = 1e-12)
d_nb_pump <- rSPDistance(tr, deaths, nb_pump, theta = 1e-12)
deaths$g_snowcost_broad <- d_b_pump[, 1]
deaths$g_snowcost_not_broad <- apply(d_nb_pump, 1, min)
deaths$g_b_nearer <- deaths$g_snowcost_broad < deaths$g_snowcost_not_broad
by(deaths$Num_Cases, deaths$g_b_nearer, sum)
