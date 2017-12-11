###homework 21_11_17################################################################
###Applied Spatial Data Analysis with R, Chapter 3: Visualizing Spatial Data in R###
install.packages("sp")
install.packages("ggplot2")
library(sp)
library(lattice)
library(ggplot2)

###3.1_The traditional Plot System###
##3.1.1_Plotting points, lines, polygons and grids##

data(meuse) # load data set "meuse"
summary(meuse)
# plotting points:
coordinates(meuse) <- c("x","y")
plot(meuse)
title("points")

# plotting lines:
cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)),"line1"))) # create SpatialLines object from list of Lines objects and data.frames
plot(m.sl)
title("lines")
# Spatial Lines object is built by joining up the points in sequence; plot daws resulting zigzags

# plotting polygons:
data(meuse.riv)
meuse.riv
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
plot(meuse.pol,col="grey")
title("polygons")

# convert grid data for Meue bank study area into a SpatialPixels object:
data(meuse.grid)
meuse.grid
coordinates(meuse.grid) <- c("x","y")
meuse.grid <- as(meuse.grid,"SpatialPixels")
image(meuse.grid,col="grey") # display created object by image() with all cells set to "grey"
title("grid")

image(meuse.grid,col="lightgrey")
plot(meuse.pol,col="grey",add=TRUE) # add meuse.grid and meuse.pol together
plot(meuse,add=TRUE) # add points, polygons and grid together

image(meuse.grid,col="yellow")
plot(meuse.pol,col="blue",add=TRUE)
plot(meuse,col="red",add=TRUE)

##3.1.2_Axes and Layout Elements##

layout(matrix(c(1,2),1,2))
plot(meuse.pol,axes=TRUE) # axes with default values
plot(meuse.pol,axes=FALSE) # axs without default values
axis(1,at=c(178000+0:2*2000),cex.axis=0.7) # setting of x-axis; cex.axis adjusts size
axis(2,at=c(326000+0:3*4000),cex.axis=0.7) # setting of y-axis
box() # box around map

oldpar=par(no.readonly=TRUE) # par() for not reserving space for adding axes and titles later on; TRUE means there are no other arguments
layout(matrix(c(1,2),1,2))

plot(meuse,axes=TRUE,cex=0.6) # R`s default margins leaving space for axes`
plot(meuse.pol,add=TRUE)
title("sample locations")
par(mar=c(0,0,0,0)+0.1) # mar modifies margins
# par() to set / query grahical parameters
plot(meuse,axes=FALSE,cex=0.6) # maximised plotting space and no axes drawn
plot(meuse.pol,add=TRUE)
box()
par(oldpar)

# if no axes are drawn on a map, you can plot a scale bar and a north arrow:
plot(meuse)
plot(meuse.pol,add=TRUE)
plot(meuse)
SpatialPolygonsRescale(layout.scale.bar(),offset=locator(1),scale=1000,fill=c("transparent","black"),plot.grid=FALSE)
text(locator(1),"0")
text(locator(1),"1 km")
SpatialPolygonsRescale(layout.north.arrow(),offset=locator(1),scale=400,plot.grid=FALSE)

##3.1.3_Degrees in Axes Labels and Reference Grid##
# add a grid to a map for reference reasons:
# code to define and draw projected latitude/longitude grid lines and grid line labels:
install.packages("rgeos")
install.packages("maptools")
install.packages("maps")
library(rgeos)
library(maptools)
library(maps)
wrld <- map("world",interior=FALSE,xlim=c(-179,179),ylim=c(-89,89),plot=FALSE)
wrld_p <- pruneMap(wrld,xlim=c(-179,179))
llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p,proj4string=llCRS)
prj_new <- CRS("+proj=moll")
install.packages("rgdal")
library(rgdal)
wrld_proj <- spTransform(wrld_sp,prj_new)

# gridLines to generate object of class SpatialLines:
wrld_grd <- gridlines(wrld_sp,easts=c(-179,seq(-150,150,50),179.5),norths=seq(-75,75,15),ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd,prj_new)
at_sp <- gridat(wrld_sp,easts=0,norths=seq(-75,75,15,offset=0.3)) # gridat() returns an object to draw the labels for these 'gridded curves'
at_proj <- spTransform(at_sp,prj_new)
plot(wrld_proj,col="grey60") # plot world map
plot(wrld_grd_proj,add=TRUE,lty=3,col="grey70") # add coorinate net to world map
text(coordinates(at_proj),pos=at_proj$pos,offset=at_proj$offset,labels=parse(text=as.character(at_proj$labels)),cex=0.6) # add labels

##3.1.4_Plot size, plotting area, map scale, and multiple plots##
# R distinguishes between figure region and plotting region
# get and set figure size in inches:
par("pin")
par(pin=c(4,4))

# for enlarging the plotting window: close current plotting device and re-open it specifying the size:
dev.off() # shut down the specified device
windows(width=10,height=10) # x11 opens new window for graphic, or use: x11(widh=10,height=10)
pdf("file.pdf",width=5,height=7) # write graphic output to a file

# control the plotted data area by passing xlim and ylim in a plot command and setting par():
# without par(): margins still will be extended by 4% on each side by default
# set aspect of plotting region equal to that of data points:
pin <- par("pin")
dxy <- apply(bbox(meuse),1,diff)
ratio <- dxy[1]/dxy[2]
par(pin=c(ratio*pin[2],pin[2]),xaxs="i",yaxs="i")
plot(meuse,pch=1) # plot points without allowing for 4% extention of the range in all directions
box()

# create more than one map in a single figure by sub-deviding figure region into number of sub-regions:
par(mfrow=c(2,3)) # split figure into 2 rows and 3 columns
#or
layout(matrix(1:6,2,3,byrow=TRUE)) # split figure into 2 rows and 3 columns

##3.1.5_Plotting attributes and map legends## p.68

data(meuse.grid) 
coordinates(meuse.grid) <- c("x", "y") 
gridded(meuse.grid) <- TRUE 
install.packages("gstat")
library(gstat) 
zn.idw <- krige(log(zinc) ~ 1, meuse, meuse.grid)

grays=grey.colors(4,0.55,0.95) # set colours
image(zn.idw,col=grays,breaks=log(c(100,200,400,800,1400))) 
plot(meuse.pol,add=TRUE)
plot(meuse,pch=1,cex=sqrt(meuse$zinc)/20,add=TRUE)
legVals <- c(100,200,500,1000,2000) # define steps for 'measured'
legend("left",legend=legVals,pch=1,pt.cex=sqrt(legVals)/20,bty="n",title="measured") # pch is symbol; bty is type of box drawn around legend
legend("topleft",legend=c("100-200","200-400","400-800","800-1800"),fill=grays,bty="n",title="interpolated")
# legend command places two legends (one for symbols, one for colour)
title("measured and interpolated zinc")

###3.2_Trellis/Lattice Plots with spplot###
##3.2.1_a straight trellis example## p.70
library(lattice)
library(gstat) 
library(sp) 
data(meuse) 
coordinates(meuse) <- ~x+y 
data(meuse.grid) 
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- T
zn <- krige(zinc~1,meuse,meuse.grid)
zn$direct <- zn$var1.pred 
zn$log <- exp(krige(log(zinc)~1,meuse,meuse.grid)$var1.pred)
.iwidth <- 8 
.iheight <- 6 
.pwd <- .8 
.ipointsize <- 9 
.epsNo <- .epsNo + 1
file <- paste("../Art/Fig-vis-", .epsNo, ".eps", sep="") 
postscript(file=file, onefile = TRUE, paper="special", width = .iwidth, height = .iheight, pointsize = .ipointsize, horizontal=FALSE) 
lattice.options(default.theme = col.whitebg()) 
library(lattice) # RSB quietening greys 

meuse@data[["zinc"]]
#??????????????????????????????????????????????????????????????????????????????????????????error:undefined columns selected???????????
# plotting of 2 interpolation scenarios for zinc variable in meuse data set obtained on direct & log scale:
levelplot(z~x+y|name,spmap.to.lev(zn[c("diredt","log")]),asp="iso",cuts=4,col.regions=grey.colors(5, 0.90, 0.50, 2.2),brewer.pal(5, "Reds"),split = c(1,1,1,2),more = TRUE)
# or:
spplot(zn[c("direct","log")])
#?????????????????????????????????????????????????????????????????????????????????????????????????????

##3.2.2_Plotting points, lines, polygons and grids## p.70
library(sp)
install.packages("spplot2")
library(spplot2)
library(maptools)
library(lattice)
data(meuse.grid)
coordinates(meuse.grid) <- c("x","y")
meuse.grid <- as(meuse.grid,"SpatialPixelsDataFrame") # first argument: Spatial*data frame object (with points, lines, polygons or a grid)
im <- as.image.SpatialGridDataFrame(meuse.grid["dist"]) # 2nd argument: tells which attributes (column names or numbers) should be used
cl <- ContourLines2SLDF(contourLines(im)) # calculate contour lines for distance to river Meuse
# further attributes control the plotting (here: colour) 
spplot(cl)

##3.2.3_Adding reference and layout elements to plots## p.73

# build an sp.layout structure:
# list of layout items: 1st layout function, 2nd object to be plotted, then optional arguments (colour,symbol,size ect.)
river <- list("sp.polygons",meuse.pol)
north <- list("SpatialPolygonsRescale",layout.north.arrow(),offset=c(178750,332500),scale=400)
scale <- list("SpatialPolygonsRescale",layout.scale.bar(),offset=c(180200,329800),scale=1000,fill=c("transparent","black"))
txt1 <- list("sp.text",c(180200,329950),"0")
txt2 <- list("sp.text",c(181200,329950),"1 km")
pts <- list("sp.points",meuse,pch=3,col="black")
meuse.layout <- list(river,north,scale,txt1,txt2,pts)
spplot(zn["log"],sp.layout=meuse.layout)
# this is good method for larger number of graphs

##3.2.4_Arranging panel layout## p.74

###3.3_Alternative routes:ggplot, latticeExtra### p.75
library(ggplot2)
# ggplot takes an object and tries to convert it to a data.rame with method fortify():
methods(fortify)

m=as(meuse,"data.frame")
ggplot(m,aes(x,y))+geom_point()+coord_equal() # aes (which variables are plotted on x and y axis); coord_equal (units along x  axis equal to y axis units ->data are projected)

install.packages("latticeExtra")
library(latticeExtra)
p=spplot(meuse["zinc"])
m=SpatialPolygonsDataFrame(meuse.pol,data.frame(col=1),match.ID=FALSE)
l=spplot(m)
l+p # colour key and xtent are taken from river polygon
p+l # colour key and extent are taken from zinc point measurements

###3.4_Interactive plots### p.76
##3.4.1_Interacting with base graphics## p.77
plot(meuse)
meuse.id <- identify(coordinates(meuse)) # identify plots & returns labels of item nearest to location clicked; right klick ends input

# example digitise session followed by selection and re-plotting of points within digitized area:
plot(meuse)
region <- locator(type="o") # click on at least 4 locations (4 coordinates)
n <- length(region$x)
p <- Polygon(cbind(region$x,region$y)[c(1:n,1),],hole=FALSE) # manually close polygon by adding first point to set of points digitized
ps <- Polygons(list(p),ID="region")
sps <- SpatialPolygons(list(ps))
plot(meuse[sps,],pch=16,cex=0.5,add=TRUE)

# identify particular polygons: use locator and overlay points with polygon layer:
library(maptools)
prj <- CRS("+proj=longlat +datum=NAD27")
nc_shp <- system.file("shapes/sids.shp",package="maptools")[1]
nc <- readShapePoly(nc_shp,proj4string=prj)

plot(nc)
pt <- locator(type="p") # click on location to get information (more than 1 click possible)

print(pt)
pt.sp <- SpatialPoints(cbind(pt$x,pt$y),proj4string=prj)
over(pt.sp,nc)

##3.4.2_Interacting with spplot and Lattice plots## p.78
ids <- spplot(meuse,"zinc",identify=TRUE) # select points with spplot; this returns the selected (clicked on) points' row numbers
# this is doing the same:
library(lattice)
trellis.focus("panel",column=1,row=1)
ids <- panel.identify()
trellis.unfocus()

library(grid)
trellis.focus("panel",column=1,row=1) # select a single point; move to a particular panel or strip
as.numeric(grid.locator()) # grid.locator digitises
trellis.unfocus() # unsets focus & makes top level viewport the current viewport

###3.5_Colour palettes and class intervals### p.79
##3.5.1_colour palettes## p.79
rw.colors <- colorRampPalette(c("red","white"))
image(meuse.grid["dist"],col=rw.colors(10))

library(RColorBrewer)
example(brewer.pal)

##3.5.2_class intervals## p.79
library(RColorBrewer)
install.packages("classInt")
library(classInt)
pal <- brewer.pal(5,"Reds")
q5 <- classIntervals(meuse$zinc,n=5,style="quantile")
q5
diff(q5$brks) # examine width of classes using diff() on the lass breaks
plot(q5,pal=pal)

fj5 <- classIntervals(meuse$zinc,n=5,style="fisher")
fj5
diff(fj5$brks)
plot(fj5,pal=pal)
# once you`re satisfied with chosen class intervals and pallette: go on and plot the data:
q5Colours <- findColours(q5,pal) # build a vector of colours and attributes with findColours()
plot(meuse,col=q5Colours,pch=19)
legend("topleft",fill=attr(q5Colours,"palette"),legend=names(attr(q5Colours,"table")),bty="n")

cuts=(0:10)/10
spplot(meuse.grid,"dist",colorkey=list(labels=list(at=cuts)),at=cuts)

