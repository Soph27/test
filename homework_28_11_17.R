###homework 21_11_17################################################################
###Applied Spatial Data Analysis with R, Chapter 3: Visualizing Spatial Data in R###

library(sp)
library(lattice)
library(ggplot2)

###3.1_The traditional Plot System###
##3.1.1_Plotting points, lines, polygons and grids##

data(meuse)
coordinates(meuse) <- c("x","y")
plot(meuse)
title("points")

cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)),"line1")))
plot(m.sl)
title("lines")

data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
plot(meuse.pol,col="grey")
title("polygons")

data(meuse.grid)
coordinates(meuse.grid) <- c("x","y")
meuse.grid <- as(meuse.grid,"SpatialPixels")
image(meuse.grid,col="grey")
title("grid")

image(meuse.grid,col="lightgrey")
plot(meuse.pol,col="grey",add=TRUE)
plot(meuse,add=TRUE)

##3.1.2_Axes and Layout Elements##

layout(matrix(c(1,2),1,2))
plot(meuse.pol,axes=TRUE)
plot(meuse.pol,axes=FALSE)
axis(1,at=c(178000+0:2*2000),cex.axis=0.7)
axis(2,at=c(326000+0:3*4000),cex.axis=0.7)
box()

oldpar=par(no.readonly=TRUE)
layout(matrix(c(1,2),1,2))

plot(meuse,axes=TRUE,cex=0.6)
plot(meuse.pol,add=TRUE)
title("sample locations")
par(mar=c(0,0,0,0)+0.1)

plot(meuse,axes=FALSE,cex=0.6)
plot(meuse.pol,add=TRUE)
box()
par(oldpar)

plot(meuse)
plot(meuse.pol,add=TRUE)
plot(meuse)
SpatialPolygonsRescale(layout.scale.bar(),offset=locator(1),scale=1000,fill=c("transparent","black"),plot.grid=FALSE)
text(locator(1),"0")
text(locator(1),"1 km")
SpatialPolygonsRescale(layout.north.arrow(),offset=locator(1),scale=400,plot.grid=FALSE)

##3.1.3_Degrees in Axes Labels and Reference Grid##
library(maptools)
library(maps)
wrld <- map("world",interior=FALSE,xlim=c(-179,179),ylim=c(-89,89),plot=FALSE)
wrld_p <- pruneMap(wrld,xlim=c(-179,179))
llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p,proj4string=llCRS)
prj_new <- CRS("+proj=moll")
library(rgdal)
wrld_proj <- spTransform(wrld_sp,prj_new)
wrld_grd <- gridlines(wrld_sp,easts=c(-179,seq(-150,150,50),179.5),norths=seq(-75,75,15),ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd,prj_new)
at_sp <- gridat(wrld_sp,easts=0,norths=seq(-75,75,15,offset=0.3))
at_proj <- spTransform(at_sp,prj_new)
plot(wrld_proj,col="grey60")
plot(wrld_grd_proj,add=TRUE,lty=3,col="grey70")
text(coordinates(at_proj),pos=at_proj$pos,offset=at_proj$offset,labels=parse(text=as.character(at_proj$labels)),cex=0.6)

##3.1.4_Plot size, plotting area, map scale, and multiple plots##

par("pin")
par(pin=c(4,4))
dev.off()
X11(width=10,height=10)
pdf("file.pdf",width=5,height=7)

pin <- par("pin")
dxy <- apply(bbox(meuse),1,diff)
ratio <- dxy[1]/dxy[2]
par(pin=c(ratio*pin[2],pin[2]),xaxs="i",yaxs="i")
plot(meuse,pch=1)
box()

par(mfrow=c(2,3))
#or
layout(matrix(1:6,2,3,byrow=TRUE))

##3.1.5_Plotting attributes and map legends##

grays=grey.colors(4,0.55,0.95)
image(zn.idw,col=grays,breaks=log(c(100,200,400,800,1400)))
plot(meuse.pol,add=TRUE)
plot(meuse,pch=1,cex=sqrt(meuse$zinc)/20,add=TRUE)
legVals <- c(100,200,500,1000,2000)
legend("left",legend=legVals,pch=1,pt.cex=sqrt(legVals)/20,bty="n",title="measured")
legend("topleft",legend=c("100-200","200-400","400-800","800-1800"),fill=grays,bty="n",title="interpolated")

###3.2_Trellis/Lattice Plots with spplot###
##3.2.1_a straight trellis example##
library(lattice)
levelplot(z~x+y|name,spmap.to.lev(zn[c("diredt","log")]),asp="iso")
ssplot(zn[c("direct","log")])

##3.2.2_Plotting points, lines, polygons and grids##
library(maptools)
data(meuse.grid)
coordinates(meuse.grid) <- c("x","y")
meuse.grid <- as(meuse.grid,"SpatialPixelsDataFrame")
im <- as.image.SpatialGridDataFrame(meuse.grid["dist"])
cl <- ContourLines2SLDF(contourLines(im))
ssplot(cl)

river <- list("sp.polygons",meuse.pol)
north <- list("SpatialPolygonsRescale",layout.north.arrow(),offset=c(178750,332500),scale=400)
scale <- list("SpatialPolygonsRescale",layout.scale.bar(),offset=c(180200,329800),scale=1000,fill=c("transparent","black"))
txt1 <- list("sp.text",c(180200,329950),"0")
txt2 <- list("sp.text",c(181200,329950),"1 km")
pts <- list("sp.points",meuse,pch=3,col="black")
meuse.layout <- list(river,north,scale,txt1,txt2,pts)
ssplot(zn["log"],sp.layout=meuse.Layout)

##3.2.4_Arranging panel layout## p.74

###3.3_Alternative routes:ggplot, latticeExtra###
library(ggplot2)
methods(fortify)

m=as(meuse,"data.frame")
ggplot(m,aes(x,y))+geom_point()+coord_equal()

library(latticeExtra)
p=ssplot(meuse["zinc"])
m=SpatialPolygonsDataFrame(meuse.pol,data.frame(col=1),match.ID=FALSE)
l=ssplot(m)
l+p
p+l

###3.4_Interactive plots### p.76
##3.4.1_Interacting with base graphics##
plot(meuse)
meuse.id <- identify(coordinates(meuse))

plot(meuse)
region <- locator(type="o")
n <- length(region$x)
p <- Polygon(cbind(region$x,region$y)[c(1:n,1),],hole=FALSE)
ps <- Polygons(list(p),ID="region")
sps <- SpatialPolygons(list(ps))
plot(meuse[sps,],pch=16,cex=0.5,add=TRUE)

library(maptools)
prj <- CRS("+proj=longlat +datum=NAD27")
nc_shp <- system.file("shapes/sids.shp",package="maptools")[1]
nc <- readShapePoly(nc_shp,proj4string=prj)
plot(nc)
pt <- locator(type="p")
print(pt)
pt.sp <- SpatialPoints(cbind(pz$x,pt$y),proj4string=prj)
over(pt.sp,nc)

##3.4.2_Interacting with ssplot and Lattice plots##
ids <- ssplot(meuse,"zinc",identify=TRUE)

library(lattice)
trellis.focus("panel",column=1,row=1)
ids <- panel.identify()
trellis.unfocus()

library(grid)
trellis.focus("panel",column=1,row=1)
as.numeric(grid.locator())
trellis.unfocus()

###3.5_Colour palettes and class intervals### p.79
##3.5.1_colour palettes##
rw.colors <- colorRampPalette(c("red","white"))
image(meuse.grid["dist"],col=rw.colors(10))

library(RColorBrewer)
example(brewer.pal)

##3.5.2_class intervals##
library(RColorBrewer)
library(classInt)
pal <- brewer.pal(5,"Reds")
q5 <- classIntervals(meuse$zinc,n=5,style="quantile")
q5
diff(q5$brks)
plot(q5,pal=pal)
fj5 <- classIntervals(meuse$zinc,n=5,style="fisher")
fj5
diff(fj5$brks)
plot(fj5,pal=pal)

q5Colours <- findColours(q5,pal)
plot(meuse,col=q5Colours,pch=19)
legend("topleft",fill=attr(q5Colours,"palette"),legend=names(attr(q5Colours,"table")),bty="n")

cuts=(0:10)/10
ssplot(meuse.grid,"dist",colorkey=list(labels=list(at=cuts)),at=cuts)


