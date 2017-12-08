###05.12.17#############################################################
###data reshaping, visualization & classification#######################
########################################################################

# how to pick a new graph from ggplot2 from website pdf5 p.73:
library(ggplot2)

# make data
data=data.frame(group=c("A ","B ","C ","D ") , value=c(33,62,56,67) )

# Usual bar plot :
ggplot(data, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat="identity")

# Circular one
ggplot(data, aes(x = group, y = value ,fill = group)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("") +
  
  #Increase ylim to avoid having a complete circle
  ylim(c(0,75)) + 
  
  #Add group labels close to the bars :
  geom_text(data = data, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) +
  
  #Remove useless legend, y axis ticks and y axis text
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
##################################################################################################

##reshaping of data##

# create the data:
fielddata_wide <- read.table(header=TRUE,text='
plot_id name Cover LAI DBH
1 Sophie 7.9 12.3 10.7
2 Achmed 6.3 10.6 11.1
3 Achmed 9.5 13.1 13.8
4 Sophie 11.5 13.4 12.9
')
fielddata_wide # look at the data
fielddata_wide$plot_id <- factor(fielddata_wide$plot_id) # make sure the plot id is a factor

library(reshape2) # activate needed package
# change the format to a long version
# plot_id and names should be preserved
melt(fielddata_wide,id.vars=c("plot_id","name"))
melt(fielddata_wide,id.vars=c("plot_id")) # check what happens if you exclude "name"

# ID variables - all the variables to keep but not split apart on
# measure valiables - the source columns
# name of destination column that will identify the original column that the measurement came from
fielddata_long <- melt(fielddata_wide,id.vars=c("plot_id","name"),measure.vars=c("Cover","LAI","DBH"),variable.name="method",value.name="measurement")
fielddata_long # look at data

# make sure the plot_id column is a factor:
fielddata_long$plot_id <- factor(fielddata_long$plot_id)

# dcast() to convert method types & corresponding values into columns
# keep plot_id name but samples should be in columns and populated with the measurement values:
data_wide <- dcast(fielddata_long,plot_id+name~method,value.var="measurement")
data_wide # sample types are now in columns & names are more condensed as well as plot_ids

###################################################################################################
##ggplot2 and spatial data##

##geofaceting
install.packages("ggmap")
library(ggmap)
library(mapproj)
map.wue <- get_map("Wuerzburg") # get data for a defined location
ggmap(map.wue) # plot map of this location
ggmap(map.wue,zoom=15) # zoom in
map <- get_map("Bavaria") # get an overview map
ggmap(map,zoom=6) # zoom in
ggmap(map)

lsat.df <- data.frame(coordinates(lsat),getValues(lsat)) # extract underlying data frame values
lsat.df <- lsat.df[lsat.df$B1_dn!=0,] # optional: remove background if needed, not with lsat data set
# plot the data and specify which band to be used:
ggplot(lsat.df)+geom_raster(aes(x=x,y=y,fill=B3_dn))+scale_fill_gradient(na.value=NA)+coord_equal()
# same as above but other color gradient:
ggplot(lsat.df)+geom_raster(aes(x=x,y=y,fill=B3_dn))+scale_fill_gradient(low="black",high="white",na.value=NA)+coord_equal()

# same plot as before but store it in "a":
a <- ggplot(lsat.df)+geom_raster(aes(x=x,y=y,fill=B3_dn))+scale_fill_gradient(low="black",high="white",na.value=NA)+coord_equal()
a # just call "a", hence plot it
# get a spatial vector from the RStoolbox package
poly <- readRDS(system.file("external/trainingPolygons.rds",package="RStoolbox"))
plots <- as.data.frame(coordinates(poly)) # extract the coordinates
plots # look up x,y
# plot the previously stored plot plus the vector data:
a+guides(fill=guide_colorbar())+geom_point(data=plots,aes(x=V1,y=V2),shape=3,colour="yellow",size=7)+theme(axis.title.x=element_blank())

# limit extent if you like to plot only certain part or data in case other data sets cover a larger area:
lim <- extent(lsat)
# use stored plot plus new plotting commands:
a+(fill=guide_colorbar())+geom_point(data=plpots,aes(x=V1,y=V2),shape=3,colour="yellow")+theme(axis.title.x=element_blank())+scale_x_continuous(limits=c(lim@xmin,lim@xmax))+ylim(c(lim@ymin,lim@ymax))

# define your own them to have fitting graphs and maps:
#themeSet <- function(type="plot"){
#my_theme <- theme_set(theme_grey())
#if(type=="plot){
 #my_theme <- theme_update(
  #axis.title.x=element_text(size=9,hjust=0.5,vjust=0.2)
  #axis.title.y=element_text(size=9,angle=90,hjust=0.5,vjust=0.1)
  #strip.text.y=element_text(size=8,angle=270)
  #plot.title=element_text(size=9,hjust=0)
  #legend.title=element_text(size=9)
  #legend.text=element_text(size=8)
  #axis.ticks=element_line(colour="grey50")
  #axis.ticks.length=unit(0.06,"cm")
 #)}
#if(type=="map"){
 #my_theme <- theme_update(
  #axis.title.y=element_blank(),
  #axis.title.x=element_blank(),
  #axis.text.x=element_text(size=6,colour="grey50"),
  #axis.text.y=element_text(angle=90,size=6,colour="grey50"),
  #axis.ticks=element_line(colour="grey50"),
  #axis.ticks.length=unit(0.06,"cm"),
  #plot.title=element_text(size=9,hjust=0),
  #legend.title=element_text(size=9),
  #legend.text=element_text(size=8)
 #)
#}}
#themeSet("map")

############################################################################################
#task:use raster & vector data in "steigerwald" package
 #plot time scan and survey points
 #convert data frames in bio__data into a spatial object & plot it
#steigerwald data
steigerwald <- load("bio_data.rda")
steigerwald
bio_data
names(bio_data)
names(bio_data)[1] <- "forest"
bio_data$forest
#ggplot(bio_data$forest,aes(lon_x,lat_y))
#############################################################################################

##supervised classification##
#readBRG()
#brick()
#randomForest(),predict()-->superClass()

# open QGIS and generate some vector data