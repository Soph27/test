###session 7, 12.12.17###################################
###classifications and related topics#############################

### funny packages:
install.packages("fortunes")
library(fortunes)
fortune() # call a quote
fortune("memory") # call and search for a quote topic

install.packages("cowsay")
library(cowsay)
say("Hello World!")
someone_say_hello <- function(){
  animal <- sample(names(animals),1);say(paste("Hello,I`m a",animal,".",collapse=""),by=animal)}
  someone_say_hello()

someone_say_my_fortune <- function(x){
  animal <- animal <- sample(names(animals),1)
  say(paste(fortune(),collapse="\n"),by=animal)
}
someone_say_my_fortune()

## send mail:
install.packages("sendmailR")
library(sendmailR)

#set working directory
setwd("C:/workingdirectorypath")

#####send plain email
#
#from <- "sophie2727@web.de"
#to <- "nina.gnann@web.de"
#subject <- "HAHAHAHAHA"
#body <- "Email body."                     
#mailControl=list(smtpServer="serverinfo")
#sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)

###############################################################
### back to the beginning - geocoding and maps
install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("ggalt")
library(ggalt)

# get coordinates:
wue <- geocode("Wuerzburg") # get longitude and latitude
# get Google Hybrid Map (alternative roads, satellite or osm (open street map)):
wue_ggl_hybrid_map <- qmap("wue",zoom=12,source="google",maptype="hybrid")
# get coordinates for certain places:
wue_places <- c("Zellerau",
  "Sanderau",
  "Gerbrunn",
  "Estenfeld")
places_loc <- geocode(wue_places) # get longitudes and latitudes
# plot hybrid map with dots of places and a circle around them:
wue_ggl_hybrid_map+geom_point(aes(x=lon,y=lat),
    data=places_loc,
    alpha=0.7,
    size=7,
    solor="tomato")+
  geom_encircle(aes(x=lon,y=lat),
    data=places_loc,size=2,color="blue")

#####################################################################
###supervised classification:
install.packages("rgdal")
library(rgdal)
install.packages("RStoolbox")
library(RStoolbox)

install.packages("e1071")
library(e1071)

install.packages("raster")
library(raster)
allbands <- brick("C:\\Users\\Sophie\\Documents\\test\\crop_p224r63_all_bands.tif")
plotRGB(allbands,3,2,1,stretch="lin")

td <- rgdal::readOGR("C:\\Users\\Sophie\\Documents\\test","superClass_trainingdata")
sc <- superClass(allbands,trainData=td,responseCol="id") # ohne defined model random forest (rf) is used
plot(sc$map)
sc
summary(sc)

getModelInfo()
#library(caret) #superClass() basiert auf caret package

sc2 <- superClass(allbands,trainData=td,responseCol="id",model="svmLinear")
plot(sc2$map)

sc3 <- superClass(allbands,trainData=td,responseCol="id",model="nnet")
plot(sc3$map)

getwd()
setwd("C:\\Users\\Sophie\\Documents\\test")
writeRaster(sc$map,filename="superClassresult",format="GTiff",overwrite=TRUE)
writeRaster(sc2$map,filename="superClassresult_svmLinear",format="GTiff",overwrite=TRUE)
writeRaster(sc3$map,filename="superClassresult_nnet",format="GTiff",overwrite=TRUE)

library(RStoolbox)
# stack raster:
raster_stack <- stack(sc$map,sc2$map,sc3$map)
superClass_comparison <- rasterEntropy(raster_stack) # analyse differences in classification results (RStoolbox)
plot(superClass_comparison)

###############################################################################################################
###redo superClass with actual R code:
library(maptools)
library(randonForest)
library(raster)
setwd()

# enter path and name (without suffix) to the import vector command:
vec <- readOGR('vector_data','training')
satImage <- brick("raster_data/input_data.tif")
numsamps <- 100 # number of samples per land cover class
attName <- 'ID' # name of attribute holding the integer land cover type identifier (very important to consider when doing the training data sets)
outImage <- 'classif_result.tif' # name and path of output GeoTiff image

# loop over each class, selecting all polygons and assign random points:
uniqueAtt <- unique(vec[[attName]])
for (x in 1:length(uniqueAtt)){ # query number of classes; uniqueAtt=number of classes; x is class
  class_data <- vec[vec[[attName]]==uniqueAtt[x],] # extract polygons with class n; subset vector data
  classpts <- spsample(class_data,type="random",n=numsamps) # set 100 random points (as defined above) inside each polygon landcover class (in selected polygons)
  #first run: create new spatial points data frame; for all subsequent ones, data will be added:
  if(x==1){ # without if we would only have data for second class;
    xy <- classpts
  }else{
    xy <- rbind(xy,classpts) # bind previous used data frame with a new one (new class) and dont overwrite it
  }
}

# plot randomly generated points on one of the rasters - for visual checking only:
pdf("training_points.pdf") # image will be saved in filesystem as pdf (generates pdf with raster and sampling points)
  image(satImage,1)
  points(xy)
#dev.off()

# extract reference and pixel values for training data:
temp <- over(x=xy,y=vec) # extract values of vector behind random points
response <- factor(temp[[attName]]) # create vector of attribute names
trainvals <- cbind(response,extract(satImage,xy)) # combines point with raster values (values of all bands are extracted, not coordinates)
# extracting and combination of classes and band values

# the actual classification statistics using randomForest method:
print("Starting to calculate random forest object")
randfor <- randomForest(as.factor(response)~., # ~ means take everything in your data frame
  data=trainvals,
  na.action=na.omit,
  confusion=T)

# apply fitted model to full raster (predict):
print("Starting predictions")
predict(satImage,randfor,filename=outImage,progress='text',format='GTiff',datatype='INT1U',type='response',overwrite=TRUE)
# using all band values of all pixels to generate a classification

####################################################################################################################
# validation: how good is classification?
library(rgdal)
validationPolygons <- rgdal::readOGR("C:\\Users\\Sophie\\Documents\\test\\vector_data","trainingdata")
sc <- superClass(allbands,trainData=td,responseCol="class_name",trainPartition=0.7) # hold out proportion
sc <- superClass(allbands,trainData=td,valData=validationPolygons,responseCol="class_name") # pre-defined hold-outs
sc$validation$performance

library(RStoolbox)
val <- validateMap(sc$map,valData=validationPolygons,responseCol="class_name",mode="classification",classMapping = sc$classMapping) # validate an existing map with reference data
val
