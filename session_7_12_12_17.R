###session 7, 12.12.17###################################
###supervised classification#############################

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


# send mail:
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

wue <- geocode("Wuerzburg")
wue_ggl_hybrid_map <- qmap("wue",zoom=12,source="google",maptype="hybrid")
wue_places <- c("Zellerau",
  "Sanderau",
  "Gerbrunn",
  "Estenfeld")
places_loc <- geocode(wue_places)
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

#########################################################
install.packages("raster")
library(raster)
allbands <- brick("E:\\EAGLE\\crop_p224r63_all_bands.tif")
td <- rgdal::readOGR("E:\\EAGLE","superClass_trainingdata")
sc <- superClass(allbands,trainData=td,responseCol="id") # ohne defined model random forest (rf) is used
plot(sc$map)
#sc2 <- superClass(allbands,trainData=td,responseCol="id",model="....")
getModelInfo()
#library(caret) #superClass() basiert auf caret package
# responseCol muss id sein????????????????????????????????????????????????????????????????????

sc2 <- superClass(allbands,trainData=td,responseCol="id",model="svmLinear")
plot(sc2$map)

sc
summary(sc)
getwd()
setwd("E:\\EAGLE\\MB2-programming\\test")
writeRaster(sc$map,filename="superClassresult",format="GTiff",overwrite=TRUE)
writeRaster(sc2$map,filename="superClassresult_svmLinear",format="GTiff",overwrite=TRUE)

#plotRGB(allbands,3,2,1,stretch="lin")

library(RStoolbox)
# stack raster:
raster_stack <- stack(sc$map,sc2$map)
Class_comparison <- rasterEntropy(raster_stack) # analyse differences in classification results (RStoolbox)
plot(Class_comparison)

###############################################################################################################
###redo superClass with actual R code###
library(maptools)
library(randonForest)
library(raster)
setwd()

# enter path and name (without suffix) to the import vector command
vec <- readOGR('vector_data','training')
satImage <- brick("raster_data/input_data.tif")
numsamps <- 100 # number of samples per land cover class
attName <- 'ID'
outImage <- 'classif_result.tif' # name and path of output GeoTiff image

uniqueAtt <- unique(vec[[attName]])
for (x in 1:length(uniqueAtt)){ # uniqueAtt=number of classes; x is class
  class_data <- vec[vec[[attName]]==uniqueAtt[x],] # subset vector data
  classpts <- spsample(class_data,type="random",n=numsamps)
  if(x==1){ # without if we would only have data for second class;
    xy <- classpts
  }else{
    xy <- rbind(xy,classpts) # bind previous used data frame with a new one (new class) and dont overwrite it
  }
}

pdf("training_points.pdf")
  image(satImage,1)
  points(xy)
#dev.off()

temp <- over(x=xy,y=vec) # extract values of vector behind random points
response <- factor(temp[[attName]]) # create vector of attribute names
trainvals <- cbind(response,extract(satImage,xy)) # combines point with raster values (values of all bands are extracted, not coordinates)

print()
randfor <- randomForest(as.factor(response)~.,
                        data=trainvals,
                        na.action=na.omit,
                        confusion=T)
# ~means take everything in your data frame

print("Starting predictions")
predict(satImage,randfor,filename=outImage,progress='text',format='GTiff',datatype='INT1U',type='response',overwrite=TRUE)

####################################################################################################################
# validation: how good is classification?
validationPolygons <- rgdal::readOGR("E:\\EAGLE\\MB2-programming\\test\\vector_data","trainingdata")
sc <- superClass(allbands,trainData=td,responseCol="id",trainPartition=0.7) # hold out proportion
#sc <- superClass(allbands,trainData=td,valData=validationPolygons,responseCol="id") # pre-defined hold-outs
#sc$validation$performance
# katrins attributtabelle muss noch umbenannt werden von land cover zu class oder wie es bei mir heisst!!

#validateMap()