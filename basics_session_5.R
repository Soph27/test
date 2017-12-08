###session_5_28.11.17####################################################
###classification & RStoolbox############################################

# a list from two vectors of different size:
a <- runif(199) # uniform & random variates (Zufallsvariable)
b <- c("aa","bb","cc","dd","ee")
c <- list(a,b)
c

# indexing a list of two vectors:
c[2] # index second object
c[[2]] # same as
c[[2]][1] # first entry of second object
c[[1]][2] # second entry of first object

# combine different object types into one object:
# a list from two vectors of different size:
a <- list(obj_1=runif(100),obj_2=c("aa","bb"),obj_3=c(1,2,4))
a$obj_1 # call object name
a[["obj_1"]] # or
a[[2]] # or
a[2]

# a list with a matrix, vector and data frame of different sizes:
a <- list(m1=matrix(runif(50),nrow=5),v1=c(1,6,10),df1=data.frame(a=runif(100),b=rnorm(100)))
a$df1[,1] # index a data frame or matrix as known
a

## some more handy functionality ##

a <- 5
if(a>0){print("it is a positive number")}
# RS application: check spatial resolution, resample

if(a!=5){print("number is not equal 5")}else{print("number is equal 5")}
# RS application: check if resolution or projection matches

j <- 0
while(j<1)
  {
y <- j+0.1;print(j)
}
# Ergebnis falsch, endlos schleife!!!!!!
# RS application: resample data until certain resolution is met (until you reach certain threshold in steps of _ )

##################################################################################################################################

## classification = cluster algorithm ##

# if you add training data (vector): supervised model
# if you just insert your raster data into the model: unsupervised model
# unsupervised classification:

allbands <- brick("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\crop_p224r63_all_bands.tif")
allbands

library(RStoolbox)
uc <- unsuperClass(allbands,nClasses=5)
uc # is a list containing all information and the map
str(uc)
plot(uc$map) # point to map inside unsupervied output

# what unsuperClass is doing step by step:
# run kmeans clustering on data of raster object (indicated by [] = brackets for accessing data) with 5 classes:
landsat_allbands.kmeans <- kmeans(allbands[],5)
# copy raster attributes to new raster file:
kmeansraster <- raster(allbands) # create an empty raster based on allbands with exactly same stacks
# populate empty raster file with cluster values from kmeans():
kmeansraster[] <-landsat_allbands.kmeans$cluster #without [] k would be overwritten
plot(kmeansraster)
# [] means don`t use spatial object but just data of spatial object

# clustering with 3 (initial) classes and merging the result with the existing raster file
## dealing with missing values
# create a copy, filled with NAs:
kmeansraster <- allbands[[1]]
kmeansraster[] <- NA
values <- getValues(allbands) # extract values from raster layers
valid <- complete.cases(values) # just use complete cases
valid
allbands.kmeans <- kmeans(values[valid],5,iter.max=100,nstart=3) # run the kmeans clustering
kmeansraster[valid] <- allbands.kmeans$cluster # populate empty vector with cluster values derived from kmeans()

# try with different classification statistical algorithms
install.packages("pamr")
library(pamr)
#landsat_allbands.PAM <- pam(allbands[],8)


## changing the color settings in R and plotting the result

plot(kmeansraster)

# just in case you cannot identify corresponding class for the colors
# click on the map and corresponding raster values are plotted on screen
click(kmeansraster,n=3)#click on 3 different classes, these are then listed in arg, if first click is on water-->water must be on first place in arg usw.

# create a list of labels corresponding to the class values
# first create a sequence from 1 to 5 in steps of 1 and label them based on your landscape understanding:
arg <- list(at=seq(1,5,1),labels=c("forest","none","defo","water","none"))

# set the colouring of the landcover classes, colors need to correspond to class order:
color <- c("green","white","brown","blue","white")

# plot the classification with predefined colors and legend names:
plot(kmeansraster,col=color,axis.arg=arg)

# exercise:
# redo analysis with more/fewer classes
# explore other suitable R functions (e.g. PAM, mean shift)
# add/remove layers for the classification (e.g. add a DEM, slope, texture)

##########################################################################################################################

## ggplot2_grammar ##
install.packages("ggplot2")
library(ggplot2)

# get list of available geometric objects:
help.search("geom_",package="ggplot2")

x11() # opens an extra graphic window
x <- data.frame(x=1,y=1,label="ggplot2 introduction \n@ EAGLE")
ggplot(data=x,aes(x==x,y==y))+geom_text(aes(label=label),size=15)

# we use steigerwald EAGLE dataset
install.packages("devtools")
library(devtools)

#geht nicht????????????????????????????????????????????????????????
install_bitbucket("EAGLE_MSc/steigerwald",build_vignettes=TRUE)

library(steigerwald)
data("bio_data") # was passiert?
bio_data
# check out the list, names with names() or on the git with actual column details:
head(bio_data)
#???????????????????????????????also anderer versuch daten zu laden:

steigerwald <- load("bio_data.rda")
steigerwald
bio_data
names(bio_data)
names(bio_data)[1] <- "forest"
bio_data$forest

# simple dot plot, beech basal area vs. ndvi:
ggplot(bio_data$forest,aes(x=beech,y=ndvi))+geom_point()
ggplot(bio_data$forest,aes(beech,ndvi,colour=height))+geom_point()+geom_smooth() # adding information
ggplot(bio_data$forest,aes(beech,ndvi))+geom_point()+facet_wrap(~sub_basin) # faceting

# you can also add several geom attributes:
# boxplot with point "jitter"
ggplot(bio_data$forest,aes(sub_basin,ndvi))+geom_boxplot(alpha=.5)+geom_point(aes(color=height),alpha=.7,size=1.5,position=position_jitter(width=.25,height=0))
ggplot()+geom_point(data=bio_data$forest,aes(sub_basin,ndvi)) # same info as here but with many points less informative
ggplot()+geom_point(data=bio_data$forest,aes(sub_basin,ndvi,colour=height)) # same but with colour
#set width and height to 0 and you see all points are in one line (on top of each other)

ggplot(bio_data$forest,aes(x=beech,y=ndvi))+geom_jitter()
ggplot(bio_data$forest,aes(x=beech,y=ndvi))+geom_boxplot()
ggplot(bio_data$forest,aes(x=beech,y=ndvi))+geom_violin()+geom_jitter(aes(alpha=.7,size=2),colour="blue")

# you can also "store" your plot:
a <- ggplot()+geom_point(data=mpg,aes(x=displ,y=hwy,colour=class))
# and then call your "stored" plot and add new options:
a+theme_bw() # e.g. a new color scheme

# for all further plots:
theme_set(theme_bw())
a # global settings
a+theme_grey() # back to the default, for this one only
a # back to global settings
theme_set(theme_grey()) # and back to default globally

# you can modify the attributes of all themes, to whatever you like:
# have a look at the contents of some themes:
theme_grey()
theme_bw()
theme_get() # the current theme
theme_update()

# you can easily change the theme:
a <- ggplot()+geom_point(data=mpg,aes(x=displ,y=hwy,colour=class))
a+theme_bw() # for one plot
# you can set the theme globally:
theme_set(theme_bw())
# theme_light(), theme_dark(), theme_minimal()...
# http://ggplot2.tidyverse.org/reference/ggtheme.html

# define your theme:
ggplot()+geom_point(data=mpg,aes(x=displ,y=hwy,colour=class))+facet_grid(manufacturer~class)+ggtitle("EAGLE chart")+theme(plot.title=element_text(angle=0,size=22,colour="hotpink"))+scale_colour_discrete(name="type")
# alternative: guides(colour=guide_legend(title="type))

#######################################################################################################################################################################################################################

# task: plot data of google spreadsheet # session 6, 6_12_17
install.packages("RCurl")
library(RCurl) # activate needed package

# get the data:
task <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbXxJqjfY-voU-9UWgWsLW09z4dzWsv9c549qxvVYxYkwbZ9RhGE4wnEY89j4jzR_dZNeiWECW9LyW/pub?gid=0&single=true&output=csv")
x <- read.csv(textConnection(task))

# inspect the data:
x
summary(x)

install.packages("reshape2")
library(reshape2)
# reformat data to suit ggplot needs if names should be on x-axis & time on y-axis: look at data to see changes
x2 <- melt(data=x) 
str(x2)
ggplot(x2,aes(x=variable,y=value))+geom_boxplot() # plot data as boxplot

# compute cummulative sum & add it with the names to a new data frame
x.cs <- data.frame(variable=names(x) ,cs=t(cumsum(x)[nrow(x),])) #cumsum is in last row & we index that
names(x.cs) <- c("variable","cumsum") # change names to fit melt output & to be able to merge it later on
x2 <- melt(data=x) # reshaping data: look at data to see the differences

# merge two dataframes based on "variable" column name (your names)
x3 <- merge(x.cs,x2,by.x="variable",all=T)
ggplot(x3,aes(x=variable,y=value,color=cumsum))+geom_point() # plot the sum as colour
# plot point plus boxplot & add a jitter (again: important to inform if jitter is used):
ggplot(x3,aes(x=variable,y=value,color=cumsum))+geom_boxplot(alpha=.5)+geom_point(alpha=.7,size=1.5,position=position_jitter(width=.25,height=.5))

# add even more information:
install.packages("gender") # load library that aims to detect gender (trained mainly on North America)
library(gender)
x.g <- gender(names(x)) # run gender detection on the names
colnames(x.g)[1] <- "variable" # change column name of names to "variable" again for later merging
x4 <- merge(x3,x.g,by.x="variable",all=T) # merge it with previously created data
# plot time per person divided by gender, plot stored in "a":
a <- ggplot(x4,aes(x=variable,y=value,color=cumsum))+geom_boxplot()+facet_wrap(~gender)
a # call plot
print(x.g) # probabilities of gender

# plot is not perfect, e.g. names can hardly be read:
a+coord_flip()
a+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

#task: a boxplotw/ and w/o different jitter values - & grouped by gender
#task: plot probabilities of gender
#task: change settings of gender detection, e.g. years or  countries
