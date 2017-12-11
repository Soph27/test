###session 7, 12.12.17###################################
###supervised classification#############################

library(rgdal)
library(RStoolbox)

install.packages("e1071")
library(e1071)

#########################################################
td <- rgdal::readOGR("C:\\Users\\Sophie\\Documents\\test","superClass_trainingdata")
sc <- superClass(allbands,trainData=td,responseCol="id")

plot(sc$map)
sc
summary(sc)

plotRGB(allbands,3,2,1,stretch="lin")
