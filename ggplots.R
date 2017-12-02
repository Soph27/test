###########################################
## ggplot2_grammar ########################
###########################################
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
names(bio_data)
names(bio_data)[1] <- "forest"

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

# task: plot data of google spreadsheet

#homework: own skript for ggplot
##ggplot2 and spatial data next week##
