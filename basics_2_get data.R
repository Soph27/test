#######getting data into R#######
getwd()#prints currenet working directory
setwd("C:\\Users\\Sophie\\Documents\\sophie\\Master Würzburg\\WS17-18\\MB2-Programming and Geostatistics\\7_11_17")#sets current working directory
my.df <- read.table("bio_data_forest.csv",header=T)#T=TRUE defines that first row consists of names not values
#other separators sep for csv are "/" , "," , ";" , for tabs as separator "\t"
my.df#prints the whole data
head(my.df)#just prints the first 6 rows
summary(my.df)#provides summary statistics
#plot(my.df)#first plot of the data
write.table(my.df,file="my_data_frame.txt")#define data frame to export and name exxported file

#######Indexing-adressing certain parts of data#######
X <- matrix(c(4,7,3,8,9,2),nrow=2)
X
X[,2]#query 2nd column
X [1,]#query first row
X[2,3]
X[1,1]

#create a data frame or matrix
numbers_1 <- rnorm(80,mean=0,sd=1)#vector of 80 entries with normal distribution with determined mean and standard deviation
mat_1 <- matrix(numbers_1,nrow=20,ncol=4)#create matrix with 20 rows, 4 columns
mat_1

#convert it to a data frame
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1","var2","var3","var4")#name column names with name()

#quick look at data
head(df_1)
summary(df_1)

df_1[1,1]
df_1$var1[1:3]#zeige aus spalte var1 die zahlen 1 bis 3
df_1[3:4,]#zeige Reihen 3 und 4
df_1[2:3,3:4]#zeige werte in Reihen 2,3 und Spalten 3,4

#generate a vector
A <- seq(1,100,by=2.5)#create sequence from 1 to 100 by steps of 2.5
A
A[5]#query 5th position of sequence
A[4:10]#extract 4th to 10th position

#extract last value of sequence
length(A)#length of A (ist 40)
A[length(A)]#length of A and query this position
A[length(A)-1]#length of A and query this minus one position

#extract all but one position
A[-2]#extract all but second position

#extract / omit a list of positions
ida <- c(1,4,6)#create a vector with three numbers
A[ida]#query A based on ida numbers
A[-ida]#omit A query of these three numbers

#query values of data
A>20#above 20
(A<=10) | (A>=30)#below or equal 10 OR above or equal 30

A[A<10 | A>30]#provide all data below 10 or above 30

#change values
A2 <- numeric(length(A))#create numeric vector with specified length
A2[A<=30] <- 1
A2[(A>30) & (A<70)] <- 2
A2[A>70] <- 3
A2

#alternative approach
install.packages("car")
library(car)#recode commander nutzen (und auch noch andere commands)
A2 <- recode(A, "0:30=1; 30:70=2; else=3")
A2

b <- c(1,2,3,4,5,6,7,8,9,10)
b2 <- recode(b,"1:5=1;5:10=2")#5 wurde als 1 recoded, nicht als 2
b2

#some stats inbetween
summary(A)#general summary stats
sum(A)#general sum
cumsum(A)#cummulative sum

#some data modification inbetween
rev(A)#revert the order
sort(A,decreasing=TRUE)#doing the same
sort(A,decreasing=F)#increasing order again
sample(A,10)#sample 10 values out of A

#generate data frame with 2 columns
test <- data.frame(B=c(1,2,3),C=c("D1","D2","D3"))
test#show dataframe
test[,1]#query just "B"
test[,"B"]#query just "B"
test$B#query just "B"

#create a data frame with characters and numbers#?????????????????????
df <- data.frame(plot="location_name_1",measure1=runif(100)*1000,measure2=round(runif(100)*100),value=rnorm(100,2,1),ID=rep(LETTERS,100))
df
df_2 <- data.frame(plot="location_name_2",measure1=runif(50)*100,measure2=round(runif(50)*10),value=rnorm(50),ID=rep(LETTERS,50))
df_2
df <- rbind(df,df_2)
df
length(df[,1])

summary(df)#summary stats
str(df)#display of structure
mode(df)#storage mode of object
head(df)#return first (or last) part of an object

#plot the whole dataframe but just for plot, measure1 and measure2
df[,c("plot","measure1","measure2")]
x <- df#save df into new object x which contains ddf_1 and df_2

plot(df$measure1,df$measure2)
plot(df$value,df$measure1)
install.packages("rgl")
library(rgl)
plot3d(df$value,df$measure1,df$measure2)

#plot data frame just for line 566 to 570 for plot, measure1 and measure2
df[566:570,c("plot","measure1","measure2")]

#df[df$value>3.0,].........??????

###more indexing, 14/11/17###now with column values of a data frame
df[df$value>3.0,]
df[df$value>3.2 | df$measure1>50,]#above 3.2 OR above 50
df[df$value>3.2 & df$measure1>50,]#above 3.2 AND above 50
df$new_col <- df$measure1*df$measure2#add a new column based on product of two others

#query your data using a keyword e.g. `a`(quite simple) for the ID column
df[grep("a",df$ID,ignore.case=T),]#grep() looks for matches to the argument
#what happens if ignore.case=F #angezeigt wird <0 Zeilen> (oder row.names mit Länge 0)??????????

#create a random list of "yes" and "no"
x1 <- rbinom(10,size=1,prob=0.5)
x2 <- factor(x1,labels=c("yes","no"))# factor()encodes a vector as a factor
summary(x2)
levels(x2)
as.character(x2)#conversion to actual characters

#recode it
library(car)
recode(x2,"`yes`=`sure`;`no`=`maybe`")
#or
ifelse(x2=="no","maybe","sure")#if statement is true use "maybe", if not use "sure"
#similar to change classes in a raster classification

###application###
#plot the data "prec" but just July
prec_avg <-c(56,46,50,53,69,83,83,80,62,55,60,63)
plot(prec_avg[7])

#plot the data "prec" from April to September
plot(prec_avg[4:9])

#substract the January from the February precipitation
prec_avg[2]-prec_avg[1]

#sum of precipitation
sum(prec_avg)

#cummulative sum of precipitation
cumsum(prec_avg)

#maximum precipitation
max(prec_avg)

#range of values
range(prec_avg)

#which is the minimum value
which.min(prec_avg)

#which is closest to value x
which.min(abs(prec_avg-50))#Antwort ist 3, also März
which.min(prec_avg-50)#Antwort ist 2, also Februar

#difference between elements
diff(prec_avg)

###application RS###
#raster data
#raster_data[[3]]#adress the third layer
#raster_data@data$...#point to the data in layer x
#raster_data[]#provides the underlying data (matrix)

#vector data
#vector_data@data#point to the data of a spatial vector data set
#vector_data@data$column_name#point to a specific column

###homework###
#import spreadsheet
getwd()
setwd("C:\\Users\\Sophie\\Documents\\test")
test_sheet <- read.csv("test_spreadsheet1.csv", header=T,sep="")
test_sheet#wieso ; als trennungszeichen???
md <- read.table("test_spreadsheet2.txt", header=T, sep=",")
md
#preliminary data checks
#cut()
z <- c(1,2,3,4,5,6,7,8,9,10)
z
cut(z,5)
cut(z,breaks=-2:1)
#quantile()
quantile(z,probs=c(0.25))#25-prozent quantil berechnen
median(z)
#sort()
sort(z,decreasing=T)
#order()
order(z)

###14/11/17###
c <- rnorm(10)#generating 10 random numbers
#newly created numbers are now ordered to the defined area -2:1
#the new area has three groups
table(cut(c,breaks=-2:1))#table macht tabelle aus cut...
#now you can see how often a number of the data c
#appears in which group
cut(c,breaks=-2:1)
c

###matrix###
#generate a 2x3 matrix
m1 <- matrix(c(4,7,3,8,9,2),nrow=2)
m1
m1[,2]
m1[2,]
m1[2,2]

#matrix with defined rows and columns and how to fill it
m2 <- matrix(c(2,4,3,1,5,7), nrow=2,ncol=3,byrow=TRUE)#c ist data elements, byrow fills matrix by rows
m2

###raster data in r###
#str @data you work with dataframes, for raster work with matrix
install.packages("raster")
library(raster)
r1 <- raster(nrows=10, ncols=10)#create empty raster with 10x10 rows and columns
r1#look at data
plot(r1)#plot the empty data
r1[] <- rnorm(100)#100 weil wir 100 cells haben; fill empty raster with 100 random values
r1#look at data again
plot(r1)#plot raster
#raster_data[[3]]#adress the third layer
#raster_data@data$...#point to the data in layer x
#raster_data[]#provides the underlying data (matrix)

#create a vector(spatial points)
library(sp)
poi1 <- cbind(c(rnorm(10)),c(rnorm(10)))#create 10 random coordinate pairs
poi1#look at output
poi1.sp <- SpatialPoints(poi1)#convert list of coordinates to a spatial object
plot(poi1.sp)#empty dataset; plot spatial point dataset
df <- data.frame(attr1=c("a","b","z","d","e","q","w","r","z","y"),attr2=c(101:110))#creating values
poi1.spdf <- SpatialPointsDataFrame(poi1.sp,df)#adding values to spatial point data set
plot(poi1.spdf)#plot spatial points data set
#vector_data@data#point to the data of a spatial vector data set
poi1.spdf@data
#vector_data@data$column_name#point to a specific column
poi1.spdf@data$attr1

#[[]] brackets for indexing raster objects (or list or moveStacks)
install.packages("RStoolbox")
library(RStoolbox)
lsat
lsat[]#provides the underlying data (matrix)
plot(lsat[[1]])#plot first band
plot(lsat$B1_dn)#plot first band
lsat[[2:3]]#query second and third band
plot(lsat[[2:3]])#plot second and third band
lsat[[1]]#query band 1
x <- lsat[[1]]#save first band in a new object
x <- lsat[[2:3]]#save second and third band in a new object
##raster@raster$B1_dn#point to data in layer 1??????????????????????????
##lsat@data$B1_dn#wieso geht das nicht??????????????
lsat@data#point to the data of a raster data set

##get data##????????????????????????????
#get values
install.packages("move")
library(move)
data(lsat)#load/create example data; from RStoolbox and move package
data(leroy)
env <- raster(leroy,vals=rnorm(100))#create a raster with the properties (extent and projection of the vector)
x <- lsat[1:10,]#values of rows one to ten#???????wieso zeigt es nicht nur rows 1 to 10 an?
x <- lsat[]#all values
x <- getValues(lsat)#all values
x <- lsat[lsat$B1_dn==10]#based on logical query; eigentlich muss statt einfachem = doppelt ==, aber das geht nicht!!!!
x <- lsat[lsat$B1_dn>10]#based on logical query
x <- extract(env,leroy)#extract raster values based on vector geometry, e.g. move objects or polygons
x
plot(x)

#set values
lsat[] <- rnorm(ncell(lsat))#populate (=bestücken) all bands with normally distributed data,ncells=number of entries
lsat[lsat<0] <- NA#set all values below 0 to NA
env[] <- 0#all values in env set to 0
env[leroy] <- 1#and leroy areas set to one

###21.11.17###

#create data and recode it
library(car)
b <- c(1,2,3,4,5,6,7,8,9,10)
b2 <- recode(b,"1:5=1;5:10=2")

#create data frame and convert it to raster / create a raster from scratch
#create several raster with random values in a stack
library(raster)
r1 <- raster(nrows=10, ncols=10)#create empty raster with 10x10 rows and columns
r1#look at data
plot(r1)#plot the empty data
r1[] <- rnorm(100)#100 weil wir 100 cells haben; fill empty raster with 100 random values
r1#look at data again
plot(r1)


