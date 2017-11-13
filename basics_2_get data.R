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
numbers_1 <- rnorm(80,mean=0,sd=1)#normal distribution with determined mean and standard deviation
mat_1 <- matrix(numbers_1,nrow=20,ncol=4)#create matrix with 20 rows, 4 columns
mat_1

#convert it to a data frame
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1","var2","var3","var4")#name column names with name()

#quick look at data
head(df_1)

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
#?df <- data.frame(plot="location_name_1",measure1=runif(100)*1000,measure2=round(runif(100)*100),value=rnorm(100,2,1),ID=rep(LETTERS,100))
df
#?df_2 <- data.frame(plot="location_name_2",measure1=runif(50)*100,measure2=round(runif(50)*10),value=rnorm(50),ID=rep(LETTERS,50))
df_2
#?df <- rbind(df,df_2)
df

summary(df)#summary stats
str(df)#display of structure
mode(df)#storage mode of object
head(df)#return first (or last) part of an object

#plot the whole dataframe but just for plot, measure1 and measure2
df[,c("plot","measure1","measure2")]

#plot data frame just for line 566 to 570 for plot, measure1 and measure2
df[566:570,c("plot","measure1","measure2")]

#df[df$value>3.0,].........??????


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
which.min(abs(prec_avg-50))#?????

#difference between elements
diff(prec_avg)

###application RS###
#raster_data[[3]]#adress the third layer
#raster_data@data$...#point to the data in layer x
#vector_data@data#point to the data of a vector

###homework###
getwd()
setwd("C:\\Users\\Sophie\\Documents\\test")
#test_sheet <- read.csv("test_spreadsheet1.csv", header=T)
#import spreadsheet
#preliminary data checks
#cut()
#quantile
#sort()
#order()

