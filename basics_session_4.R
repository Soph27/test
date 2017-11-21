###21.11.17###
#build your own functions to redo them again
#check your data automatically
#if statement (if something is true, do something)
a <- sqrt(2)
#if a multiplied with a is unequal to 2 then print something
if(a*a != 2)#!= means unequal to
{
print("R is great!")
}

b <- sqrt(25)
if(b*b!=25)
{
print("geht nicht")
}
#spatial analysis: if e.g. resolution is not x, then resample

#while statement
j <- 0
while(j<1)
  {
j <- j+0.1;print(j)
}

#creating own function
#add commonly used analysis to a function
#myfunction <- function(arg1,arg2, ...){
  #statements
  #return(objects)
  #}

myownfunction <- function(x,y){
  z <- x+y
  return(z)
  }

#or
myownfunction <- function(x,y){
  x+y
}

myownfunction(4,3)

#importance for RS, e.g. for NDVI
fun_ndvi <- function(nir,red) {(nir-red)/(nir+red)}
