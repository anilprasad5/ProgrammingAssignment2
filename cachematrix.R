## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function prepares a special matrix, accepting a parameter matrix.

makeCacheMatrix<-function()
{
#------------------------------------Required Variables
o.mat<<-NULL 
mat.inverse<<-NULL 					# cache inverse of matrix
mat<<-NULL 							#cache matrix
#------------------------------------
set<-function(y)					#function to set matrix object
{
mat<<-y
}
#____________________________________
o.set<-function(z)o.mat<<-z

o.get<-function() o.mat

#-----------------------------------#return value of matrix
get<-function() mat	
#-----------------------------------#set value of inverse
setinverse<-function(m) mat.inverse<<-m
#-----------------------------------#get value of inverse matrix
getinverse<-function() mat.inverse
#-----------------------------------#list of function
list(o.set=o.set,set=set,get=get,setinverse=setinverse,getinverse=getinverse,o.get=o.get)
#------------------------------------
}

cacheSolve<-function(x,m11=matrix(),......)
{

s<-NULL
mat1<-x$get()	# calling the value of matrix
x$o.set(mat1)	# retainig the value of matrix
x$set(m11)		# set value of new matrix 	
o<-x$o.get()	# get retained value of matrix

ginverse<-x$getinverse()# get value of inverse of matrix
mat1<-x$get()			# get value of matrix
g1<-!is.null(ginverse)	# check value of inverse whether NULL 
g2<-(all(mat1==o))		# compare value of new and old matrix


if(g1 && g2)
{
print("Matrix Inverse Exist and Matrix are same")
return(ginverse)
}
else
{
print("New Matrix. Calculating Inverse")
mat1<-x$get()
s<-solve(mat1)	#Calculation of Inverse Of Matrix 
}

#print(s)
x$setinverse(s)

}

