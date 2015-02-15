## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function prepares a special matrix, accepting a parameter matrix.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
	
	 #set(x)
	 #get()
	 #setinverse(x)
	# getinverse()
	#--------------------------------,
	set <- function(y)
		{
		x<<-y
		m<<-NULL
		}
#--------------------------------
	get<-function() x
#--------------------------------
setinverse <- function(y)
	{
	m <<-solve(y)
	}
#---------------------------------
	getinverse <- function() m
#--------------------------------

	 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

m<<-getinverse()
	if((!is.null(m)))
		{
		message("getting cached data")
		return(m)
		}
	data<-get()
	m<-solve(data, ...)
	setinverse(m)

        ## Return a matrix that is the inverse of 'x'
}
