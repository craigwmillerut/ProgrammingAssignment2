## Put comments here that give an overall description of what your
## functions 
# makeCacheMatrix takes a matrix as a variable and returns 4 functions
# set - function to create a copy of the matrix and a dummy variable for the inverse
# get - returns the matrix
# setInverse - caches the matrix inverse 
# getInverse - if the inverse has been calculated, returns the cached answer

# This would be much simpler if R was an object oriented language.
# We could have created a matInverse class with methods to set and retrieve values.
# However, it's good to know how to do it this way!

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	#set the internal value x to be the matrix
	#create the variable for cached matrix
	set <- function(y) {
		#x will contain a copy of the matrix 
		x <<- y
		#m is intended for the cached inverse but begins as a null
		m <<- NULL  
	}
	#get returns x
  set(x)
	get <- function() x
	setInverse <- function(matrixInverse) m <<- matrixInverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)	
}


## Write a short comment describing this function
#The first time through the inverse will be computed and cached.
#All following calls will return the cached version

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
        ## Return a matrix that is the inverse of 'x'
	m
}

#Code to demonstrate
matdata <- matrix(c(4,-8,2,-2,-1,1,-3,1,2),nrow=3,ncol=3)
example <- makeCacheMatrix(matdata)
cacheSolve(example)
#prints out the inverse
cacheSolve(example)
#prints "getting cached data"
#then prints inverse
