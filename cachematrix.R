## Functions to cache potentially time-consuming matrix inversion computation

## makeCacheMatrix creates a special "matrix" which returns a list cotatining function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
	  set <- function(y) {
		x <<- y
		m <<- NULL
	  }
	  get <- function() x
	  setInverse <- function(inverse) m <<- inverse
	  getInverse <- function() m
	  list(set=set, get=get,
		   setInverse=setInverse,
		   getInverse=getInverse)
}


## cacheSolve inverse the special "matrix" created by the makeCacheMatrix function
## if the inverse matrix has already been calculated, then the function gets the value from the cache and skips the computation
## Otherwise it inverse the matrix and set the value in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()
	  if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	  }
	  data <- x$get()
	  inv <- solve(data,...)
	  x$setInverse(inv)
	  inv
}
