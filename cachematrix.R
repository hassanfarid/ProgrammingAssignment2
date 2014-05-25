## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates special matrix that has
## 1) get/set functions for matrix value
## 2) getSolve function to to provide cached inverse or NULL
## 3) setSolve function to cache inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solved) s <<- solved
	getSolve <- function() s
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}


## Write a short comment describing this function
## this function calculates inverse of a square matrix;
## assuming matrix for which inverse is possible
## takes cached inverse is calculated before
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getSolve()
	if (!is.null(s))
	{
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
}
