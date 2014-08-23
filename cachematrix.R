
## This file has two R functions makeCacheMatrix() and CacheSolve() 
## that creates a matrix Object (to store the inverse of a matrix)
## and return the inverse of the matrix respectively


## #####################################################
## makeCacheMatrix() funciton does the following :
## 1. Creates a matrix object to cache the inverse
## 2. Returns a list of functions that do the following :
##      set() - sets value of the original matrix
##      get() - return the vlaue of the original matrix
##      getInverse() - returns the cached Inverse of the matrix
##      setInverse() - stores the Inverse of the matrix 
## ###################################################

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

	set <- function(y) {

    ## Set the value of the original matrix
                x <<- y

		## Initailize inverse to NULL
                m <<- NULL
        }

        get <- function() x

        setInverse <- function(mean) m <<- mean

        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Cachesolve() function takes a matrix as an argument
## and returns the Inverse of the given matrix.
## It returns the saved inverse if it was previously calculated.
## Otherwise it calculates inverse using solve() function.
## For the purpose of this assignment it is assumed that the 
## Matrix supplied is invertible

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

	  ## Get the cached value of Inverse
	  m <- x$getInverse()

	  ## Return chached Inverse if it is not  
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        
        ## Computing Inverse of the Matrix
        m <- solve(data)

	# Cache Inverseof the matrix for future
        x$setInverse(m)
	
	# Return the Inverse of x
        m
}


