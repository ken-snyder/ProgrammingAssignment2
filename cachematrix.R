## This file contains 2 R functions, makeCacheMatrix and CacheSole.
##
## makeCacheMatrix will create a matrix is it doesn't exist and cache its inverse.
##
## cacheSolve will retrieve the inverse of the matrix if it exists and if it hasn't
## changed.  Otherwise it will solve and return the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolution <- function(solve) m <<- solve
    getSolution <- function() m
    list(set = set, 
         get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolution()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setSolution(m)
    m
}
