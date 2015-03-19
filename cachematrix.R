## Assignment 2: Caching the Inverse of a Matrix

## This first function creates a matrix object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
        
        ## Function to set the original matrix and set the inverse
        ## matrix to NULL in another environment, using <<-.
    set <- function(y) {
        x <<- y          
        i <<- NULL
    }
    
        ## Function to get the original matrix.
    get <- function() x
    
        ## Function to set the inverse of the matrix.
    setinverse <- function(inverse) i <<- inverse
    
        ## Function to get the inverse of the matrix.
    getinverse <- function() i
    
        ## List of the four functions used to set and get the
        ## original matrix and the inverse matrix.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This second function computes the inverse of the
## matrix returned by makeCacheMatrix.  If the inverse
## has already been calculated and is unchanged, the
## inverse matrix will be retreived from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'.
        ## We can use the values of x and i above since
        ## they were set to be used in another environment.

        ## First check if the inverse has been computed.
        ## If the inverse has already been computed, get
        ## the cached inverse.
    i <- x$getinverse()
    if (!is.null(i)) {
            message("Getting cached inverse")
            return(i)
    }
    
        ## If the inverse has not yet been computed,
        ## compute the inverse of the matrix and return it.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
