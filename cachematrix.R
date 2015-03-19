## Assignment 2: Caching the Inverse of a Matrix

## (1) This first function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
        
    set <- function(y) {        ## Function to set the original matrix and set the inverse
        x <<- y                 ## matrix to NULL in another environment, using <<-.
        i <<- NULL
    }
    
    get <- function() x         ## Function to get the original matrix.
    
    setinverse <- function(inverse) i <<- inverse       ## Function to set the inverse of the matrix.
    
    getinverse <- function() i      ## Function to get the inverse of the matrix.
    
    list(set = set, get = get,      ## List of the four functions (and environments)used to set and get 
         setinverse = setinverse,   ## the original matrix and the inverse matrix.
         getinverse = getinverse)
}

## (2) This second function computes the inverse of the matrix returned by makeCacheMatrix.  
## If the inverse has already been calculated and is unchanged, the inverse matrix will be 
## retreived from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'. We can use the values of x and i above since
        ## they were set to be used in another environment.

    i <- x$getinverse()     ## If the inverse has already been computed, get
    if (!is.null(i)) {      ## the cached inverse.
            message("Getting cached inverse")
            return(i)
    }
    data <- x$get()         ## If the inverse has not yet been computed,
    i <- solve(data, ...)   ## compute the inverse of the matrix and return it.
    x$setinverse(i)
    i
}
