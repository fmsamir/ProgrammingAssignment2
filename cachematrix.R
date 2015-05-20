## The following functions work together using the lexical scoping property of
## R to create a cache which stores matrix inverses, which can be used to
## bypass repeated costly computations.

## This function creates a special matrix object which caches its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## This function sets the value of the matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## This function gets the value of the matrix.
    get <- function() x
    
    ## This function sets the value of the inverse.
    setinverse <- function(inverse) inv <<- inverse
    
    ## This function gets the value of the inverse.
    getinverse <- function() inv
    
    ## A list is created which contains the above functions.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the
## makeCacheMatrix function. If the inverse exists in the cache, it is
## retrieved from the cache instead.

cacheSolve <- function(x, ...) {
    
    ## The following code attempts to find a cached inverse. If it exists,
    ## a message is displayed and the inverse is returned, ending the
    ## function's execution.
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    }
    
    ## The following code computes the matrix's inverse and stores it in
    ## the cache.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    ## The computed inverse is displayed.
    inv
}