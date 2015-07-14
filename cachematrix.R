## Matrix inverse computation is a costly operation, these functions/objects will
## allow us to compute matrix inverse as well as store them in cache, so that 
## the next time we need a matrix inverse we can simply get it from cache
## instead of recomputation

## input an invertible matrix and return
## a complex object which contains functions for storing
## its value and inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse  <- function(inverse) inv <<- inverse 
    getInverse  <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
