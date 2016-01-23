## These functions provide an implementation of a matrix
## object that caches calls to get the inverse when
## appropriate to avoid costly re-calculations.

## This function creates a special matrix object
## that has the ability to store a cached version
## of its inverse and use this if it's available.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Variable storing the cached inverse
    set <- function(y) {
        x <<- y
        # Reset/invalidate the cached inverse 
        # as we're storing an entirely new matrix here.
        inv <<- NULL
    }
    get <- function() { x }
    setinv <- function(val) { inv <<- val }
    getinv <- function() { inv }
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## This function calculates the inverse of the given
## CacheMatrix using a cached version if it already
## exists, otherwise calling solve(x) to calculate
## it and storing it in the cache for later use.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("using cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
