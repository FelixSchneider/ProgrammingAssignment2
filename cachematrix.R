## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                 # initialize local variable inv
    set <- function(y) {                        # is 'set' ever used?
        x <<- y
        inv <<- NULL
    }
    get <- function() x                         # 'get' fetches the argument
    setinv <- function(solve) inv <<- solve     # 'setinv' sets the inverse
    getinv <- function() inv                    # 'getinv' retrieves the inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    # retrieve the contents of the cache from the cache
    inv <- x$getinv()
    # if the cached value is not NULL, i.e. an inverse was cached before,
    # leave the function and return the cached value together with an
    # informational message.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()             # otherwise get the matrix
    inv <- solve(data, ...)     # calculate the inverse
    x$setinv(inv)               # and put the result into the cache
    inv                         # finally return the calculated inverse
}
