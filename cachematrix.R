## Functions below compute the inverse of a matrix.
## If the inverse of a matrix was done previously (and the matrix
## did not change), the result is retrieved from cache.
## If not, the inverse of the matrix is computed.

## makeCacheMatrix create a list with the following functions:
## set: set the matrix to be inverted.
## get: get the matrix to be inverted.
## setinverse: set the inverse matrix (if necessary).
## getinverse: get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix, if it is not cached previously.
## If so, get from cache the inverse of the matrix set in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}