## Functions below compute the inverse of a matrix.
## If the inverse of a matrix was done previously (and the matrix
## did not change), the result is retrieved from cache.
## If not, the inverse of the matrix is computed.

## Instructions:
## 1. Assign makeCacheMatrix(your_own_matrix) to a varible.
##    "your_own_matrix" should be a matrix object.
## 2. If you want to check the matrix to be inversed, use: variable$get()
## 3. If you want to check the inversed matrix, use: variable$getinverse()
## 4. If you want to compute the inverse of a matrix (or get it from cache), use: cacheSolve(variable)
## 5. If you want to set a matrix to be inversed, use: variable$set(your_own_matrix)
## 6. If you want to set the inversed matrix, use: variable$setinverse(your_inversed_matrix)

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