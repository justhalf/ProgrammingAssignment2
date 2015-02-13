## This file describes a function to create matrix capable of caching its
## inverse (makeCacheMatrix) and another function to calculate the inverse,
## avoiding calculation as much as possible by utilizing the cache (cacheSolve)

## The function makeCacheMatrix accepts a matrix as an input, then returning
## a CacheMatrix object represented by a list consisting of four members,
## all of which are functions.
## Below is the description of each function:
## - set: set the matrix content of this CacheMatrix object
## - get: get the matrix content of this CacheMatrix object
## - setinv: save the inverse of this matrix
## - getinv: returns the inverse of this matrix, NULL if not set before.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
                x <<- y
                inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The function cacheSolve accepts a CacheMatrix object (a list, actually) and
## calculate its inverse. If the CacheMatrix object already cached the inverse,
## no calculation is performed as the cached inverse is simply returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
