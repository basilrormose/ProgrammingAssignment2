## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a environment (equivalent to a object
## that is used to cache or save state), in this instance the inverse
## of a matrix.
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function accepts a CacheMatrix object that contains a
## matrix, determines if it's inverse has already been calculated and
## if so returns the cached value (the object state if you will) otherwise
##  solves for the inverse, caches the value for future use, and returns
## the calculated inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieving cached inverse")
        return(inv)
    }
    data <- x$get()
    x$setinverse(solve(data, ...))
    x$getinverse()
}
