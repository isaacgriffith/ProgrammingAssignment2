## Functions to create a matrix which stores its inverse for later recall

## Function which creates the cache matrix object

makeCacheMatrix <- function(x = matrix()) {

    inverse <- null
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function which returns the inverse of the matrix or if not already present caches the inverse as well.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("retrieving cached inverse")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(x)
    x$setInverse(inverse)
    inverse
}
