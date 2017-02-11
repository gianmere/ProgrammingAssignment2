## This file contains two functions that permit to calculate the inverse
## of a matrix and to cache its value, so it could be obtained without 
## calculate it again

## This function creates a special object which contains a matrix and could contains
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function takes as parameter a special matrix and calculates its inverse 
## or return its saved value (if the value was calculated previous)

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
