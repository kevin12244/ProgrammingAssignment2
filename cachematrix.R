## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is a function that creates a special matrix, where the value of the matrix and its inverse 
##can be stored. It is equipped with functions to update and set the value of the the matrix and its inverse. 
##cacheSolve checks to see, whether the special matrix already has an inverse cached. If so, it returns the value.
##Otherwise, it sets the value of the special matrix's inverse to the inverse calculated by the solve function and
##return that inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
