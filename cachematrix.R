## Creates a mechanism for calculating the inverse of a matrix once, caching the
## result, and retrieving the cached result upon subsequent requests.
## Algorithim from makeVector example in Programming Assignment 2: Lexical
## Scoping, of the Coursera R Programming class taught by R. D. Peng, et. al.

## Given a matrix x, makeCacheMatrix creates a list of four functions used
## internally by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(inv) inverse <<- inv
   getinverse <- function() inverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x$get from makeCacheMatrix.
## If cacheSolve is called the first time with x, it will calculate the inverse
## and cache the result.  If cacheSolve is called again with the same value of
## x, (and makeCacheMatrix$set has not been called between the two calls), it
## will return the cached result, which is faster.
cacheSolve <- function(x, ...) {
   inverse <- x$getinverse()
   if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
   }
   data <- x$get()
   inverse <- solve(data, ...)
   x$setinverse(inverse)
   inverse
}
