## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix() creates and initualizes a "class object" "cachematrix" which contains
# the original matrix, a cache for its inverse matrix, and a set of functions to manipulate
# the original and inverse matrices.
# cacheSolve() returns the inverse of the matrix of "class" cachematrix.
# It stores the computed inverse in cache. If cacheSolve is called again for the same
# source matrix, the inverse matrix is retrieved from cache.

## Write a short comment describing this function
# Description: makeCacheMatrix creates and initualizes a "class object" "cachematrix". 
# A cachematrix object has:
# Functions set and get = Assign and get the (source) matrix data.
# Variable matrix.cache = Matrix cache for holding an already calculated (inverse) matrix.
# Functions setinverse and getinverse = Assign and get the (inverse) matrix stored in matrix.cache.
# Arguments: matrix

makeCacheMatrix <- function(x = matrix()) {
    matrix.cache <- NULL
    # Set original matrix data and invalidate cached inverse
    set <- function(y) {
        x <<- y
        matrix.cache <<- NULL
    }
    # Get original matrix data
    get <- function() {
        x
    }
    # Store inverse matrix data in cache
    setinverse <- function(s) {
        matrix.cache <<- s
    }
    # Get inverse matrix data from cache
    getinverse <- function() {
        matrix.cache
    }
    # Return the cachematrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# Description: cacheSolve returns the inverse of the matrix of "class" cachematrix.
# Arguments: x = cachematrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # Retrieve  a possibly previously calculated matrix inverse from the cache
    inverted.matrix <- x$getinverse()
    # If NULL, cache was empty and inverse must be calculated first.
    # Otherwise, return the cached inverse matrix.
    if(!is.null(inverted.matrix)) {
        message("getting cached data")
        # Return the inverse matrix (from cache)
        inverted.matrix
    }
    else {
        message("calculating inverse and storing it in cache")
        # Get original matrix values
        original.matrix <- x$get()
        # Solve to obtain inverse matrix
        inverted.matrix <- solve(original.matrix , ...)
        # Store inverse matrix into object's cache
        x$setinverse(inverted.matrix)
        # Return the inverse matrix (calculated)
        inverted.matrix
    }
}
