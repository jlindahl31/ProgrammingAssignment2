## makeCacheMatrix creates an object that holds both the matrix and its
## inverse.  CacheSolve takes a makeCacheMatrix object and returns the inverse
## either by using the cached inverse or solving, caching, and returning
## the inverse.

## makeCacheMatrix takes a matrix and returns a makeCacheMatrix object
## with getters and setters for the matrix and its inverse
## setting the matrix will also clear the inverse so that it has
## to be recalculated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ## get matrix
    get <- function() x
    
    ## set inverse and also return it
    setinverse <- function(invtoset) {
        inv <<- invtoset
        inv
    }
    
    ## get inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function will check to see if the makeCacheMatrix object already knows
## its inverse. If it doesn't it will calculate it and store it in the
## makeCacheMatrix object.  Either way it returns the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    if(is.null(inv)){
        inv <- x$setinv(solve(x$get()))
    }
    inv
}
