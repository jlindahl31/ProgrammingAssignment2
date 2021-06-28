## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    if(is.null(inv)){
        inv <- x$setinv(solve(x$get()))
    }
    inv
}
