## Below are two functions that are used to create a wrapper object
## that stores a matrix and cache's its inverse (assume only using
## square invertible matrices) 


## The function creates a wrapper for a "matrix" object, which is 
## really a list containing a function to:
## - set the value of  the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
    iM <- NULL
    set <- function(y){
        x <<- y
        iM <<- NULL
    }
    get <- function() x
    setinv <- function(m) iM <<- m
    getinv <- function() iM
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function calculates the inverse matrix of the wrapper "matrix" 
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the 
## cache via the solve function.

cacheSolve <- function(x){
    iM <- x$getinv()
    if(!is.null(iM)){
        return(iM)
    }
    data <- x$get()
    ## we only need to calculate the inverse, so the ... parameters
    ## were excluded to avoid confusion
    iM <- solve(data)
    x$setinv(iM)
    iM
}
