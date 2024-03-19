## These two functions first create an object storing a matrix and cached inverse
##If available, the cached inverse is then retrieved and returned, or the inverse is computed

## This function makes a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invrse <- NULL
    #sets input argument to x in the parent environment
    #sets invrse object back to NULL instead of retrieving a previous value
    set <- function(y) {
        x <<- y
        invrse <<- NULL
    }
    #retrives value of x from parent environment
    get <- function() x
    
    #mutate value of invrse in parent environment
    setinverse <- function(solvex) invrse <<- solvex
    #retrieve value of invrse
    getinverse <- function() invrse 
    
    #names functions and returns functions to parent environment
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    
    
}


## Takes the output of makeCacheMatrix function and retrieves cached inverse if available, if not, computes inverse

cacheSolve <- function(x, ...) {
    #takes the output of makeCacheMatrix function
    #retrives value of invrse set by makeCacheMatrix function
    invrse <- x$getinverse()
    
    #if there is an existing inverse matrix, retrieves value instead of procesing again
    if(!is.null(invrse)) {
        message("getting cached data")
        return(invrse)
    }
    #if not, calculates inverse of matrix 
    data <- x$get()
    
    invrse <- solve(data, ...)
    #sets the value of invrse to calculated value and returns inverse matrix
    x$setinverse(invrse)
    return(invrse)
    ## Return a matrix that is the inverse of 'x'
}
