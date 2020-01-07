## The following functions initialize a matrix (makeCacheMatrix) and then
## calculates it's inverse, which is stored. When the function cacheSolve is 
## call, returns the cache matrix if available, calculate otherwise.

## The function makeCacheMatrix initialize a matrix and the "setters" and "getters"

makeCacheMatrix <- function(x = matrix()) {
    
    nr <- nrow(x)
    nc <- ncol(x)
    
    inv <- NULL
    
    set <- function(y) {
        
        nr <- nrow(y)
        nc <- ncol(y)
        
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inversa) inv <<- inversa
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function returns the inverse of the matrix "x". If it was previously calculated,
## return the cache inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}








