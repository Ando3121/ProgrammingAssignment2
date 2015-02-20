## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This function will
# 1. set the value of the Matrix
# 2. get the value of the Matrix
# 3.set the value of the Inverse
# 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matinv <<- inverse
        getinverse <- function() matinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       
}



## Write a short comment describing this function

## The following function calculates the inverse of the special "Matrix" 
## created with the above function. However, it first checks to see if the 
## Inverse has already been calculated. If so, it gets the Inverse 
## from the cache and skips the computation. Otherwise, it calculates the Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matinv <- x$getinverse()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(matinv, ...)
        matinv
}