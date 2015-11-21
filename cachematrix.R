## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is a function that contains a number of other functions.
## These allow you to set or get a matrix and also to calculate its inverse (solve) 
## and assign it to a variable that can be used outside of the function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This cacheSolve function will return the inverse of the a matrix. 
## If the inverse has already been calculated for the given matrix and cached, 
## the cached value will be used, if not it will be calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

