## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'm' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setImatrix() to assign computed inverse matrix (of x) to m;
## 4. Provides function getImatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}