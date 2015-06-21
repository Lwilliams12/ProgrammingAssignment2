## makeCacheMatrix.R consists of two main components:
## A. The first part: "makeCacheMatrix" creates a list
##    of 4 functions that implement a "caching" feature
##      1. set the value of the matrix: (x)
##      2. get the value of the matrix: (x)
##      3. set the value of the inverse of the matrix: (x_inv)
##      4. get the value of the inverse of the matrix: (x_inv)
##    The "caching" feature stores x in an environment 
##         different from the current environment. This allows
##         reuse of the "cached" inverse when x has not changed.
##         Using the "cached" inverse saves time by not
##         recalculating the same inverse when x has not changed.
##
## B. The second part: "cacheSolve" returns the inverted matrix.
##    This implementation assumes x has an inverse. 
##    (i.e. no need to check for singular, or nearly singular matrices.)

## makeCacheMatrix creates a list of 4 functions 
##  which implement caching of x and x_inv as needed.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(xinv) x_inv <<- xinv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        xmatrix <- x$get()
        x_inv <- solve(xmatrix)
        x$setinv(x_inv)
        x_inv
}

