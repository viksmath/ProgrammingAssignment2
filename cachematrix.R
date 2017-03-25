## Caching the Inverse of a Matrix 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a ## matrix rather than compute it 
## repeatedly.  Here are a pair of functions that cache the inverse of a matrix.

## To see the results, create a matrix and pass it to makeCacheMatrix function. 
## Then pass the output from above to the cacheSolve function.  It will return 
## the inverse of the matrix.  Running the cacheSolve function again will
## display "Getting the cached data", as the results are returned from cache.       

## The following function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) Inv <<- solve
        get_inverse <- function() Inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The following function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve function should retrieve the 
## inverse from the cache.
## Assumption is that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        Inv <- x$get_inverse()
        if(!is.null(Inv)) {
                message("Getting the cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$set_inverse(Inv)
        Inv
}
