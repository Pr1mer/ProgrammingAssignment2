## Matrix inversion is usually a costly computation and when using repeatedly
## there is a benefit to caching its inverse rather than compute it each time.
## The following two functions (makeCacheMatrix & cacheSolve) implement
## the functionality of matrix inversion with result caching.


## This function creates a special matrix object that can cache its inverse.
## After creating special matrix object with this function you can pass
## it as an argument to cacheSolve function for matrix inversion with
## cache functionality.

makeCacheMatrix <- function(x = matrix()) {
    
    # This variable will store inverted matrix value if available.
    s <- NULL
    
    # Function for setting new value of a matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Function for retreiving the value of a matrix
    get <- function() x
    
    # Function for caching the result of matrix inversion
    setsolve <- function(solve) s <<- solve
    
    # Function for retreiving the cached value of inverted matrix.
    getsolve <- function() s
    
    # Return the list of 4 functions which is the implementation
    # of special matrix object that can cache its inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special matrix object 
## returned by makeCacheMatrix function (above).
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Check for cached value and return it if exits
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # Restore the matrix content and invert it
    data <- x$get()
    s <- solve(data, ...)
    
    # Cache the result and return it
    x$setsolve(s)
    s
}
