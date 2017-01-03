## Find the inverse of an invertible square matrix
## but first check if we already have this inverse stored in cache
## so we can retrieve that and avoid the matix inversion calculations

## This function populates the in-memory cache functions for a matrix , returning them as a vector

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## This function would be called instead of a direct call to solve
## If the matrix inverse is already in cache, return that, else go ahead and call solve
## This function takes the makeCacheMatrix vector for the matrix as its input

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
