## This function is able to cache the matrix 
## that is the inverse of 'x' so that when we need it again, 
## it can be looked up in the cache rather than recomputed.

## This function creates a list containing a function to
## set the value of the inverse of 'x' in the cache and
## get the value of the inverse of 'x' from the cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function checks to see if the inverse of 'x' has 
## already been calculated and gets it from the cache, or
## calculates it and sets in the cache

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}