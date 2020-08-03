##  This will cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## set the value of the vector
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get the value of the vector
        setsolve <- function(solve) m <<- solve ##set the value of the inverse
        getsolve <- function() m ## get the value of the inverse
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
       m <- x$getsolve
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       data <- x$get
       m <- solve(data, ...)
       x$setsolve(m)
       m
}

