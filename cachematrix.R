## These functions cache the inverse of a matrix.
## To work do not pass the matrix itself to the cacheSolve function,
## but pass the return object of the makeCacheMatrix
## e.g. -> cacheSolve(makeCacheMatrix(x))
## or m <- makeCacheMatrix(x)
## cacheSolve(m)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # Define function to set the value of the matrix. It also clears the old
        # inverse from the cache
        set <- function(y) {
                x <<- y         #Set the value
                m <<- NULL      #Clear the cache
        }
        # Define function to get the value of the matrix
        get <- function() x
        # Define function to set the inverse. This is only used by getinverse() when
        # there is no cached inverse
        setinverse <- function(inverse) m <<- inverse
        # Define function to get the inverse
        getinverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()     # This fetches the cached value for the inverse
        if(!is.null(m)) {       # If the cache was not empty, we can just return it
                message("getting cached data")
                return(m)
        }
        
        # The cache was empty. We need to calculate it, cache it, and then return it.
        data <- x$get()         # Get value of matrix
        m <- solve(data, ...)   # Calculate inverse
        x$setinverse(m)         # Cache the result
        return(m)
}

