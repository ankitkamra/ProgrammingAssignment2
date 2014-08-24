## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##My First Function
## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Creates a list of functions that
    ## can cache the inverse of a matrix.
    m <- NULL # sets the value of m to NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m     # get the value of the inverse and caches it within the setinverse function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##My Second Function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    # Need to compare matrix to what was there before!
    ## Computes the inverse of the matrix returned by makeCacheMatrix(), unless the inverse has already been calculated, in which case it retrieves it from the cache
    m <- x$getInverse()
    if ( ! is.null(m)) {# check to see if cacheSolve has been run before
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
