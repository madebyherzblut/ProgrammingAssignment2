# This is my solution to the Programming Assignment 2 for the R Programming
# course.

## This function creates a self-cacheable matrix to easily store the inversion
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function checks if the matrix was inversed previously and return the
# result from cache if possbile.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()

    # Check if there is already a cached inversion. If so, simply return it
    if (! is.null(m)) {
      message("Returning matrix from cache.")
      return(m)
    }

    message("Inverting matrix and persisting the result.")

    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# Test Case:
#> test <- makeCacheMatrix(matrix(c(-3, 2, 0, 2, -1, 0, 1, -2, 1), 3, 3))
#> cacheSolve(test)
#> cacheSolve(test)
